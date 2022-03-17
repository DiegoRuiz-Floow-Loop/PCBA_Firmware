/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/cli/cli.h"

#ifdef CLI_ENABLE

/********************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "hal/hal.h"
#include "hal/dio/dio.h"
#include "hal/mcupin/mcupin.h"
#include "hal/wdt/wdt.h"

#include "hal/hal.h"
#include "plf/plf.h"
#include "plf/cli/cli.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"


/******************************** LOCAL DEFINES *******************************/

static EvosEventHandle_t cliTimer;
#define START_CHAR_TIMEOUT() EvosEventSetDelta(cliTimer, 300, 0)
#define STOP_CHAR_TIMEOUT()  EvosEventClear(cliTimer)

#define CHAR_TAB     '\t'
#define CHAR_BACKSP  '\b'
#define CHAR_CTRLB   '\002'
#define CHAR_CTRLC   '\003'
#define CHAR_CTRLD   '\004'
#define CHAR_CTRLE   '\005'
#define CHAR_CTRLL   (12)
#define CHAR_CTRLP   '\020'
#define CHAR_CTRLR   '\022'
#define CHAR_CTRLT   '\024'
#define CHAR_SPACE   ' '
#define CHAR_BELL    '\007'
#define CHAR_ESC     0x1B
#define CHAR_TRCSW      CHAR_CTRLT
#define CHAR_TRCPUR     CHAR_CTRLP
#define CHAR_TRCLOG     CHAR_CTRLL
#define CHAR_SIFBAN     CHAR_CTRLB
#define CHAR_ARROWUP    0xfb
#define CHAR_ARROWDOWN  0xfc
#define CHAR_ARROWRIGHT 0xfd
#define CHAR_ARROWLEFT  0xfe
#define CHAR_TIMEOUT    0xff

#define STATE_COMMAND    0
#define STATE_PARAMETER  1
#define STATE_ECHO       2

#define STATE_VT100_IDLE    0
#define STATE_VT100_ESC     1
#define STATE_VT100_BRACKET 2

const char  cliNewLine[] = CLI_NL;


/********************************* LOCAL DATA *********************************/
typedef struct {
  CliTestParam_t  type;
  const char  *enumString;
} CliEnumList_t;

typedef enum {
  PROMPT_EMPTY,
  PROMPT_COMMAND,
  PROMPT_PARAM
} CliPrompt_t;

typedef enum {
  PARAM_UP,
  PARAM_DOWN
} CliBrowseParameter_t;

typedef struct {
  const CliEntry_t   *prompt[CLI_CFG_LEVEL];
  int                   prompt_level;
  const CliEntry_t         *pTest;
  char                  cmd[CLI_CFG_LINE_LENGTH];
} CliHistory_t;

#define TRC_CLI_PARAM_DOMAIN  \
  TRC_AREA_TEXT_0 " 0\0" \
  TRC_AREA_TEXT_1 " 1\0" \
  TRC_AREA_TEXT_2 " 2\0" \
  TRC_AREA_TEXT_3 " 3\0" \
  TRC_AREA_TEXT_4 " 4\0" \
  TRC_AREA_TEXT_5 " 5\0" \
  TRC_AREA_TEXT_6 " 6\0" \
  TRC_AREA_TEXT_7 " 7\0" \
  "all 0xff\0"

#define TRC_CLI_PARAM_TRACEMASK  \
  TRC_LEVEL_TEXT_0 " 0x01\0" \
  TRC_LEVEL_TEXT_1 " 0x02\0" \
  TRC_LEVEL_TEXT_2 " 0x04\0" \
  TRC_LEVEL_TEXT_3 " 0x08\0" \
  TRC_LEVEL_TEXT_4 " 0x10\0" \
  TRC_LEVEL_TEXT_5 " 0x20\0" \
  TRC_LEVEL_TEXT_6 " 0x40\0" \
  TRC_LEVEL_TEXT_7 " 0x80\0" \
  "all 0xff\0"
  
/* Enumerated parameter list.
   Add your new enumerates here */
static  const CliEnumList_t eList[] = {
  {CLI_PARAM_ONOFF, (const char  *)"off 0\0on 1\0"},
  {CLI_PARAM_NOYES, (const char  *)"no 0\0yes 1\0"},
  {CLI_PARAM_DISABLEENABLE, (const char  *)"disable 0\0enable 1\0"},
  {CLI_PARAM_LAN_SPEED, (const char  *)"10 0\0100 1\0auto 2\0"},

#ifdef TRC_ENABLE
  {CLI_PARAM_TRACEMASK, (const char  *)TRC_CLI_PARAM_TRACEMASK},
//  {CLI_PARAM_TRACEMODE, TRC_CLI_PARAM_TRACEMODE},
  {CLI_PARAM_DOMAIN, (const char  *)TRC_CLI_PARAM_DOMAIN},
#endif /* (TRC_ENABLE) */

  {CLI_PARAM_NONE, 0}
};

static const char  *  argHelp[CLI_PARAM_Last] = {
  (const char  *)"",
  (const char  *)"UI64",
  (const char  *)"UINT",
  (const char  *)"INT",
  (const char  *)"CHAR",
  (const char  *)"STRING",
  (const char  *)"STRING",
  (const char  *)"ONOFF",
  (const char  *)"NOYES",
  (const char  *)"DISABLEENABLE",
  (const char  *)"TRACEMASK",
  (const char  *)"TRACEMODE",
  (const char  *)"DOMAIN",
  (const char  *)"MAC",
  (const char  *)"IP",
  (const char  *)"SPEED"
};

static const char  helpTxt[] =
  CLI_NL
  "CLI (test/debug command line interface)"  CLI_NL
  "Use keys:" CLI_NL
  " Tab              (Quick command/parameter)" CLI_NL
  " Arrow left/right (Command/parameter browse)" CLI_NL
  " Arrow up/down    (Command history)" CLI_NL
  " Ctl-b            (Toggle Banner on/off)" CLI_NL
  " Ctl-c            (Clear command)" CLI_NL
  " Ctl-d            (Toggle trace display)" CLI_NL
  " Ctl-e            (Echo mode)" CLI_NL
  " Ctl-l            (Toggle Log on/off)" CLI_NL
  " Ctl-p            (Purge trace log)" CLI_NL
  " Ctl-r            (Help info (this))" CLI_NL
  " Ctl-t            (Toggle Trace on/off)" CLI_NL
  " Esc              (Erase keywords)" CLI_NL
  " ..               (One menu back)" CLI_NL
  " .                (Main menu)" CLI_NL
  " ?                (Help to command/parameter)" CLI_NL
  " !                (List of all test commands)" CLI_NL
  CLI_NL;

static int16_t test_level;


//CLI_DECLARE_SUBTEST(MainMenu)

bool cliShowHeader = true;

static const CliEntry_t    *cliCommands[CLI_CFG_LEVEL]; // = { MainMenu_TestTable };
static const CliEntry_t    *prompt_tables[CLI_CFG_LEVEL];
static const CliEntry_t    *pushedTest;
static CliEntry_t    *pushESPrompt[CLI_CFG_LEVEL];

static bool           historyCmd=false;
static bool           cmd_update=true,
                        echo=true;
static int16_t          pushedLevel;
static CliSend_t        outputPrint = NULL;

static char             cmd[CLI_CFG_LINE_LENGTH],
                        param[CLI_MAX_PARAM][CLI_MAX_PARAM_LENGTH],
                        tab[CLI_CFG_LINE_LENGTH],
                        buf[CLI_CFG_LINE_LENGTH*3]; /*lint -e578 */
static uint16_t         cmd_idx,
                        tab_idx,
                        hist_idx,
                        param_idx,
                        param_level;
static CliParam_t       paramVal[CLI_MAX_PARAM];
static CliHistory_t     history[CLI_CFG_HIST_SIZE];


/******************************* LOCAL FUNCTIONS ******************************/
static char *get_prompt(CliPrompt_t type);
static int16_t getKeywords(void);
static void clearKeywords(int16_t cnt);
static void clearLine(void);
static int16_t findCommand(const CliEntry_t  **pTest, char *cmd, bool cr);
static int16_t executeCmd(const CliEntry_t  **pTest, char const *cmd, CliParam_t const pVal[CLI_MAX_PARAM], int16_t cnt);
static void getValues(CliTestParam_t type, char * bp);
static int16_t CliBrowseParameter(CliTestParam_t type, int16_t idx, CliBrowseParameter_t event, char *cp);
static int16_t parseParameter(char *cp, CliTestParam_t type, uint32_t *param, bool cr);
static int16_t execInput(char ch);
static int16_t popTest(const CliEntry_t  **pCmd);

static const char  * strchr_P(const char  * p, char c)
{
  do {
    if (*p == c)
    {
      return p;
    }
  } while (*p++);
  return 0;
}

//static char *strlfe(char *s, int16_t maxLen)
//{
//  int16_t nCnt;
//  char *p;

//  for (nCnt=0, p=s; *p; p++)
//  {
//    if (*p=='\n')
//    {
//      nCnt++;
//    }
//  }

//  if (((uint16_t)strlen((char *)s)+nCnt) > maxLen)
//  {
//    return 0;
//  }

//  for(p=s+strlen((char *)s); nCnt!=0; p--)
//  {
//    *(p+nCnt) = *p;
//    if (*p == '\n')
//    {
//      nCnt--;
//      *(p+nCnt) = '\r';
//    }
//  }
//  return s;
//}


//static int16_t CliWriteChar(char ch)
//{
//  return CliPrint((const char  *)"%c", ch);
//}


static void CliPrintCmdHelp(const CliEntry_t  *pTest)
{
  const CliEntry_t  *tOrg;
  static const CliEntry_t  *tStack[CLI_CFG_LEVEL];
  int i;
  static int depth=0;

  depth++;
  if (depth <= CLI_CFG_LEVEL) {
    HalWdtFeed();
    tOrg = pTest;
    while (pTest->xname[0] > 0)
    {
      if (pTest->func)
      {
        for (i=1; i<depth; i++)
        {
          CliWriteChar(CHAR_SPACE);
        }
        CliWrite((const char  *)" ");
        CliWrite(pTest->xname);
        CliWrite((const char  *)" (");
        CliWrite(pTest->xhelp);
        CliWrite((const char  *)") [");
        for (i=0; i<CLI_MAX_PARAM && pTest->param[i]!=CLI_PARAM_NONE; i++)
        {
          if (i > 0)
          {
            CliWrite((const char  *)", ");
          }
          CliWrite(argHelp[pTest->param[i]]);
        }
        CliWrite((const char  *)"]" CLI_NL);
      }
      pTest++;
    }
    pTest = tOrg;
    while (pTest->xname[0] > 0) {
      HalWdtFeed();
      if (pTest->table)
      {
        tStack[depth-1] = pTest;
        for (i=1; i<depth; i++) CliWriteChar(CHAR_SPACE);
        {
          CliWrite((const char  *)"Valid commands in '");
        }
        for (i=0; i<depth-1; i++)
        {
          CliWrite((const char  *)"/");
          CliWrite(tStack[i]->xname);
        }
        CliWrite((const char  *)"/");
        CliWrite(pTest->xname);
        CliWrite((const char  *)"/' (");
        CliWrite(pTest->xhelp);
        CliWriteLn((const char  *)")");
        CliPrintCmdHelp(pTest->table);
      }
      pTest++;
    }
  }
  depth--;
  if (depth == 1)
  {
    CliWrite(CLI_NL);
  }
}


static void printHelp(const CliEntry_t  *pTest)
{
  int i;
  char sz[80];

  if (pTest)  {
    sprintf(sz, (const char  *)"?" CLI_NL "Valid commands in '%s'", get_prompt(PROMPT_EMPTY));
    CliWriteLn(sz);

    while (pTest->xname[0] > 0) {
      HalWdtFeed();
      if (pTest->func) {
        CliWrite((const char  *)"  ");
        CliWrite(pTest->xname);
        CliWrite((const char  *) " (");
        CliWrite(pTest->xhelp);
        CliWrite((const char  *) ") [");
        for (i=0; i<CLI_MAX_PARAM && pTest->param[i]!=CLI_PARAM_NONE; i++)
        {
          if (i > 0)
          {
            CliWrite((const char  *)", ");
          }
          CliWrite(argHelp[pTest->param[i]]);
        }
        CliWriteLn((const char  *)"]");
      }
      else
      {
        CliWrite((const char  *)"> ");
        CliWrite(pTest->xname);
        CliWrite((const char  *)" (");
        CliWrite(pTest->xhelp);
        CliWriteLn((const char  *)")");
      }
      pTest++;
    }
  } else {
    CliWriteLn((const char  *)"!" CLI_NL "Valid commands in main");
    CliPrintCmdHelp(cliCommands[0]);
  }

  if (test_level > 0) {
    CliWriteLn((const char  *)"  .. (Go one menu back)");
    CliWriteLn((const char  *)"  .  (Go to main menu)");
  }
  CliWrite(get_prompt(PROMPT_PARAM));
}


static void printParamHelp(const CliEntry_t  *pTest, int16_t argCnt)
{
  char sz[80];
  char buffer[80];

  getValues(pTest->param[argCnt], buffer);

  sprintf(sz, (const char  *)CLI_NL "Valid parameter %d values: ", argCnt+1);
  CliWrite(sz);
  CliWriteLn(buffer);
  CliWrite(get_prompt(PROMPT_PARAM));
}


static char *get_prompt(CliPrompt_t type)
{
  static char buf[CLI_CFG_LINE_LENGTH];

  strcpy((char *)buf, "$ /");
  for (int i=0; i<test_level; i++)
  {
    strcat((char *)buf, (char  *)prompt_tables[i]->xname);
    strcat((char *)buf, "/");
  }

  if (type >= PROMPT_COMMAND)
  {
    cmd[cmd_idx] = 0;
    strcat((char *)buf, (char *)cmd);
  }

  if (cmd_idx && (type >= PROMPT_PARAM))
  {
    if (param_idx)
    {
      param[param_level][param_idx] = 0;
      for (int i=0; i<(param_level+1); i++)
      {
        strcat((char *)buf, " ");
        strcat((char *)buf, (char *)param[i]);
      }
    }
    else
    {
      for (int i=0; i<param_level; i++)
      {
        strcat((char *)buf, " ");
        strcat((char *)buf, (char *)param[i]);
      }
      strcat((char *)buf, " ");
    }
  }

  return buf;
}


static int16_t getKeywords(void)
{
  int16_t cnt;

  cnt = cmd_idx ? 1 : 0;
  cnt += param_level;
  cnt += param_idx ? 1 : 0;

  return cnt;
}


static void clearKeywords(int16_t cnt)
{
  if (cnt == 0)
  {
    param_idx = 0;
    param_level = 0;
    cmd_idx = 0;
  }
  else
  {
    if (/* cnt && */ param_idx)
    {
      param_idx = 0;
      cnt--;
    }
    while (cnt && param_level)
    {
      param_level--;
      cnt--;
    }
    if (cnt)
      cmd_idx = 0;
  }
  tab_idx = 0;
  tab[0] = '\0';
}


static void clearLine(void)
{
  int i;

  if (cmd_update == true)
  {
    for (i=(int)strlen((char *)get_prompt(PROMPT_PARAM)); i; i--)
    {
      CliWrite((const char  *)"\b \b");
    }
  }
  else
  {
    CliWrite(CLI_NL);
  }
}


static int16_t findCommand(const CliEntry_t  **pTest, char *command, bool cr)
{
  int16_t len, cc = 0;
  const CliEntry_t  *pT = *pTest;

  if (cmd_idx == 0)
  {
    cc = -1;
  }
  else
  {
    command[cmd_idx]='\0';
    if ( strcmp((char *)command, "..") == 0 )
    {
      if (test_level)
        test_level--;
      *pTest = cliCommands[test_level];
      cc = 1;
    }
    else if ( strcmp((char *)command, ".") == 0 )
    {
      test_level = 0;
      *pTest = cliCommands[test_level];
      cc = 1;
    }
    else
    {
      while (pT->xname[0] > 0)
      {
        len = (int16_t)strlen((char  *)pT->xname);
        if ( strncmp((char *)command, (char  *)pT->xname, strlen((char  *)pT->xname)) == 0 &&
          (command[len] == '\0' || isspace(command[len])) )
        {
          break;
        }
        pT++;
      }
      if (pT->xname[0] > 0)
      {
        *pTest = pT;
      }
      else
      {
        cc = -1;
      }
    }
  }

  switch (cc) {
  case -1:
    CliWriteChar(CHAR_BELL);
    CliWriteLn((const char  *)CLI_NL "Unknown command");
    if (cr)
    {
      clearKeywords(0);
    }
    CliWrite(get_prompt(PROMPT_COMMAND));
    break;
  case 0:
    if (!cr)
    {
      CliWriteChar(CHAR_SPACE);
    }
    tab_idx = 0;
    tab[0] = 0;
    break;
  case 1:
    CliWrite(CLI_NL);
    clearKeywords(0);
    CliWrite(get_prompt(PROMPT_COMMAND));
    break;
  default:
    /* do nothing */
    break;
  }

  return cc;
}


static int16_t executeCmd(
  const CliEntry_t  **pTest,
  char const *command,
  CliParam_t const pVal[CLI_MAX_PARAM],
  int16_t cnt)
{
  int16_t i, cc=0;
  bool runTest=false;
  char cliTxt[5];
  int16_t ccTest=CLI_RESULT_OK;
  char sz[80];

  CliWrite(CLI_NL);

  /* Test for correct amount of parameters */
  for (i=0; (i<CLI_MAX_PARAM) && ((*pTest)->param[i]!=CLI_PARAM_NONE); i++) { /* do nothing */ };
  if (i != cnt)
  {
    cc = -1;
  }
  else
  {
    if ((*pTest)->func)
    {
      //int16_t cc;
      ccTest = (*(*pTest)->func)(pVal[0], pVal[1], pVal[2]);
      if (ccTest >= CLI_RESULT_OK)
      {
        if (ccTest == CLI_RESULT_OK)
          CliWriteLn("+OK");
      }
      else
      {
        sprintf(sz, "-ERROR %d", ccTest);
        CliWriteLn(sz);
      }
      runTest = true;
    }
    else
    {
      if ( test_level < CLI_CFG_LEVEL - 1)
      {
        prompt_tables[test_level] = *pTest;
        test_level++;
        cliCommands[test_level] = (*pTest)->table;
        *pTest = cliCommands[test_level];
      }
      else
      {
        TRACE_ERROR(TRC_TA_PLF, "Too deep command nesting");
      }
    }
  }

  if (cc == -1)
  {
    CliWriteChar(CHAR_BELL);
    strcpy((char*)cliTxt,  i>cnt ? "few" : "many");
    sprintf(sz, (const char  *)"To %s parameters, got %d expects %d", cliTxt, cnt, i);
    CliWriteLn(sz);
    CliWrite(get_prompt(PROMPT_PARAM));
  }
  else
  {
    static int16_t nextHistIdx=0;

    if ((runTest==true) && cmd_idx)
    {
      memcpy(history[nextHistIdx].prompt, prompt_tables, sizeof(prompt_tables));
      history[nextHistIdx].prompt_level = test_level;
      if (historyCmd == false)
      {
        history[nextHistIdx].pTest = cliCommands[test_level];
      }
      else
      {
        history[nextHistIdx].pTest = history[hist_idx].pTest;
      }
      strcpy((char *)history[nextHistIdx].cmd, (char *)command);
      for(int16_t idx=0; idx<param_level; idx++)
      {
        strcat((char *)history[nextHistIdx].cmd, " ");
        strcat((char *)history[nextHistIdx].cmd, (char *)param[idx]);
      }
      nextHistIdx = (nextHistIdx + 1) % CLI_CFG_HIST_SIZE;
      hist_idx = nextHistIdx;
    }
    popTest(pTest);
    clearKeywords(0);
    if (ccTest != CLI_RESULT_EXECUTING)
      CliWrite(get_prompt(PROMPT_PARAM));
  }

  return cc;
}


static void getValues(CliTestParam_t type, char* bp)
{
  CliEnumList_t const  *ep;
  const char  *sp;

  *bp = '\0';

  switch (type)
  {
  case CLI_PARAM_STRING:
  case CLI_PARAM_STRING_LAST:
    strcpy((char *)bp, "A character string");
    break;
  case CLI_PARAM_P_UINT64:
    strcpy((char *)bp, "pointer, unsigned long long number");
    break;
  case CLI_PARAM_CHAR:
    strcpy((char *)bp, "A single printable character");
    break;
  case CLI_PARAM_INT32:
    strcpy((char *)bp, "Signed long number");
    break;
  case CLI_PARAM_UINT32:
    strcpy((char *)bp, "Unsigned long number");
    break;
  case CLI_PARAM_MAC:
    strcpy((char *)bp, "MAC address xx-xx-xx-xx-xx-xx");
    break;
  case CLI_PARAM_IP:
    strcpy((char *)bp, "IP address x.x.x.x");
    break;
  case CLI_PARAM_NONE:
    break;
  default:
    /* Search through all enum definitions */
    for (ep = eList; ep->type; ep++)
    {
      if (type == ep->type)
      {
        /* Search through all values for this enum definition */
        for (sp=ep->enumString; *sp; sp+=strlen((const char  *)sp)+1)
        {
          const char  *pv;
          pv = (const char  *)strchr_P((const char  *)sp, ' ');
          if (pv)
          {
            memcpy(bp, sp, (uint32_t)(pv-sp));
            bp[pv-sp] = '\0';
            if (sp[strlen((char *)sp)+1] != '\0')
            { // If not last value in enum
              strcat((char *)bp, ", ");
              bp += strlen((char *)bp);
            }
          }
          else
          {
            TRACE_ERROR(TRC_TA_PLF, "Invalid enum string");
          }
        }
        break;
      }
    }
    if (ep->type == CLI_PARAM_NONE)
    {
      TRACE_ERROR(TRC_TA_PLF,"Unknown command parameter");
    }
    break;
  }
}

/*lint -esym(613,sp) */
/*lint -esym(668,strlen) */

static int16_t CliBrowseParameter(
  CliTestParam_t type,
  int16_t idx,
  CliBrowseParameter_t event,
  char *cp)
{
  CliEnumList_t const  *ep;
  const char  *sp;
  const char  *pv;
  int16_t cc=0;

  switch(event) {
  case PARAM_UP:
    idx++;
    break;
  case PARAM_DOWN:
    idx = idx==0 ? 0 : idx-1;
    break;
  }

  switch (type) {
  case CLI_PARAM_STRING:
  case CLI_PARAM_STRING_LAST:
  case CLI_PARAM_P_UINT64:
  case CLI_PARAM_CHAR:
  case CLI_PARAM_INT32:
  case CLI_PARAM_UINT32:
  case CLI_PARAM_MAC:
  case CLI_PARAM_IP:
  case CLI_PARAM_NONE:
    cc = -1;
    break;
  default:
    /* Search through all enum definitions */
    for (ep = eList; ep->type; ep++)
    {
      if (type == ep->type)
      {
        /* Search through all values for this enum definition */
        for (sp=ep->enumString; *sp && idx; sp+=strlen((const char  *)sp)+1)
        {
          if ((char *)strstr((char *)sp, (char *)tab) == sp)
          {
            idx--;
            if (idx==0)
              break;
          }
        }
        if (*sp)
        {
          pv = (const char  *)strchr_P((const char  *)sp, ' ');
          if (pv)
          {
            memcpy(cp, sp, pv-sp);
            cp[pv-sp] = '\0';
          }
          else
          {
            TRACE_ERROR(TRC_TA_PLF, "Invalid enum string");
          }
        }
        else
        {
          cc = -1;
        }
        break;
      }
    }
    if (ep->type == CLI_PARAM_NONE)
    {
      TRACE_ERROR(TRC_TA_PLF, "Unknown command parameter");
    }
  }
  return cc;
}


static int16_t parseParameter(
  char * cp,
  CliTestParam_t type,
  uint32_t * parameter,
  bool cr)
{
  char * cp1;
  char validValues[CLI_CFG_LINE_LENGTH];
  CliEnumList_t const  *ep;
  const char  *sp;
  const char  *pv;
  int16_t cc=0;
  char sz[80];
  static uint64_t ui64[CLI_MAX_PARAM];

  if (param_idx == 0)
  {
    return 0;
  }

  cp[param_idx] = '\0';
  switch (type)  {
    case CLI_PARAM_STRING:
    case CLI_PARAM_STRING_LAST:
      *parameter = (uint32_t)cp;
      break;
  case CLI_PARAM_P_UINT64:
    *parameter = (uint32_t)&ui64[param_level];
    ui64[param_level] = strtoull((char *)cp, (char **)&cp1, 0);
    if (cp1 == cp) {
      strcpy((char *)validValues, "Unsigned long long number");
      cc = -1;
    }
    break;
  case CLI_PARAM_CHAR:
    if (strlen((char *)cp) == 1)
    {
      *parameter = (uint32_t) cp[0];
    }
    else
    {
      strcpy((char *)validValues, "A single printable character");
      cc = -1;
    }
    break;
  case CLI_PARAM_INT32:
    *parameter = (uint32_t) strtol((char *)cp, (char **)&cp1, 0);
    if (cp1 == cp)
    {
      strcpy((char *)validValues, "Signed long number");
      cc = -1;
    }
    break;
  case CLI_PARAM_UINT32:
    *parameter = (uint32_t) strtoul((char *)cp, (char **)&cp1, 0);
    if (cp1 == cp)
    {
      strcpy((char *)validValues, "Unsigned long number");
      cc = -1;
    }
    break;
  case CLI_PARAM_MAC:
    {
      static uint8_t mac[6];
      uint8_t i;
      for (i=0; i<6; i++)
      {
        mac[i] = (uint8_t)strtoul((char *)cp, (char **)&cp1, 16);
        if (cp == cp1)
        {
          cc = -1;
          break;
        }
        else
        {
          cp = cp1+1;
        }
      }
      if (cc == 0)
      {
        *parameter = (uint32_t)mac;
      }
    }
    break;
  case CLI_PARAM_IP:
    {
      uint8_t ip[4], i;

      for (i=0; i<4; i++)
      {
        ip[i] = (uint8_t)strtoul((char *)cp, (char **)&cp1, 10);
        if (cp == cp1)
        {
          cc = -1;
          break;
        }
        else
        {
          cp = cp1+1;
        }
      }
      if (cc == 0)
      {
        *parameter = ((uint32_t)ip[0]<<24) + ((uint32_t)ip[1]<<16) + ((uint32_t)ip[2]<<8) + ip[3];
      }
    }
    break;
  case CLI_PARAM_NONE:
    *parameter=0;
    cc = -2;
    break;
  default:
    /* Search through all enum definitions */
    for (ep = eList; ep->type; ep++)
    {
      if (type == ep->type)
      {
        /* Search through all values for this enum definition */
        for (sp=ep->enumString; *sp; sp+=strlen((const char  *)sp)+1)
        {
          pv = (const char  *)strchr_P((const char  *)sp, ' ');
          if (pv)
          {
            if (memcmp(cp, sp, pv-sp) == 0)
            {
              /* The value is found */
              while ( isspace(*pv) )
                pv++; /* skip leading spaces */
              *parameter = (uint32_t) strtol((char*)strcpy((char*)validValues, (const char  *) pv), (char **)&cp1, 0);
              if (cp1 == pv)
              {
                TRACE_ERROR(TRC_TA_PLF,"Invalid enum string");
              }
              break;
            }
          }
          else
          {
            TRACE_ERROR(TRC_TA_PLF,"Invalid enum string");
          }
        }
        if (*sp == 0)
        {
          getValues(type, validValues);
          cc = -1;
        }
        break;
      }
    }
    if (ep->type == CLI_PARAM_NONE)
    {
      // TRACEF_ERROR0(TRC_TA_PLF, "Unknown command parameter");
    }
  }

  switch (cc) {
  case -2:
    CliWriteChar(CHAR_BELL);
    CliWriteLn((const char *)CLI_NL "No more parameters are exepected");
    param_idx = 0;
    CliWrite(get_prompt(PROMPT_PARAM));
    break;
  case -1:
    CliWriteChar(CHAR_BELL);
    sprintf(sz, CLI_NL "Bad parameter value. Valid values are: %s", validValues);
    CliWriteLn(sz);
    param_idx = 0;
    CliWrite(get_prompt(PROMPT_PARAM));
    break;
  default:
    if (!cr)
      CliWriteChar(CHAR_SPACE);
//    if (type != CLI_PARAM_STRING_LAST) {
    param_level++;
    param_idx = 0;
    tab_idx = 0;
    tab[0] = '\0';
//    }
  }

  return cc;
}


static int16_t pushTest(const CliEntry_t  **pCmd)
{
  if (historyCmd == false)
  {
    pushedTest = *pCmd;
    memcpy(pushESPrompt, prompt_tables, sizeof(prompt_tables));
    pushedLevel = test_level;
    historyCmd = true;
  }

  memcpy(prompt_tables, history[hist_idx].prompt, sizeof(prompt_tables));
  test_level = history[hist_idx].prompt_level;

  return 0;
}


static int16_t popTest(const CliEntry_t  **pCmd)
{
  if (historyCmd == true)
  {
    *pCmd = pushedTest;
    memcpy(prompt_tables, pushESPrompt, sizeof(prompt_tables));
    test_level = pushedLevel;
    historyCmd = false;
  }
  return 0;
}

static void RefreshScreen(void)
{
//   char sz[80];
//  CliWriteLn(applicationString);
//  sprintf(sz, "Sw version %s Build %s", versionString, buildString);
//  CliWriteLn(sz);
//  sprintf(sz, "Hw version: %s", hwVersionString());
  //CliWriteLn(sz);
  CliWrite(helpTxt);
//  CliWrite(header2);
//  CliWrite(header3);
//  CliWrite(header4);
  CliWrite(get_prompt(PROMPT_PARAM));
}

static int16_t execInput(char ch)
{
  int           i, cc = 0;
//  char sz[80];
  static unsigned int    state = STATE_COMMAND;
  const CliEntry_t  *pTest;
  static const CliEntry_t  *pCmd;
  char name[40];

  if (historyCmd == false)
  {
    pTest = cliCommands[test_level];
  }
  else
  {
    pTest = history[hist_idx].pTest;
  }

  switch (state) {
  case STATE_COMMAND:
    switch ((unsigned int)ch) {
    case CHAR_CTRLE: /* Test echo */
      CliWriteLn((const char *)"Testing transmission line by echoing." CLI_NL "Press 'ESC' to escape test");
      state = STATE_ECHO;
      break;
    case '!':
      printHelp(0);
      break;
    case '?':
      printHelp(pTest);
      break;
//    case CHAR_TRCDISP:
//      cmdDisplay = !cmdDisplay; //==true ? false : true;
//#ifdef TRACE_ENABLE
//      TrcDisplaySet(cmdDisplay);
//#endif
//      break;
    case CHAR_SIFBAN:
      BannerToggle();
      break;
    case CHAR_TRCLOG:
      LogToggle();
      break;
    case CHAR_TRCSW:
#ifdef TRC_ENABLE
			TrcToggle();
#endif 
		break;
    case CHAR_TRCPUR:
#ifdef TRACE_ENABLE
      TrcPurge();
#endif
      break;
    case CHAR_ESC:   /* escape */
      clearLine();
      if (getKeywords() != 0)
      {
        /* Clear command */
        clearKeywords(1);
      }
      else
      {
            /* Step one menu back */
        if (test_level)
        {
          test_level--;
        }
      }
      CliWrite(get_prompt(PROMPT_PARAM));
      break;
    case CHAR_TAB:   /* tab */
      if (cmd_idx == 0)
      {
        break;
      }
      cmd[cmd_idx]='\0';
      strcpy((char *)tab, (char *)cmd);
      while (pTest->xname[0] > 0)
      {
        strcpy(name, (char  *)pTest->xname);
        if (strstr(name, (char *)cmd) == name)
        {
          break;
        }
        pTest++;
      }
      if (pTest->xname[0] > 0)
      {
        strcpy((char *)cmd, (char  *)pTest->xname);
        CliWrite(cmd+cmd_idx);
        cmd_idx = strlen((char  *)pTest->xname);
        tab_idx = 1;
      }
      break;
    case CHAR_BACKSP:/* back space - erase one char */
      if (cmd_idx)
      {
        cmd_idx--;
        CliWrite((const char *)"\b \b");
      }
      tab[0] = '\0';
      tab_idx = 0;
      break;
    case CHAR_CTRLR: /* refresh screen */
      RefreshScreen();
      break;
    case CHAR_CTRLC: /* clear cmd */
      clearLine();
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      break;
    case CLI_NL_CHAR:    /* command ended */
      if (cmd_idx)
      {
        if ((cc=findCommand(&pTest, cmd, true)) == 0)
        {
          tab_idx = 0;
          pCmd = pTest;
          state = STATE_PARAMETER;
          if ((cc=executeCmd(&pTest, cmd, paramVal, 0)) == 0)
          {
            state = STATE_COMMAND;
            cc = 1;
          }
        }
      }
      else
      {
        CliWrite(cliNewLine);
        CliWrite(get_prompt(PROMPT_EMPTY));
      }
      break;
    case CHAR_ARROWUP:
      if (hist_idx==0)
      {
        if (strlen((char *)history[CLI_CFG_HIST_SIZE-1].cmd) == 0)
        {
          break;
        }
        hist_idx = CLI_CFG_HIST_SIZE-1;
      }
      else
      {
        hist_idx--;
      }
      clearLine();
      pushTest(&pCmd);
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      for (i=0; i<(uint16_t)strlen((char *)history[hist_idx].cmd); i++)
      {
        execInput(history[hist_idx].cmd[i]);
      }
      break;
    case CHAR_ARROWDOWN:
      if (strlen((char *)history[hist_idx+1].cmd) == 0)
      {
        break;
      }
      hist_idx = (hist_idx + 1) % CLI_CFG_HIST_SIZE;
      clearLine();
      pushTest(&pCmd);
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      for (i=0; i<(uint16_t)strlen((char *)history[hist_idx].cmd); i++)
      {
        execInput(history[hist_idx].cmd[i]);
      }
      break;
    case CHAR_ARROWRIGHT:
      i = 0;
      while (pTest->xname[0] > 0)
      {
        strcpy(name, (char  *)pTest->xname);
        if (strstr(name, (char *)tab) == name)
        {
          i++;
        }
        if (i == (tab_idx+1))
        {
          break;
        }
        pTest++;
      }
      if (pTest->xname[0] > 0)
      {
        clearLine();
        strcpy((char *)cmd, (char  *)pTest->xname);
        cmd_idx = strlen((char  *)pTest->xname);
        tab_idx++;
        CliWrite(get_prompt(PROMPT_COMMAND));
      }
      break;
    case CHAR_ARROWLEFT:
      if (tab_idx <= 1)
      {
        break;
      }
      i = 0;
      while (pTest->xname[0] > 0)
      {
        strcpy(name, (char  *)pTest->xname);
        if (strstr(name, (char *)tab) == name)
        {
          i++;
        }
        if (i == (tab_idx-1))
        {
          break;
        }
        pTest++;
      }
      if (pTest->xname[0] > 0)
      {
        clearLine();
        strcpy((char *)cmd, (char  *)pTest->xname);
        cmd_idx = strlen((char  *)pTest->xname);
        tab_idx--;
        CliWrite(get_prompt(PROMPT_COMMAND));
      }
      break;
    case CHAR_SPACE:
      if ((cc=findCommand(&pTest, cmd, false)) == 0)
      {
        if (pTest->param[0] != CLI_PARAM_NONE)
        {
          tab_idx = 0;
          param_idx = 0;
          param_level = 0;
          pCmd = pTest;
          state = STATE_PARAMETER;
        }
        else
        {
          CliWriteChar(CHAR_BELL);
          CliWriteLn((const char *)CLI_NL "Parameters are not expected");
          CliWrite(get_prompt(PROMPT_PARAM));
        }
      }
      break;
    default:         /* Other printable character */
      if ( ch < 32 )
      {
        break; /* ignore unhandled control chars */
      }
      if ( cmd_idx < (sizeof(cmd) - 1))
      {
        cmd[cmd_idx++] = ch;
        if (echo == true)
        {
          CliWriteChar(ch);
        }
      }
      tab[0] = '\0';
      tab_idx = 0;
    }
    break;

  case STATE_PARAMETER:
    switch ((unsigned int)ch) {
    case '?':
      printParamHelp(pCmd, param_level);
      break;
//    case CHAR_TRCDISP:
//      cmdDisplay = !cmdDisplay; //==true ? false : true;
//#ifdef TRACE_ENABLE
//      TrcDisplaySet(cmdDisplay);
//#endif
//      break;
    case CHAR_SIFBAN:
      BannerToggle();
      break;
    case CHAR_TRCLOG:
      LogToggle();
      break;
    case CHAR_TRCSW:
#ifdef TRC_ENABLE
      TrcToggle();
#endif
		break;
    case CHAR_TRCPUR:
#ifdef TRACE_ENABLE
      TrcPurge();
#endif
      break;
    case CHAR_ESC:   /* escape */
      clearLine();
      if (getKeywords() == 1)
      {
        state = STATE_COMMAND;
      }
      clearKeywords(1);
      CliWrite(get_prompt(PROMPT_PARAM));
      break;
    case CHAR_TAB:   /* tab */
      if (param_idx == 0)
      {
        break;
      }
      param[param_level][param_idx]='\0';
      strcpy((char *)tab, (char *)param[param_level]);
      tab_idx = 0;
      if ((cc = CliBrowseParameter(pCmd->param[param_level], tab_idx, PARAM_UP, param[param_level])) == 0)
      {
        tab_idx++;
        param_idx = strlen((char *)param[param_level]);
        clearLine();
        CliWrite(get_prompt(PROMPT_PARAM));
      }
      break;
    case CHAR_BACKSP:/* back space - erase one char */
      if (param_idx)
      {
        param_idx--;
        CliWrite((const char *)"\b \b");
      }
      else
      {
        if (param_level != 0)
        {
          clearLine();
          param_level--;
          param_idx = strlen((char *)param[param_level]);
          CliWrite(get_prompt(PROMPT_PARAM));
        }
        else
        {
          clearLine();
          CliWrite(get_prompt(PROMPT_COMMAND));
          state = STATE_COMMAND;
        }
      }
      tab[0] = '\0';
      tab_idx = 0;
      break;
    case CHAR_CTRLR: /* refresh screen */
      RefreshScreen();
      break;
    case CHAR_CTRLC: /* clear cmd */
      clearLine();
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      state = STATE_COMMAND;
      break;
    case CLI_NL_CHAR:    /* command ended */
      cc = parseParameter(
        param[param_level],
        pCmd->param[param_level],
        (uint32_t *)&paramVal[param_level],
        true);
      if (cc == 0)
      {
        tab_idx = 0;
        if ((cc=executeCmd(&pCmd, cmd, paramVal, param_level)) == 0)
        {
          cc = 1;
          state = STATE_COMMAND;
        }
      }
      break;
    case CHAR_ARROWUP:
      if (hist_idx==0)
      {
        if (strlen((char *)history[CLI_CFG_HIST_SIZE-1].cmd) == 0)
        {
          break;
        }
        hist_idx = CLI_CFG_HIST_SIZE-1;
      }
      else
      {
        hist_idx--;
      }
      state = STATE_COMMAND;
      clearLine();
      pushTest(&pCmd);
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      for (i=0; i<(uint16_t)strlen((char *)history[hist_idx].cmd); i++)
      {
        execInput(history[hist_idx].cmd[i]);
      }
      break;
    case CHAR_ARROWDOWN:
      if (strlen((char *)history[hist_idx+1].cmd) == 0)
      {
        break;
      }
      state = STATE_COMMAND;
      hist_idx = (hist_idx + 1) % CLI_CFG_HIST_SIZE;
      clearLine();
      pushTest(&pCmd);
      clearKeywords(0);
      CliWrite(get_prompt(PROMPT_PARAM));
      for (i=0; i<(uint16_t)strlen((char *)history[hist_idx].cmd); i++)
      {
        execInput(history[hist_idx].cmd[i]);
      }
      break;
    case CHAR_ARROWRIGHT:
      clearLine();
      if ((cc=CliBrowseParameter(pCmd->param[param_level], tab_idx, PARAM_UP, param[param_level])) == 0)
      {
        tab_idx++;
        param_idx = strlen((char *)param[param_level]);
      }
      CliWrite(get_prompt(PROMPT_PARAM));
      break;
    case CHAR_ARROWLEFT:
      clearLine();
      if (tab_idx>1 && (cc=CliBrowseParameter(pCmd->param[param_level], tab_idx, PARAM_DOWN, param[param_level])) == 0)
      {
        tab_idx--;
        param_idx = strlen((char *)param[param_level]);
      }
      CliWrite(get_prompt(PROMPT_PARAM));
      break;
    case CHAR_SPACE:
       if (pCmd->param[param_level] == CLI_PARAM_STRING_LAST) {
        if (param_idx < (sizeof(param[param_level]) - 1) ) {
          param[param_level][param_idx++] = ch;
          if (echo == true) {
            CliWriteChar(ch);
          }
        }
        tab[0] = '\0';
        tab_idx = 0;
        
      } else {
        if ((cc=parseParameter(param[param_level], pCmd->param[param_level], (uint32_t *)&paramVal[param_level], false)) == 0) {
          tab_idx = 0;
          if (pCmd->param[param_level] == CLI_PARAM_NONE)
          {
            CliWriteChar(CHAR_BELL);
            CliWriteLn((const char *)CLI_NL "No more parameters are expected");
            CliWrite(get_prompt(PROMPT_PARAM));
          }
        }
      }
      break;
    default:         /* Other printable character */
      if ( ch < 32 )
      {
        break; /* ignore unhandled control chars */
      }
      if ( param_idx < (sizeof(param[param_level]) - 1) )
      {
        param[param_level][param_idx++] = ch;
        if (echo == true)
        {
          CliWriteChar(ch);
        }
      }
      tab[0] = '\0';
      tab_idx = 0;
    }
    break;

  case STATE_ECHO:
    switch (ch) {
    case CHAR_ESC: /* Exit test echo */
      CliWriteLn((const char *)"Exiting echo test");
      CliWrite(get_prompt(PROMPT_PARAM));
      state = STATE_COMMAND;
      break;
    case CLI_NL_CHAR:
      CliWrite(CLI_NL);
      break;
    default:
      if (echo == true)
      {
        CliWriteChar(ch);
      }
      break;
    }
    break;
  }

  return cc;
}


/****************************************************************************
 Function Name    :

 Input Parameters : ch - character inputted
 Return           : 0 - if no command was executed and 1 otherwise
 Module vars.     :
 Globale vars     :
 Description      :
 comments         : Handles command input to the test component
****************************************************************************/
int_fast16_t CliHandleInput(char ch)
{
  static int state = STATE_VT100_IDLE;
  int        cc = 0;

  switch (state)
  {
  case STATE_VT100_IDLE:
    switch (ch)
    {
    case CHAR_ESC:   /* escape */
      state = STATE_VT100_ESC;
      START_CHAR_TIMEOUT();
      break;
    case CHAR_TAB:   /* tab */
    case CHAR_BACKSP:/* back space - erase one char */
    case CHAR_CTRLR: /* refresh screen */
    case CHAR_CTRLC: /* clear cmd */
    case CHAR_CTRLE: /* Test echo */
    case CLI_CR:    /* command ended */
    default:         /* Other printable character */
      cc = execInput(ch);
      break;
    }
    break;

  case STATE_VT100_ESC:
    switch ((unsigned int)ch)
    {
    case '[':
      STOP_CHAR_TIMEOUT();
      state = STATE_VT100_BRACKET;
      break;
    case CHAR_TIMEOUT:
      // A single esc was pressed
      cc = execInput(CHAR_ESC);
      state = STATE_VT100_IDLE;
      break;
    case CHAR_ESC:   /* escape */
      cc = execInput(CHAR_ESC);
      START_CHAR_TIMEOUT();
      break;
    case CHAR_TAB:   /* tab */
    case CHAR_BACKSP:/* back space - erase one char */
    case CHAR_CTRLR: /* refresh screen */
    case CHAR_CTRLC: /* clear cmd */
    case CHAR_CTRLE: /* Test echo */
    case CLI_CR:    /* command ended */
    default:
      STOP_CHAR_TIMEOUT();
      cc = execInput(CHAR_ESC);
      cc = execInput(ch);
      state = STATE_VT100_IDLE;
    }
    break;

  case STATE_VT100_BRACKET:
    switch (ch)
    {
    case 'A':
      // Arrow up
      cc = execInput(CHAR_ARROWUP);
      break;
    case 'B':
      // Arrow down
      cc = execInput(CHAR_ARROWDOWN);
      break;
    case 'C':
      // Arrow right
      cc = execInput(CHAR_ARROWRIGHT);
      break;
    case 'D':
      // Arrow left
      cc = execInput(CHAR_ARROWLEFT);
      break;
    default:
      cc = execInput(ch);
    }
    state = STATE_VT100_IDLE;
    break;
  }

  return cc;
}

static void OutputPrint(const char * st)
{
  (void)st;
}

/********************************************************************************/

static void CliTimer(EvosEventParam_t parameter)
{
  CliHandleInput(CHAR_TIMEOUT);
}


void CliInit(const CliEntry_t * rootTable)
{
  cliCommands[0] = rootTable;
  outputPrint = OutputPrint;
  cliTimer = EvosEventRegister(CliTimer,(char *)"CliTimer");      
}

/********************************************************************************/

CliSend_t CliOutputDefine(CliSend_t newOutput)
{
  CliSend_t old=outputPrint;
  outputPrint = newOutput;
  return old;
}

static const char cliLine[] = "+------------------------------------------------------------+";

void CliWriteLine(void)
{
  CliWriteLn(cliLine);
}

int_fast16_t cliWriteLen;

#define LEN  (61)

void CliWriteChar(char ch)
{
  if (outputPrint) {
    static char sz[2] = " ";
    sz[0] = ch;
    (void)(*outputPrint)(sz);
    cliWriteLen = 1;
  }
}


void CliWriteEol(void)
{
  int16_t ix;
  ix = LEN - cliWriteLen;
  if (ix >= 0) {
    char sz[LEN+2];
    memset(sz, ' ', ix);
    sz[ix++] = '|';
    sz[ix] = 0;
    CliWrite(sz);
  }
  CliWrite(cliNewLine);
}

void CliWrite(const char * text)
{
  if (outputPrint) {
    (void)(*outputPrint)(text);
    cliWriteLen = strlen(text);
  }
}

void CliWriteLn(const char * text)
{
  if (outputPrint) {
    (void)(*outputPrint)(text);
    (void)(*outputPrint)(cliNewLine);
    cliWriteLen = strlen(text) + 1;
  }
}

//************************************************************************

#if (PLF_OS != PLF_OS_WINDOWS)

#include <stdio.h>
//struct __FILE
//{
//  int handle;
//  /* Whatever you require here. If the only file you are using is */
//  /* standard output using printf() for debugging, no file handling */
//  /* is required. */
//};
///* FILE is typedef’d in stdio.h. */
//FILE __stdout;

int fputc(int ch, FILE *f)
{
  /* Your implementation of fputc(). */
  CliWriteChar(ch);
  return ch;
}
//int ferror(FILE *f)
//{
//  /* Your implementation of ferror(). */
//  CliPrintf("ferror(%u)\n", f->handle);
//  return 0;
//}

#endif

//************************************************************************

int_fast16_t CliPrintf(const char *format, ...)
{
  va_list ap;
  int nr_of_chars;

  /*lint -esym(530,ap) */
  va_start(ap, format);      /* Variable argument begin */
  nr_of_chars =  vsnprintf((char *)buf, sizeof(buf), (char *)format, ap);
  va_end(ap);                /* Variable argument end */

  if (outputPrint) {
    (void)(*outputPrint)(buf);
  }
  cliWriteLen = nr_of_chars;
  return cliWriteLen;     
}


void CliCompleted(int16_t cc)
{
  if (cc == CLI_RESULT_OK)
    CliWriteLn("+OK");
  else {
    char sz[40];
    sprintf(sz, "-ERROR %d" CLI_NL, cc);
    CliWriteLn(sz);
  }


  clearKeywords(0);
  CliWrite(get_prompt(PROMPT_PARAM));
}


/********************************************************************************/

// Implemented in xxx.c
__weak void LogToggle(void)                        // ^L 
{
}
// Implemented in SIF.c
 __weak void BannerToggle(void)                        // ^B
{
}
// Implemented in TRC.c
__weak  void TrcEnable(bool onOff)        // ^T
{
}

/********************************************************************************/

#endif
