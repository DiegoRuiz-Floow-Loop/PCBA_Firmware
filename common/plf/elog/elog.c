/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef __MODULE__
#define __MODULE__ "elog"
#endif

#include <string.h>
#include <stdio.h>
#include <stddef.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"
#include "hal/dflash/dflash.h"

#include "plf/nvm/nvm.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

#include "plf/elog/elog.h"

#include "sif/sif.h"
#include "sup/sup.h"


/*******************************************************************************/

static ELog_t eLog;

typedef uint8_t xx_t[1+ELOG_ENTRY_SIZE-sizeof(ELogEntry_t)];

static bool eLogInit = false;

/******************************************************************************/
/* Event Function to slice longer runs up... */

EvosEventHandle_t       pony;

typedef enum {
  PS_NONE,
  PS_EVENT_LIST,
  PS_EVENT_MAKE,
  PS_Last
}                       PonyState_t;
PonyState_t             ponyState = PS_NONE;

typedef union {
  struct {
    char                * info;
    uint32_t            firstAddr;
    uint16_t            oldest;    
    uint16_t            numbers;
    uint16_t            nxt;
    uint16_t            count;
		uint16_t						shown;
  }                     l;
  struct {
    char                * info;
    uint32_t            num;
    //ELogType_t          type;sh	
    uint32_t            n;
  }                     m;
  uint32_t              ui32;
}                       PonyData_t;

PonyData_t              ponyData;


static const char newLine[] = "\r\n";
static char logLine[sizeof(ELogEntry_t)+30];

char *  LogLineMake(ELogEntry_t * e)
{
 
  char timeStr[40];
  char tickStr[40];
  RtcGetSpecStr(timeStr, e->time);
  PlfTime_DHMSM_StrConv(tickStr, e->tick);
  sprintf(logLine, "  %s/%s %u \"%s\" \"%s:%u\"%s", timeStr, tickStr, ponyData.l.nxt, e->dat, e->fileName, e->fileLine, newLine);      
  
  //TRACE_S2(TRC_TA_MWR, TRC_TL_2, "LogLineMeka", logLine);
  return logLine;
}

static void Pony(EvosEventParam_t xx)
{
  int i = 0;
  if (ponyState == PS_EVENT_LIST) {
    
    ELogEntry_t el;

    if (ponyData.l.count == 0) {
      CliWriteLn(ponyData.l.info);       
      ponyData.l.nxt = ponyData.l.oldest; 
    }
    
    for (;;) {  
      
      if (HalDFlashRead(ponyData.l.firstAddr+(ponyData.l.nxt*ELOG_ENTRY_SIZE), (uint8_t*)&el, sizeof(el)) != 0) {
        CliPrintf("DFLASH read failed\n");       
        //return CLI_RESULT_ERROR_UNDEFINED;
      }
      if (el.time.all != RTC_TIME_INVALID) {
				ponyData.l.shown++;
				
        el.dat[ELOG_DATA_SIZE-1] = 0;
        
        //      char * s;
        //      for (s = el.fileName; *s != 0; s++)
        //        if (*s <= ' ') *s = '_';
        //      for (s = el.dat; *s != 0; s++)
        //        if (*s <= ' ') *s = '_';
        //      
        CliWrite(LogLineMake(&el));
        //CliPrintf("  %s/%s %u \"%s\" \"%s:%u\"\n", timeStr, tickStr, ponyData.l.nxt, el.dat, el.fileName, el.fileLine);    
        while (SifTxBusy()) ;
      } else {
        CliPrintf("%u\r", ponyData.l.nxt);  // showing seaching number on same line while finding used entry
      }
      if (++ponyData.l.nxt >= ponyData.l.numbers) 
        ponyData.l.nxt = 0;      
      HalWdtFeed();
      if (ponyData.l.nxt == ponyData.l.oldest) {
        CliPrintf("  Total %u out of max %u entries\n", ponyData.l.shown, ponyData.l.numbers);         
        CliHandleInput('\r');
        return;
      }
  		ponyData.l.count++;
      if (++i > 50) {
        EvosEventSetDelta(pony, 100, 0);
        return;
      }
    };
  } else if (ponyState == PS_EVENT_MAKE) {
    char sz[80];
    if (ponyData.m.n == 0) {      
      CliWriteLn(ponyData.m.info);       
    }

    for (;;) {
      HalWdtFeed();
      sprintf(sz, "test event #%u", ponyData.m.n);
      if (EventLogAddS3(__MODULE__, __LINE__, sz, NULL, NULL)) {
        CliPrintf("\r%u  ", 1+ponyData.m.n);       
      } else {
        CliPrintf("\rERROR !!!");       
        break;
      }
      if (++ponyData.m.n >= ponyData.m.num ) {
        CliPrintf("\n");        
        CliHandleInput('\r');
        return;
      }
      if (++i > 50) {
        EvosEventSetDelta(pony, 100, 0);
        return;
      }
    }
  } else {
    
    
  }
  
}

/*******************************************************************************/

const char * EventLogGet(int * nextNo)
{
  ELogEntry_t el;

  for (;;) {
    if (HalDFlashRead(DFLASH_ELOG_START+(*nextNo * ELOG_ENTRY_SIZE), (uint8_t*)&el, sizeof(el)) != DFLASH_OK) {
      *nextNo = ELOG_ENTRIES;
      return newLine;      
    } 
    (*nextNo)++;
    if (el.time.all != RTC_TIME_INVALID) {
      el.dat[ELOG_DATA_SIZE-1] = 0;
      ponyData.l.nxt = *nextNo;
      return LogLineMake(&el);
    }    
    if (*nextNo >= ELOG_ENTRIES)
      return newLine;
  }
}

/*******************************************************************************/

bool EventLogAddData(  
  const ELogFileName_t  fileName,
  int                   fileLine,
  const ELogData_t      dat)
{
  int_fast8_t stat;
  uint32_t dFlashAddr;
  ELogEntry_t el;
  char * p;
  
  if (!eLogInit) 
    return false;

  TRACE(TRC_TA_MWR, TRC_TL_3, "EventLogAdd()");
  HalWdtFeed();
  
  memset(&el, 0, sizeof(el));

  RtcGet(&el.time);
  PlfTimeMsGet(&el.tick);
  
  memcpy(el.dat, dat, sizeof(el.dat));

  
  /* only save actual file name, not path */
  //remove ".c"
  p = strrchr(fileName, '\\'); // Find last seperator 
  if (p != NULL) {
    p++;
    strncpy(el.fileName, p, sizeof(ELogFileName_t)-1);
  } else {
    strncpy(el.fileName, fileName, sizeof(ELogFileName_t)-1);
  }
  el.fileName[sizeof(ELogFileName_t)-1] = 0;

  el.fileLine = fileLine;
  
  dFlashAddr = DFLASH_ELOG_START+(eLog*ELOG_ENTRY_SIZE);
  if (++eLog >= ELOG_ENTRIES) 
        eLog = 0;      
  
  if ((dFlashAddr % DFLASH_SMALL_BLOCK_SIZE) == 0) { // every 4K
    // 4K per sector....
    stat = HalDFlashEraseSmallBlock(dFlashAddr, true);
    if (stat != 0) {
      return false;
    }
  }
  
  stat = HalDFlashWrite(dFlashAddr, (uint8_t*)&el, sizeof(el), false);
  if (stat != 0) {
    return false;
  }
  
  NvmWrite(offsetof(NvmStruct_t, appl.eLog), (uint8_t *)&eLog, sizeof(ELog_t));
  return true;
}

/*******************************************************************************/

static void MyStrCat(char * szDst, const char * szSrc, int maxLen)
{
  int ld = strlen(szDst);
  
  for (;;) {
    if (*szSrc == 0) break;
    if (ld >= maxLen-1) break;
    if (*szSrc >= ' ') {
      szDst[ld++] = *szSrc;
    }
    szSrc++;
  }
  szDst[ld] = 0;    
}
  
bool EventLogAddS3(  
  const ELogFileName_t  fileName,
  int                   fileLine,
  const char            * txt,
  const char            * txt2,
  const char            * txt3)
{
  char dat[ELOG_DATA_SIZE];
  strncpy(dat, txt, ELOG_DATA_SIZE-1);
  if (txt2) {
    if (txt2[0] != 0) {
      MyStrCat(dat, " ", ELOG_DATA_SIZE-1);
      MyStrCat(dat, txt2, ELOG_DATA_SIZE-1);
      if (txt3) {
        if (txt3[0] != 0) {
          MyStrCat(dat, " ", ELOG_DATA_SIZE-1);
          MyStrCat(dat, txt3, ELOG_DATA_SIZE-1);
        }
      }
    }
  }
  dat[ELOG_DATA_SIZE-1] = 0;
  return EventLogAddData(fileName, fileLine, (uint8_t *)dat);
}
  
  
/*******************************************************************************/

void ELogInit(void)
{
  TRACE(TRC_TA_MWR, TRC_TL_COMPONENT, "ELOG_Init()");
  
  if (nvmState != NVM_OK) {
    eLog = 0;
    NvmWrite(offsetof(NvmStruct_t, appl.eLog), (uint8_t *)&eLog, sizeof(ELog_t));
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.eLog), (uint8_t *)&eLog, sizeof(ELog_t));
    if (eLog >= ELOG_ENTRIES) 
      eLog = 0;      
  }
  eLogInit = true;  

  pony = EvosEventRegister(Pony, "pony");
	
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t CliELogShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  ponyState             = PS_EVENT_LIST;
  ponyData.l.info       = "\n\nEVENT log:";
  ponyData.l.firstAddr  = DFLASH_ELOG_START;
  ponyData.l.oldest     = eLog;
  ponyData.l.numbers    = ELOG_ENTRIES;  
  ponyData.l.count      = 0;
	ponyData.l.shown			= 0;
  EvosEventSetNow(pony, 0);
  return CLI_RESULT_OK;
}

int_fast16_t CliELogErase(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{

  HalDFlashEraseAddrRange(DFLASH_ELOG_START, DFLASH_ELOG_START+DFLASH_ELOG_SIZE-1);
  memset(&eLog, 0, sizeof(eLog));
  NvmWrite(offsetof(NvmStruct_t, appl.eLog), (uint8_t *)&eLog, sizeof(ELog_t));   
  EVENT_LOG_ADD_S("Info Log Erased");
  EVENT_LOG_ADD_S("Error Log Erased");  
  return CLI_RESULT_OK;
}

static int_fast16_t ELogTst(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  ponyData.m.num  = (uint32_t)param1;

  if ((ponyData.m.num > 10000) || (ponyData.m.num < 1)) {
    CliPrintf("num: 1..100\n");       
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  
  ponyState   = PS_EVENT_MAKE;
  ponyData.m.info = "\n\nMaking Test Event:";
  ponyData.m.n = 0;
  EvosEventSetNow(pony, 0);
	
  return CLI_RESULT_OK;
}

static int_fast16_t ELogTst2(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  char sz[0x100];
  memset(sz, 'x', sizeof(sz));
  sz[255] = 0;
  if (EventLogAddS3(__MODULE__, __LINE__, sz, NULL, NULL)) {
    CliPrintf("\rOK");       
  } else {
    CliPrintf("\rERROR !!!");       
  }	
  return CLI_RESULT_OK;
}

CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(elog)
  CLI_ENTRY0( "show", "# Show Event elog", CliELogShow)
  CLI_ENTRY0( "erase", "# Erase ELOG", CliELogErase)

  CLI_ENTRY1( "tmake", "Make [1..N] LOGGINs", ELogTst, CLI_PARAM_UINT32)
  CLI_ENTRY0( "test", "Make Test Logging", ELogTst2)

#ifdef TRC_ENABLE
  CLI_SUBTEST("trc", "Trace System", trc)
#endif
CLI_END_TABLE(elog)

/******************************************************************************/
#endif
/******************************************************************************/
