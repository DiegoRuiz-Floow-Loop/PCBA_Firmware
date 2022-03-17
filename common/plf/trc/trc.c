/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/cfg/trc_cfg.h"

/******************************************************************************/
#ifdef TRC_ENABLE
/******************************************************************************/

#include <string.h>
#include <stdio.h>

#include "hal/hal.h"

#include "plf/plf.h"
#include "plf/nvm/nvm.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"
#include "plf/fifo/fifo.h"
#include "plf/evos/evos.h"
#include "plf/heap/heapx.h"

#include "sif/sif.h"

/******************************************************************************/

#if (PLF_OS==PLF_OS_RTOS)

#include "plf/rtos/rtos.h"

static RtosBinSemaphor_t        trcLockSem = NULL;
	
static void xTrcPortLockInit(void)
{
  RTOS_BIN_SEMAPHOR_CREATE(trcLockSem);
  RTOS_BIN_SEMAPHOR_GIVE(trcLockSem);
}

void TrcPortLock(void)
{
  if (rtosRunning) {
		if (!trcLockSem) {
			xTrcPortLockInit(); 
		}
    RTOS_BIN_SEMAPHOR_TAKE(trcLockSem, 3000);
	}
}
void TrcPortUnLock(void)
{
  if (rtosRunning) {
		if (!trcLockSem) {
			xTrcPortLockInit(); 
		}
    RTOS_BIN_SEMAPHOR_GIVE(trcLockSem);
	}
}



#else
//static void xTrcPortLockInit(void)
//{
//}
void TrcPortLock(void)
{
}
void TrcPortUnLock(void)
{
}
#endif

/******************************************************************************/


static bool TrcInitOk = false;
TrcConfig_t trcConfig;

static void SaveAll(void)
{
#ifdef NVM_ENABLE  
 
#if (NVM_ACCESS==NVM_ACCESS_NAME)     
  NvmWrite(NVM_ID_TRC, trcConfig);
#elif (NVM_ACCESS==NVM_ACCESS_ADDRESS)
  NvmWrite(offsetof(NvmStruct_t, trcConfig), (uint8_t *)&trcConfig, sizeof(trcConfig));
#elif (NVM_ACCESS==NVM_ACCESS_ID)
  NvmWrite(NVM_ID_TRC, trcConfig);
#else
#endif
#endif
}

static void LoadAll(void)

{
#ifdef NVM_ENABLE  
	if (nvmState != NVM_OK)
		NvmInit();
  if (nvmState != NVM_OK) {
    memset(&trcConfig, 0x0, sizeof(trcConfig));  // all off
    SaveAll();
  } else  {

#if (NVM_ACCESS==NVM_ACCESS_NAME)
    NvmRead(NVM_ID_TRC, trcConfig);
#elif (NVM_ACCESS==NVM_ACCESS_ADDRESS)
    NvmRead(offsetof(NvmStruct_t, trcConfig), (uint8_t *)&trcConfig, sizeof(trcConfig));
#elif (NVM_ACCESS==NVM_ACCESS_ID)
  NvmRead(NVM_ID_TRC, trcConfig);
#else
		memset(&trcConfig, 0, sizeof(trcConfig));
#endif
		
  }
#else
  memset(&trcConfig, 0x0, sizeof(trcConfig));  // all off
#endif
}

/******************************************************************************/

static void TrcPutString(const char * st)
{
//  st = st;
}


TrcOutputFunction_t TrcPrint = &TrcPutString;

TrcOutputFunction_t TrcOutputDefine(TrcOutputFunction_t p)
{
  TrcOutputFunction_t xx  = TrcPrint;
  TrcPrint = p;
  return xx;
}

/******************************************************************************/

static  const char  * const  lstTrcArea[TRC_TA_Last] = {
	TRC_AREA_TEXT_0,
	TRC_AREA_TEXT_1,
	TRC_AREA_TEXT_2,
	TRC_AREA_TEXT_3,
	TRC_AREA_TEXT_4,
	TRC_AREA_TEXT_5,
	TRC_AREA_TEXT_6,
	TRC_AREA_TEXT_7,
};

static const char  txtTrcLevel0[]= " " TRC_LEVEL_TEXT_0;
static const char  txtTrcLevel1[]= " " TRC_LEVEL_TEXT_1;
static const char  txtTrcLevel2[]= " " TRC_LEVEL_TEXT_2;
static const char  txtTrcLevel3[]= " " TRC_LEVEL_TEXT_3;
static const char  txtTrcLevel4[]= " " TRC_LEVEL_TEXT_4;
static const char  txtTrcLevel5[]= " " TRC_LEVEL_TEXT_5;
static const char  txtTrcLevel6[]= " " TRC_LEVEL_TEXT_6;
static const char  txtTrcLevel7[]= " " TRC_LEVEL_TEXT_7;
static  const  char  * const lstTrcLevel[TRC_TL_Last] = {
   txtTrcLevel0,
   txtTrcLevel1,
   txtTrcLevel2,
   txtTrcLevel3,
   txtTrcLevel4,
   txtTrcLevel5,
   txtTrcLevel6,
   txtTrcLevel7
};

/******************************************************************************/

bool TrcEnabled(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel)
{
  if (TrcInitOk) {
    if ((traceLevel==TRC_TL_FATAL) ||
      (trcConfig.trcEnabled &&
       (trcConfig.areas[traceArea] & (1 << traceLevel)) != 0))  {
      return true;
    }
  }
  return false;
}

static bool TrcConditionOK(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel)
{
  if (TrcEnabled(traceArea, traceLevel)) {
    char txtTrcStr[20];
    uint32_t sec, ms;
#if (PLF_OS==PLF_OS_RTOS)
    RtosTime_t s;
    s = RTOS_GET_TICK_COUNT();
#else
    PlfTime_t s;
    PlfTimeMsGet(&s);
#endif
    sec = s / 1000;
    ms = s % 1000;
    sprintf((char *)txtTrcStr, "[%03d.%03d ", sec, ms);
    TrcPortLock();
    TrcPrint(txtTrcStr);
    TrcPrint(lstTrcArea[traceArea]);
    TrcPrint(lstTrcLevel[traceLevel]);
    TrcPrint("]" " ");
    return true;
  }
  return false;
}

/******************************************************************************/

#include <stdarg.h>

void TrcTrace(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  const char                  *szInfo)
{
  if (TrcConditionOK(traceArea, traceLevel)) {
    TrcPrint(szInfo);
    TrcPrint(cliNewLine);
    TrcPortUnLock();
  }
}

void TrcTraceS2(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  const char                  *szInfo,
  const char                  *szInfo2)
{
  if (TrcConditionOK(traceArea, traceLevel)) {
    TrcPrint(szInfo);
    TrcPrint(" ");
    TrcPrint(szInfo2);
    TrcPrint(cliNewLine);
    TrcPortUnLock();
  }
}


void TrcTraceS3(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  const char                  *szInfo,
  const char                  *szInfo2,
  const char                  *szInfo3)
{
  if (TrcConditionOK(traceArea, traceLevel)) {
    TrcPrint(szInfo);
    TrcPrint(" ");
    TrcPrint(szInfo2);
    TrcPrint(" ");
    TrcPrint(szInfo3);
    TrcPrint(cliNewLine);
    TrcPortUnLock();
  }
}

#define TRC_VA_MAX     (1024)
void TrcTraceArg(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  char                        *format,
  ...)
{
  char buf[TRC_VA_MAX];
  if (*format >= ' ') {
    if (TrcConditionOK(traceArea, traceLevel)) {   
      va_list args;
      va_start(args, format);
      vsnprintf(buf, TRC_VA_MAX, format, args);
      va_end(args);
      buf[TRC_VA_MAX-1] = 0;
      int iw = strlen(buf);
      // remove faling \r \n etz...
      for (;;) {
        if (buf[iw-1] >= ' ') break;
        if (iw<=4) break;
        iw--;
        buf[iw] = 0;
      }
      TrcPrint(buf);
      TrcPrint(cliNewLine);
    }
    TrcPortUnLock();
  }
}

void TrcTraceArgX(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  char                        *format,
  ...)
{
  if (*format >= ' ') {
    if (TrcConditionOK(traceArea, traceLevel)) {
      TrcPrint(format);
      TrcPrint(cliNewLine);
    }
    TrcPortUnLock();
  }
}


/******************************************************************************/

void TrcTraceBlkFatal(
  const TraceArea_t     traceArea,    /* see enum above */
  const char            *szInfo,
  const uint8_t         *data,
  uint16_t              cnt)
{
  uint8_t i, j;

	char txtTrcStr[80];
	strcpy((char *)txtTrcStr, (const  char*)szInfo);
	TrcPrint(txtTrcStr);
	TrcPrint(cliNewLine);
	while (cnt)
	{
		txtTrcStr[0] = 0;
		for (i=0; i<16 && cnt; i++, cnt--)
		{
			sprintf((char *)txtTrcStr+strlen((char *)txtTrcStr), "%02x ", data[i]);
		}
		j = i;
		while (j++<16) {
			strcat((char *)txtTrcStr, "   ");
		}
		strcat((char *)txtTrcStr, " ");
		for (j=0; j<i; j++) {
			if ((*data >= ' ') &&  (*data <= 0x7F)){
				sprintf((char *)txtTrcStr+strlen((char *)txtTrcStr), "%c", (uint8_t)*data++);
			} else {
				strcat((char *)txtTrcStr+strlen((char *)txtTrcStr), ".");
				data++;
			}
		}
		TrcPrint(txtTrcStr);
		TrcPrint(cliNewLine);
	}
	//TrcPrint(newLine);
	TrcPortUnLock();
}

void TrcTraceBlk(
  const TraceArea_t     traceArea,    /* see enum above */
  const char            *szInfo,
  const uint8_t         *data,
  uint16_t              cnt)
{
//  uint8_t i, j;

  if (TrcConditionOK(traceArea, TRC_TL_BLOCK))
  {
		TrcTraceBlkFatal(traceArea, szInfo, data, cnt);
  }
}

/******************************************************************************/

void TrcTraceDefine(
  TraceArea_t           traceArea,
  TraceLevel_t          traceLevel,
  bool                on)
{
  if (on) {
    trcConfig.areas[traceArea] |= (1 << traceLevel);
  } else {
    trcConfig.areas[traceArea] &= ~(1 << traceLevel);
  }
  SaveAll();
}

/******************************************************************************/

void TrcTraceDefineLevelMask(
  TraceArea_t           traceArea,    /* see enum above */
  uint8_t               traceLevelMask)
{
  trcConfig.areas[traceArea] = traceLevelMask;
  SaveAll();
}

void TrcTraceDefineAreaMask(
  TraceLevel_t          traceLevel,    /* see enum above */
  uint8_t               traceAreaMask)
{
  uint8_t          traceArea;
  for (traceArea=0; traceArea<TRC_TA_Last; traceArea++) {
    if ((traceAreaMask & (traceAreaMask << traceArea)) > 0) {
      trcConfig.areas[traceArea] |= (1 << traceLevel);
    } else {
      trcConfig.areas[traceArea] &= ~(1 << traceLevel);
    }
  }
  SaveAll();
}

/******************************************************************************/

void TrcGetEnable(bool * onOff)
{
  *onOff = trcConfig.trcEnabled;
}

void TrcToggle(void)
{
  trcConfig.trcEnabled = !trcConfig.trcEnabled;
  SaveAll();
}

void TrcEnable(bool onOff)
{
  trcConfig.trcEnabled = onOff;
  SaveAll();
}

void TrcNvmInit(void)
{
  memset((void *)&trcConfig, 0, sizeof(trcConfig));
  SaveAll();
}


void TrcInit(void)
{
  if (!TrcInitOk) {
    memset((void *)&trcConfig, 0, sizeof(trcConfig));
    LoadAll();
    TrcInitOk = true;
  }
}

/******************************************************************************/

#ifdef CLI_ENABLE

#define TRC_BUF_SIZE  (80)

extern int_fast16_t TrcShowStatus(CliParam_t param1, CliParam_t param2, CliParam_t param3);

int_fast16_t TrcShowStatus(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  TraceArea_t ta;
  TraceLevel_t tl;
  char buf[TRC_BUF_SIZE];
  
  TrcPortLock();

  CliWriteLine();
  CliWriteLn("Trace status and filter:");
  if (trcConfig.trcEnabled) {
    CliWriteLn("  Trace is ENABLED");
  } else {
    CliWriteLn("  Trace is DISABLED");
  }
  /* Print the trace mask */
  for (ta=(TraceArea_t)0; ta < TRC_TA_Last; ta++) {
    strcpy((char *)buf, "  ");
    strcat((char *)buf, lstTrcArea[ta]);
    CliWrite(buf);

    for (tl=(TraceLevel_t)0; tl < TRC_TL_Last; tl++) {
      if (trcConfig.areas[ta] & (1 << tl)) {
        strcpy((char *)buf, lstTrcLevel[tl]);
        CliWriteChar(' ');
        CliWrite(buf);
      } else {
        CliWrite("     ");
      }
    }
    CliWrite(cliNewLine);
  }
  CliWriteLine();
  TrcPortUnLock();  
  return CLI_RESULT_OK;
}

static int_fast16_t trc_filter(CliParam_t domain, CliParam_t level, CliParam_t on)
{
  CliParam_t dp;
  dp = 0;
  if (domain == 0xff) { // all
    // Set all domain to the level
    TraceArea_t ta;
    for (ta=(TraceArea_t)0; ta < TRC_TA_Last; ta++) {
      if (on) {
        trcConfig.areas[ta] |= (TraceLevel_t)level;
      } else {
        trcConfig.areas[ta] &= ~(TraceLevel_t)level;
      }
    }
  } else {
    if (on) {
      trcConfig.areas[(TraceArea_t)domain] |= (TraceLevel_t)level;
    } else {
      trcConfig.areas[(TraceArea_t)domain] &= ~(TraceLevel_t)level;
    }
  }
  SaveAll();
  TrcShowStatus(dp, dp, dp);
  return CLI_RESULT_OK;
}

static int_fast16_t trc_filter_std(CliParam_t p1, CliParam_t p2, CliParam_t p3)
{

  uint8_t    traceArea;
  uint8_t    traceMask = 0xFF;
  traceMask &= ~(1 << TRC_TL_3);
  traceMask &= ~(1 << TRC_TL_4);
  traceMask &= ~(1 << TRC_TL_5);
  traceMask &= ~(1 << TRC_TL_BLOCK);
  
  for (traceArea=0; traceArea<TRC_TA_Last; traceArea++) {
    trcConfig.areas[traceArea] = traceMask;
  }
  TrcEnable(true);
  TrcShowStatus(0, 0, 0);
  return CLI_RESULT_OK;
}

static int_fast16_t trc_start_strace(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  TrcEnable(true);
  return CLI_RESULT_OK;
}
static int_fast16_t trc_stop_strace(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  TrcEnable(false);
  return CLI_RESULT_OK;
}

CLI_START_TABLE(trc)
  CLI_ENTRY0("show", "Show trace status", TrcShowStatus)
  CLI_ENTRY0("start", "Start trace", trc_start_strace)
  CLI_ENTRY0("stop",  "Stop trace", trc_stop_strace)
  CLI_ENTRY3("filter", "Set trace filter", trc_filter, CLI_PARAM_DOMAIN, CLI_PARAM_TRACEMASK, CLI_PARAM_ONOFF)
  CLI_ENTRY0("std", "Set Standard trace filter", trc_filter_std)

	//CLI_SUBTEST("trc",      "Trace system", trc)
//  CLI_ENTRY1( "spw",   "Set Password", CliPasswordSet, CLI_PARAM_STRING)
//  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(trc)

#endif

/******************************************************************************/

#else 


#endif 

/******************************************************************************/

