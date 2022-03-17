/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef __MODULE__
#define __MODULE__ "dlog"
#endif

#include <string.h>
#include <stdio.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"

#include "plf/nvm/nvm.h"
#include "plf/cli/cli.h"
#include "plf/trc/trc.h"
#include "plf/rtos/rtos.h"

#include "plf/dlog/dlog.h"
#include "plf/elog/elog.h"

#include "sif/sif.h"


/*******************************************************************************/

static DLog_t dLog;
static bool dLogInit = false;

/*******************************************************************************/

void DataLogAdd(DataLog_t * dataLog)
{
  uint32_t dFlashAddr;
  DLogEntry_t entry;
 
  if (!dLogInit) return;
  
  HalWdtFeed();

  RtcGet(&entry.log.time);
  entry.log.data = *dataLog;

  dFlashAddr = DFLASH_DLOG_START+(dLog*DLOG_ENTRY_SIZE);
  if ((dFlashAddr % DFLASH_SMALL_BLOCK_SIZE) == 0) { // every 4K / 128 == 32 entries 
    // 4K per sector....
    if (HalDFlashEraseSmallBlock(dFlashAddr, true) != 0) {
      return ;
    }
  }
  
  if (HalDFlashWrite(dFlashAddr, (uint8_t*)&entry, sizeof(DLogEntry_t), false) != 0) {
    return ;
  }
  
  if (++dLog >= DLOG_ENTRIES) dLog = 0;      
  NvmWrite(offsetof(NvmStruct_t, appl.dLog), (uint8_t *)&dLog, sizeof(DLog_t));
}

void DataLogClear(void)
{
  HalDFlashEraseAddrRange(DFLASH_DLOG_START, DFLASH_DLOG_START+DFLASH_DLOG_SIZE-1);

  memset(&dLog, 0, sizeof(dLog));
  NvmWrite(offsetof(NvmStruct_t, appl.dLog), (uint8_t *)&dLog, sizeof(DLog_t));  
  
  EVENT_LOG_ADD_S("Data Log Erased");
}
/*******************************************************************************/

static void DLogPrint(char * tab, int no, DLogEntry_t * pel)
{
  char timeStr[40];
  RtcGetSpecStr(timeStr, pel->log.time);
  
  CliPrintf(
    "%s#%-4d "
    "%s "
//    "iGen:%0.2fA vGen:%0.2fV iBC:%0.2fA vBat:%0.2fV wUsr:%0.2fW tBat:%d\n"
    , tab, no, 
    timeStr
//    pel->u.powerData.solarPanelAverageCurrent,
//    pel->u.powerData.solarPanelAverageVoltage,
//    pel->u.powerData.batteryAverageCurrent,
//    pel->u.powerData.batteryAverageVoltage,
//    pel->u.powerData.consumedEnergy,
//    pel->u.powerData.batteryTemp);
  );
}

/*******************************************************************************/

static uint32_t dataLogFind = 0;

// look back and find the entry "first older then time"
bool DataLogFindFirst(RtcTime_t time, DLogEntry_t * pel)
{
  int cnt;
  
  if (!dLogInit) {
    return false;
  }
  
  dataLogFind = dLog-1;  // newest
  cnt = 0;  
  int32_t dif;
  do {
    if (HalDFlashRead(DFLASH_DLOG_START+(dataLogFind*DLOG_ENTRY_SIZE), (uint8_t*)pel, sizeof(DLogEntry_t)) == 0) {
      if ((pel->log.time.all != RTC_TIME_INVALID) && (pel->log.time.all != RTC_TIME_ZERO)) {
        dif = RtcTimeDiff(&pel->log.time, &time);
        if (dif >= 0) { // "el->time - time <= 0"
          return true;
        }
      }
    } else {
      return false;      
    }
    if (--dataLogFind < 0) {
      dataLogFind = DLOG_ENTRIES-1;
    }
    HalWdtFeed();
  } while (++cnt < DLOG_ENTRIES);
  
  return false;
}

bool DataLogFindOlder(DLogEntry_t * pel)
{
  if (--dataLogFind < 0) {
    dataLogFind = DLOG_ENTRIES-1;
  }
  
  if (HalDFlashRead(DFLASH_DLOG_START+(dataLogFind*DLOG_ENTRY_SIZE), (uint8_t*)pel, sizeof(DLogEntry_t)) == 0) {
    if ((pel->log.time.all != RTC_TIME_INVALID) && (pel->log.time.all != RTC_TIME_ZERO)) {
      return true;
    } 
  }
  return false;      
}

bool DataLogFindNewer(DLogEntry_t * pel)
{
  if (++dataLogFind >= DLOG_ENTRIES) {
    dataLogFind = 0;
  }
  
  if (HalDFlashRead(DFLASH_DLOG_START+(dataLogFind*DLOG_ENTRY_SIZE), (uint8_t*)pel, sizeof(DLogEntry_t)) == 0) {
    if ((pel->log.time.all != RTC_TIME_INVALID) && (pel->log.time.all != RTC_TIME_ZERO)) {
      return true;
    } 
  }
  return false;      
}

// look back and find the newest entry
bool DataLogFindNewest(DLogEntry_t * const pel)
{
  if (!dLogInit) {
    return false;
  }
  
  dataLogFind = dLog > 0 ? (dLog - 1) : (DLOG_ENTRIES - 1);  // newest
  
  if (HalDFlashRead(DFLASH_DLOG_START+(dataLogFind*DLOG_ENTRY_SIZE), (uint8_t*)pel, sizeof(DLogEntry_t)) == 0) {
    if ((pel->log.time.all != RTC_TIME_INVALID) && (pel->log.time.all != RTC_TIME_ZERO)) {
      return true;
    }
  } else {
    return false;      
  }
  
  return false;
}

/*******************************************************************************/

void DLogInit(void)
{
  TRACE(TRC_TA_PLF, TRC_TL_COMPONENT, "DLogInit()");
  
  if (sizeof(DLogEntry_t) != DLOG_ENTRY_SIZE)
    for (;;) ;
  
  if (nvmState != NVM_OK) {
    dLog = 0;
    NvmWrite(offsetof(NvmStruct_t, appl.dLog), (uint8_t *)&dLog, sizeof(DLog_t));
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.dLog), (uint8_t *)&dLog, sizeof(DLog_t));
    if (dLog >= DLOG_ENTRIES) 
      dLog = 0;      
  }
  
  dLogInit = true; 
}

/*******************************************************************************/
#ifdef CLI_ENABLE
/*******************************************************************************/

int_fast16_t CliDLogShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  PlfTime_t t, t2;
  uint32_t nxt, cnt;
  DLogEntry_t el;


  if (cliShowHeader) {
    CliWriteLine();
    CliWrite(" DATA LOG (DLOG):");
    CliWriteEol();
    CliWriteLine();
  }
  
  CliWriteLn("DATA log:");       
  nxt = dLog; 
  cnt = 0;
  PlfTimeMsGet(&t);
  do {    
    if (HalDFlashRead(DFLASH_DLOG_START+(nxt*DLOG_ENTRY_SIZE), (uint8_t*)&el, sizeof(DLogEntry_t)) != 0) {
      CliPrintf("DFLASH read failed\n");       
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    if (el.log.time.all != RTC_TIME_INVALID) {
      DLogPrint("  ", nxt, &el);
      cnt++;
    } else {
      CliPrintf("%u\r", nxt);  // showing seaching number on same line while finding used entry
    }
    if (++nxt >= DLOG_ENTRIES) 
      nxt = 0;      
    HalWdtFeed();
  } while (nxt != dLog);
  PlfTimeMsGet(&t2);
  CliPrintf("  Total %u out of max %u entries\n", cnt, DLOG_ENTRIES); 
  CliPrintf("  Listing took %u msec\n", TIME_DIF(t2, t));

  return CLI_RESULT_OK;
}

int_fast16_t CliDLogErase(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  DataLogClear();
  return CLI_RESULT_OK;
}

static int_fast16_t DLogTstMak(CliParam_t num, CliParam_t param2, CliParam_t param3)
{
  DataLog_t  dataLog;
  
  if (num < 1) {
    CliPrintf("num: 1..\n");       
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }

  for (uint32_t n = 1; n<num; n++) {
    DataLogAdd(&dataLog);
  }

  return CLI_RESULT_OK;
}

//static int_fast16_t DLogTstFnd(CliParam_t param1, CliParam_t param2, CliParam_t param3)
//{
//  int no;
//  RtcTime_t rtcTime;
//  time_t t;
//  DLogEntry_t el;
//  
//  TimeGet(&t);
//  t -= (60*60*24); // one day back;
//  Time2Rtc(&rtcTime, t);
//  no = 1;
//  if (DataLogFindFirst(rtcTime, &el)) {
//    for (;;) {
//      DLogPrint("", -1, &el);
//  
//      if (++no > 4)
//        break;
//      if (!DataLogFindOlder(&el)) 
//        break;
//    }
//  } 
//  return CLI_RESULT_OK;
//}


CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(dlog)
  CLI_ENTRY0( "show", "# Show DLOG", CliDLogShow)

  CLI_ENTRY0( "erase", "# Erase DLOG", CliDLogErase)

  CLI_ENTRY1( "tmake", "Make [1..] TEST LOGGINs", DLogTstMak, CLI_PARAM_UINT32)
//  CLI_ENTRY1( "tstfnd", "find last 3 older then yesterday...", DLogTstFnd, CLI_PARAM_UINT32)

#ifdef TRC_ENABLE
  CLI_SUBTEST("trc",  "Trace System", trc)
#endif

CLI_END_TABLE(dlog)

#endif
