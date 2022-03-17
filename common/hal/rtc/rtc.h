/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_RTC_H
#define HAL_RTC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <time.h>
#include "hal/hal.h"
#include "plf/plf.h"
#include "plf/cli/cli.h"


/******************************************************************************/
/* RTC Backup Data Register */

#if (PLF_OS != PLF_OS_WINDOWS)

typedef uint8_t         RtcBackupData_t[RTC_BKP_NUMBER*4];  // 20 byte (RTC_BKP_NUMBER == 5)

extern void RtcBackupDataRead(RtcBackupData_t data);
extern void RtcBackupDataWrite(RtcBackupData_t data);

#endif

/******************************************************************************/
/* System RTC Time.                                                           */
/* RTC time == UTC time == GMT == time in London                              */

#if (PLF_OS != PLF_OS_WINDOWS)
extern RTC_HandleTypeDef hrtc;
#endif


extern bool RtcSetVal(RTC_DateTypeDef * pDate, RTC_TimeTypeDef * pTime);

#define RTC_TIME_ZERO       (0x0000000000000000ULL)
#define RTC_TIME_INVALID    (0xFFFFFFFFFFFFFFFFULL)

#pragma pack(push, 1)
typedef union {
  struct {
    uint16_t        year;     // 2017 ...
    uint8_t         mon;      // 1..12
    uint8_t         mday;     // 1..31
    uint8_t         hour;     // 0..23
    uint8_t         min;      // 0..59
    uint8_t         sec;      // 0..59
  }									u;
  uint64_t          all;      // RTC_TIME_INVALID ???
}                   RtcTime_t;
#pragma pack(pop)

//return seconds (signed) between t2 - t1
extern int32_t RtcTimeDiff(const RtcTime_t * t1, const RtcTime_t * t2);

extern void RtcGet(RtcTime_t * pRtcTime);
extern void RtcSet(const RtcTime_t * pRtcTime);
extern bool RtcSetItems(
  int year,     
  int mon,     
  int mday,     
  int hour,     
  int min,     
  int sec); 

// 2017-12-22  8:02:33"
extern char * RtcStr(char * sz, const RtcTime_t * rtcTime);
// Current RTC time !!!
extern char * RtcGetStr(char * sz);   
extern bool RtcSetStr(char * sz);   

extern void RtcGetSpecStr(char * sz, RtcTime_t rtcTime);

typedef char PlfTimeStr_t[20];    // "999999.23:59:59.999"

extern void PlfTime_DHMSM_StrConv(PlfTimeStr_t sz, PlfTime_t time);
extern void PlfTime_DHMSM_StrGet(PlfTimeStr_t sz);
  
/*******************************************************************************/

#if (PLF_OS != PLF_OS_WINDOWS)
extern time_t time(time_t * t);
#endif

typedef time_t          UtcTime_t;
  
extern UtcTime_t RtcToUtcTime(const RtcTime_t * rtcTime);

extern bool UtcTimeSet(UtcTime_t utcTime);
extern void UtcTimeGet(UtcTime_t * pUtcTime);

  
/*******************************************************************************/

#ifdef CLI_ENABLE
  
  extern int_fast16_t CliRtcSet(CliParam_t yyyymmdd, CliParam_t hhmmss, CliParam_t param3);

#endif

extern void HalRtcInit(void);

/*******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // HAL_RTC_H

