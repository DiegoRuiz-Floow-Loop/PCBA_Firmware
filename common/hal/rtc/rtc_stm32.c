/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/


//#define __MODULE__      "hal_rtc"


#include <stdio.h>
#include <string.h>

#include "hal/hal.h"
#include "hal/rtc/rtc.h"


/******************************************************************************/
/* RTC Backup Data Register */

void RtcBackupDataRead(uint8_t * data)
{
  for (uint32_t ix=0; ix < RTC_BKP_NUMBER; ix++) {
    *(uint32_t *)data  = HAL_RTCEx_BKUPRead(&hrtc, RTC_BKP_DR0+ix);
    data += 4;
  }
}

void RtcBackupDataWrite(uint8_t * data)
{
  for (uint32_t ix=0; ix < RTC_BKP_NUMBER; ix++) {
    HAL_RTCEx_BKUPWrite(&hrtc, RTC_BKP_DR0+ix, *(uint32_t *)data);
    data += 4;
  }
}

/******************************************************************************/
/* RTC Time */

// 2017-02-12  8:02:33"
char * RtcStr(char * sz, const RtcTime_t * rtcTime)
{
  sprintf(sz, 
    "%04u-%02u-%02u %2u:%02u:%02u", 
    rtcTime->u.year, rtcTime->u.mon, rtcTime->u.mday, rtcTime->u.hour, rtcTime->u.min, rtcTime->u.sec);  
  return sz;
}

void PlfTime_DHMSM_StrConv(PlfTimeStr_t sz, PlfTime_t time)
{
  uint32_t d, h, m, s, ms;
  d = (time / (24ULL*60*60*1000));
  h = (time - (d*24ULL*60*60*1000)) / (60ULL*60*1000);
  m = (time - (d*24ULL*60*60*1000) - (h*60ULL*60*1000)) / (60ULL*1000);
  s = (time - (d*24ULL*60*60*1000) - (h*60ULL*60*1000) - (m*60ULL*1000) ) / 1000ULL;
  ms= (time - (d*24ULL*60*60*1000) - (h*60ULL*60*1000) - (m*60ULL*1000) - (s*1000ULL));
  sprintf(sz, "%u.%02u:%02u:%02u.%03u", d, h, m, s, ms);
}


int32_t RtcTimeDiff(const RtcTime_t * t1, const RtcTime_t * t2)
{
  time_t ti1, ti2;
  ti1 = RtcToUtcTime(t1);
  ti2 = RtcToUtcTime(t2);
  return ti2 - ti1;
}

void RtcGet(RtcTime_t * pRtcTime)
{
  RTC_TimeTypeDef sTime;
  RTC_DateTypeDef sDate;
  
  HAL_RTC_GetTime(&hrtc, &sTime, RTC_FORMAT_BIN);
  HAL_RTC_GetDate(&hrtc, &sDate, RTC_FORMAT_BIN);
  
  pRtcTime->u.year = 2000 + sDate.Year;
  pRtcTime->u.mon = sDate.Month;
  pRtcTime->u.mday = sDate.Date;
  pRtcTime->u.hour = sTime.Hours;
  pRtcTime->u.min = sTime.Minutes;
  pRtcTime->u.sec = sTime.Seconds;
}

bool RtcSetVal(RTC_DateTypeDef * pDate, RTC_TimeTypeDef * pTime)
{
  pTime->DayLightSaving = false;
  if (HAL_OK != HAL_RTC_SetTime(&hrtc, pTime, RTC_FORMAT_BIN))
    return false;
  if (HAL_OK != HAL_RTC_SetDate(&hrtc, pDate, RTC_FORMAT_BIN))
    return false;
  return true;
}

void RtcSet(const RtcTime_t * pRtcTime)
{
  RTC_TimeTypeDef sTime;
  RTC_DateTypeDef sDate;
  
  HAL_RTC_GetTime(&hrtc, &sTime, RTC_FORMAT_BIN);
  HAL_RTC_GetDate(&hrtc, &sDate, RTC_FORMAT_BIN);

  sTime.Hours = pRtcTime->u.hour;
  sTime.Minutes = pRtcTime->u.min;
  sTime.Seconds = pRtcTime->u.sec;
  sTime.DayLightSaving = false;
  HAL_RTC_SetTime(&hrtc, &sTime, RTC_FORMAT_BIN);
  
  sDate.Year = pRtcTime->u.year-2000;
  sDate.Month = pRtcTime->u.mon;
  sDate.Date = pRtcTime->u.mday;
  HAL_RTC_SetDate(&hrtc, &sDate, RTC_FORMAT_BIN);
  
}

bool RtcSetItems(
  int year,     
  int mon,     
  int mday,     
  int hour,     
  int min,     
  int sec)     
{
  RTC_TimeTypeDef sTime;
  RTC_DateTypeDef sDate;
 
  if (year < 2020)  return false;
  if (mon < 1)      return false;
  if (mon > 12)     return false;
  if (mday < 1)     return false;
  if (mday > 31)    return false;
  if (hour > 23)    return false;
  if (min > 59)     return false;
  if (sec > 59)     return false;
  
  HAL_RTC_GetTime(&hrtc, &sTime, RTC_FORMAT_BIN);
  HAL_RTC_GetDate(&hrtc, &sDate, RTC_FORMAT_BIN);

  sTime.Hours = hour;
  sTime.Minutes = min;
  sTime.Seconds = sec;
  sTime.DayLightSaving = false;
  HAL_RTC_SetTime(&hrtc, &sTime, RTC_FORMAT_BIN);
  
  sDate.Year = year-2000;
  sDate.Month = mon;
  sDate.Date = mday;
  HAL_RTC_SetDate(&hrtc, &sDate, RTC_FORMAT_BIN);

  return true;  
}

/******************************************************************************/
/* UTC TIME */

void UtcTimeGet(time_t * pUtcTime)
{
  struct tm tm;
  RtcTime_t rtcTime;
  RtcGet(&rtcTime);

  memset(&tm, 0, sizeof(tm));
  tm.tm_year = rtcTime.u.year - 1900;  
  tm.tm_mon  = rtcTime.u.mon-1;         // 0..11
  tm.tm_mday = rtcTime.u.mday;          // 1..31
  tm.tm_hour = rtcTime.u.hour;          // 0..23
  tm.tm_min  = rtcTime.u.min;           // 0..59
  tm.tm_sec  = rtcTime.u.sec;           // 0..59 (61)
  *pUtcTime = mktime(&tm);
}

bool UtcTimeSet(UtcTime_t utcTime)
{
  
  struct tm * ptm;
  
  //ptm = gmtime(&utcTime);
  ptm = localtime(&utcTime);
  
  if (ptm) {
    return RtcSetItems(ptm->tm_year+1900, ptm->tm_mon+1, ptm->tm_mday, ptm->tm_hour, ptm->tm_min, ptm->tm_sec);
  } else {
    return false;
  }
  
}

UtcTime_t RtcToUtcTime(const RtcTime_t * rtcTime)
{
  struct tm tm;

  memset(&tm, 0, sizeof(tm));
  tm.tm_year = rtcTime->u.year - 1900;  
  tm.tm_mon  = rtcTime->u.mon-1;  // 0..11
  tm.tm_mday = rtcTime->u.mday;
  tm.tm_hour = rtcTime->u.hour;
  tm.tm_min  = rtcTime->u.min;
  tm.tm_sec  = rtcTime->u.sec;
  return mktime(&tm);
}

time_t time(time_t * t)
{
  UtcTimeGet(t);
  return *t;
}

/*******************************************************************************/

//#define RTC_TEST

#ifdef RTC_TEST
static void RtcTest(void)
{
  char sz[80];
  RtcTime_t rtcTime1,rtcTime2;
  
  CliWriteLn("\rRTC Test");
  RtcSetItems(2022, 1, 2, 3, 4, 5);
  RtcGet(&rtcTime1);
  CliWriteLn(RtcStr(sz, &rtcTime1)); 

  RtcSet(&rtcTime1);
  RtcGet(&rtcTime2);
  CliWriteLn(RtcStr(sz, &rtcTime2)); 

  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 

  RtcSetItems(2022, 1, 2, 3, 4+1, 5);
  RtcGet(&rtcTime2);
  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 
  RtcSetItems(2022, 1, 2, 3+1, 4, 5);
  RtcGet(&rtcTime2);
  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 
  RtcSetItems(2022, 1, 2+1, 3, 4, 5);
  RtcGet(&rtcTime2);
  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 
  RtcSetItems(2022, 1+1, 2, 3, 4, 5);
  RtcGet(&rtcTime2);
  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 
  RtcSetItems(2022+1, 1, 2, 3, 4, 5);
  RtcGet(&rtcTime2);
  CliPrintf("Dif: %ld\n", RtcTimeDiff(&rtcTime1, &rtcTime2)); 
  
  
  CliWriteLn("\rUTC Test");
  UtcTime_t utcTime1, utcTime2;
  utcTime1 = RtcToUtcTime(&rtcTime1);
  UtcTimeSet(utcTime1);
  UtcTimeGet(&utcTime2);
  
  CliPrintf("Dif: %ld\n", utcTime2 - utcTime1); 
    
  
}

#endif

void HalRtcInit(void)
{
  //char sz[80];
  RtcTime_t rtcTime;

  RtcGet(&rtcTime);
  if (rtcTime.u.year < 2021) {
    RtcSetItems(2021, 1, 1, 8, 0, 0);
  }
  

#ifdef RTC_TEST
  RtcTest();
#endif
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

// 2017-12-22  8:02:33"
bool RtcSetStr(char * sz)
{
  int i, ye, mo,da, ho, mi, se;
  i = sscanf(sz, "%d-%d-%d %d:%d:%d", &ye, &mo, &da, &ho, &mi, &se);  
  if (i == 6) {
    RtcSetItems(ye, mo, da, ho, mi, se);
    return true;
  } 
  return false;
}

// "2017-12-22  8:02:33"
void RtcGetSpecStr(char * sz, RtcTime_t rtcTime)
{
  sprintf(sz, "%04u-%02u-%02u %02u:%02u:%02u", 
    rtcTime.u.year, rtcTime.u.mon, rtcTime.u.mday, rtcTime.u.hour, rtcTime.u.min, rtcTime.u.sec);  
}


char * RtcGetStr(char * sz)
{
  RtcTime_t rtcTime;
  RtcGet(&rtcTime);
  RtcGetSpecStr(sz, rtcTime);
  return sz;
}

int_fast16_t CliRtcSet(CliParam_t yyyymmdd, CliParam_t hhmmss, CliParam_t param3)
{
  char sz[80];
  strcpy(sz, (char *)yyyymmdd);
  strcat(sz, " ");
  strcat(sz, (char *)hhmmss);
  
  RtcSetStr(sz);
  RtcGetStr(sz);
  CliPrintf("RTC Date & Time set to %s\n", sz);

  return CLI_RESULT_OK;
}

/******************************************************************************/
#endif
/******************************************************************************/
