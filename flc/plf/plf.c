/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef __MODULE__
#define __MODULE__ "plf"
#endif

#include <string.h>
#include <stdio.h>
#include <time.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"
#if (PLF_OS==PLF_OS_RTOS)
#include "plf/rtos/rtos.h"
#endif
#include "plf/sio/sio.h"
#include "plf/trc/trc.h"
#include "plf/nvm/nvm.h"
#include "plf/led/led.h"
#include "plf/key/key.h"

#if !defined(BOOTLOADER)
#include "plf/heap/heapx.h"
#include "plf/dlog/dlog.h"
#include "plf/elog/elog.h"
#endif

/******************************************************************************/
/* System Tick (Time) */

char * PlfTime_DHMSM_Str(char * sz, PlfTime_t time)
{
  uint_fast32_t d, h, m, s, ms;
  d = (time / (24*60*60*1000));
  h = (time - (d*24*60*60*1000)) / (60*60*1000);
  m = (time - (d*24*60*60*1000) - (h*60*60*1000)) / (60*1000);
  s = (time - (d*24*60*60*1000) - (h*60*60*1000) - (m*60*1000) ) / 1000;
  ms= (time - (d*24*60*60*1000) - (h*60*60*1000) - (m*60*1000) - (s*1000));
  sprintf(sz, "%u.%02u:%02u:%02u.%03u", d, h, m, s, ms);
  return sz;
}

void PlfTime_DHMSM_StrGet(char * sz)
{
  PlfTime_t time;
  PlfTimeMsGet(&time);
  PlfTime_DHMSM_Str(sz, time);
}


char * PlfTime_DHMS_Str(char * sz, PlfTime_t time)
{
  uint_fast32_t d, h, m, s;
  d = (time / (24*60*60*1000));
  h = (time - (d*24*60*60*1000)) / (60*60*1000);
  m = (time - (d*24*60*60*1000) - (h*60*60*1000)) / (60*1000);
  s = (time - (d*24*60*60*1000) - (h*60*60*1000) - (m*60*1000) ) / 1000;
  sprintf(sz, "%u.%02u:%02u:%02u", d, h, m, s);
  return sz;
}

void PlfTime_DHMS_StrGet(char * sz)
{
  PlfTime_t time;
  PlfTimeMsGet(&time);
  PlfTime_DHMS_Str(sz, time);
}

#if (PLF_OS != PLF_OS_WINDOWS) /* see project file */

void PlfTimeMsGet(PlfTime_t * currentTime)
{
#if (PLF_OS==PLF_OS_RTOS)
    *currentTime = RTOS_GET_TICK_COUNT();
#else
  *currentTime = HAL_GetTick();
#endif
}

#endif

void PlfDelay(int32_t dTim)
{
#if (PLF_OS==PLF_OS_RTOS)
  RTOS_TASK_DELAY(dTim);
#else
  PlfTime_t               t1, t2;
  PlfTimeMsGet(&t1);
  do {
    HalWdtFeed();
		PlfTimeMsGet(&t2);
  } while (TIME_DIF(t2, t1) < dTim); 
#endif
}


/******************************************************************************/
/* Utility Functions */

// strLen == 0 no leading '0', else '0' is added excl. '-'

char * DecToNum(LongBuf_t buffer, int32_t val, uint8_t strLen)
{
  int ix = LONG_BUF_SIZE-1;
  bool neg;
  
  neg = (val < 0);
  if (neg)
    val *= -1;

  buffer[ix--] = 0;
  
  if (val == 0) {
    buffer[ix--] = '0';
  } else {
    while (val > 0) {
      buffer[ix--] = '0' + (val % 10);
      val /= 10;
    }
  }
  if (strLen > 0) {
    while (ix > (LONG_BUF_SIZE-2-strLen))
      buffer[ix--] = '0';
  }
  if (neg) {
    buffer[ix--] = '-';
  }
  return &buffer[1+ix];
}

char * itoa(int i)
{
  static char xx[12];
  sprintf(xx, "%d", i);
  return xx;
}

bool CmpStr(char * s1, char * s2)
{
  int16_t i;
  int16_t l = strlen(s2);
  for (i=0; i<l; i++) {
    if (*s1++ != *s2++)
      return false;
  }
  return true;
}

// s1 >= s2
bool StrGE(const char * s1, const char * s2)
{
	int l = strlen(s2);
	int i = 0;
	for (;;) {
		if (*s1 == 0) break;
		if (*s1 == *s2) {
			i++;
		} else {
			break;
		}
		s1++;
		s2++;
  }
  return(i >= l);
}

uint8_t HexToVal(const char * hex)
{
	uint8_t res = 0;
	int i;
	for (i=0; i<2; i++) {
		res <<= 4;
		if ((*hex >= '0') && (*hex  <= '9')) {
			res += (*hex - '0');
		} else if ((*hex >= 'a') && (*hex  <= 'f')) {
			res += (*hex - 'a' + 10);
		} else if ((*hex >= 'A') && (*hex  <= 'F')) {
			res += (*hex - 'A' + 10);
		}
		hex++;
	}
	return res;
}

void HexToData(uint8_t * data, int * lenData, const char * hexText, int maxLen)
{
  int y = 0;
  int len = strlen(hexText);
  while (len > 0) {
    data[y] = HexToVal(hexText);
    len -= 2;
    hexText += 2;
    y += 1;
    if (maxLen-- < 0) 
      return;
  }
  //data[y] = 0;
  *lenData = y;
}


static const char hexChar[] = "0123456789ABCDEF";


void DataToHex(char * hexText, int * lenHex, const uint8_t * data, const int lenData)
{
  int i, j;
  char * hex = hexText;
  for (i = 0, j=0; i < lenData; i++) {
    uint8_t c = *data++;
    hex[j++] = hexChar[c >> 4];
    hex[j++] = hexChar[c & 0xF];
  }
  hex[j] = 0;
  *lenHex = 2*lenData;
}

char * Hex2Str(uint32_t x, int minWidth, char * s)
{
  *--s = 0;
  if (!x) {
    *--s = '0';
    while (minWidth-- > 1) {
      *--s = '0';
    }
  } else {
    for (; x; x/=16) {
      *--s = hexChar[x>>4];
      minWidth--;
    }
    while (minWidth-- > 0) {
      *--s = '0';
    }
  }
  *--s = 'x';
  *--s = '0';
  return s;
}

// Dec2Str(number, &buf[sizeof(buf)])
char * Dec2Str(uint32_t x, char * s)
{
  *--s = 0;
  if (!x) 
    *--s = '0';
  for (; x; x/=10) 
  *--s = hexChar[x%10];
  return s;
}

/******************************************************************************/

void __aeabi_assert(const char * p1, const char * p2, int i)
{
  for (;;) ;
}


void PlfInit(void)
{
#if defined(BOOTLOADER)
  SioInit();
#else
  HeapxInit();

  LedInit();
  
  LedOnlevelSet(LED_HEARTBEAT, 1);
  LedOnlevelSet(LED_RGB_R, 1);
  LedOnlevelSet(LED_RGB_G, 1);
  LedOnlevelSet(LED_RGB_B, 1);
  LedRGB(LRGB_BLACK);

  KeyInit();
  NvmInit(); 
#ifdef TRC_ENABLE
  TrcInit();
  if (nvmState != NVM_OK) {
    TrcEnable(true);
    //TrcTraceDefineLevelMask(TRC_TA_PLF, 0xFF);
    TrcTraceDefineAreaMask(TRC_TL_FATAL, 0xFF);
  }
  ELogInit();
  DLogInit();
#endif
  
#endif
 }

