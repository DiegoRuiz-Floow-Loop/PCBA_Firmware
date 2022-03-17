/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#ifndef FMX
#include <sys\timeb.h>
#endif

#include "plf/plf.h"


/******************************************************************************/

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

uint8_t HexToVal(char * hex)
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

void HexToData(uint8_t * data, int * lenData, char * hexText, int maxLen)
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

/******************************************************************************/

#define LONG_BUF_SIZE (12)  // "-1111222333"
typedef char LongBuf_t[LONG_BUF_SIZE];

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

void StrCatChar(char * sz, char ch)
{
  int_fast8_t l = strlen(sz);
  sz[l++] = ch;
  sz[l]   = 0;
}

/******************************************************************************/

char * PlfTime_DHMSM_Str(char * sz, PlfTime_t msec)
{
	PlfTime_t d, h, m, s, ms;
	LongBuf_t buf;
	char * c;
	d = (msec / (24ULL*60*60*1000));
	h = (msec - (d*24ULL*60*60*1000)) / (60*60*1000);
	m = (msec - (d*24ULL*60*60*1000) - (h*60*60*1000)) / (60*1000);
	s = (msec - (d*24ULL*60*60*1000) - (h*60*60*1000) - (m*60*1000) ) / 1000;
	ms= (msec - (d*24ULL*60*60*1000) - (h*60*60*1000) - (m*60*1000) - (s*1000));

	strcpy(sz, DecToNum(buf, d, 0));
	StrCatChar(sz, '.');
	c = DecToNum(buf, h, 2);
	strcat(sz, c);
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, m, 2));
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, s, 2));
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, ms, 3));
	return sz;
}


char * PlfTime_DHMS_from_sec_Str(char * sz, PlfTime_t sec)
{
	uint32_t d, h, m, s;
	LongBuf_t buf;
	char * c;
	d = (sec / (24ULL*60*60));
	h = (sec - (d*24ULL*60*60)) / (60*60);
	m = (sec - (d*24ULL*60*60)  - (h*60*60)) / 60;
	s = (sec - (d*24ULL*60*60)  - (h*60*60) - (m*60));

	strcpy(sz, DecToNum(buf, d, 0));
	StrCatChar(sz, '.');
	c = DecToNum(buf, h, 2);
	strcat(sz, c);
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, m, 2));
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, s, 2));
	return sz;
}

char * PlfTime_DHMS_Str(char * sz, PlfTime_t msec)
{
	PlfTime_t d, h, m, s;
	LongBuf_t buf;
	char * c;
	d = (msec / (24ULL*60*60*1000));
	h = (msec - (d*24ULL*60*60*1000)) / (60*60*1000);
	m = (msec - (d*24ULL*60*60*1000) - (h*60*60*1000)) / (60*1000);
	s = (msec - (d*24ULL*60*60*1000) - (h*60*60*1000) - (m*60*1000) ) / 1000;

	strcpy(sz, DecToNum(buf, d, 0));
	StrCatChar(sz, '.');
	c = DecToNum(buf, h, 2);
	strcat(sz, c);
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, m, 2));
	StrCatChar(sz, ':');
	strcat(sz, DecToNum(buf, s, 2));
	return sz;
}


/******************************************************************************/

#ifndef FMX

volatile PlfTime_t bootTime;

void PlfTimeMsGet(PlfTime_t * pTime)
{
  struct {
	struct timeb    timebNow;
	  char xx[8];
  } xx;
  long long t;
  ftime(&xx.timebNow);
  t = ((xx.timebNow.time*1000UL) + xx.timebNow.millitm) - bootTime;
	*pTime = (PlfTime_t)t;
}

/******************************************************************************/

void PlfInit(void)
{
  bootTime = 0;
  PlfTimeMsGet((PlfTime_t *)&bootTime);
}

/******************************************************************************/
#endif



