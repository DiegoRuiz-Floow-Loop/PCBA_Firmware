/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/util/widechar.h"

char * wchar2char(
	char sz[],
	const wchar_t wsz[],
  uint16_t maxLen)
{
	int i=0;
	do {
    if (i>= maxLen) break;
		sz[i] = (char)wsz[i];
	} while (wsz[i++] != 0);
	return sz;
}

wchar_t * char2wchar(
	wchar_t wsz[],
	const char sz[],
  uint16_t maxLen)
{
	int i=0;
	do {
    if (i>= maxLen) break;
		wsz[i] = (wchar_t)sz[i];
	} while (sz[i++] != 0);
	return wsz;
}

int wstrlen(wchar_t * ws)
{
  int i=0;
  for (;;) {
    if (*ws++ == 0) return i;
    i++;
  }
}

