/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef WIDECHAR_H
#define WIDECHAR_H
//---------------------------------------------------------------------------

#include <stdint.h>
#include <stdlib.h>
#include <wchar.h>


extern char * wchar2char(
	char sz[],
	const wchar_t wsz[],
  uint16_t maxLen);

extern wchar_t * char2wchar(
	wchar_t wsz[],
	const char sz[],
  uint16_t maxLen);
  
  
#define wsizeof(xx)  (sizeof(xx)/sizeof(xx[0]))
  
extern int wstrlen(wchar_t * ws);


  
#endif
