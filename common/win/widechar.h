/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef WIDECHAR_H
#define WIDECHAR_H
//---------------------------------------------------------------------------

extern int UTF16to8(
  char *s,
  const wchar_t * w,
  int maxLen);
extern int UTF8to16(
  wchar_t * w,
  const char * s,
  int maxLen);
extern char * wchar2char(
  char sz[],
  const wchar_t wsz[],
  int maxLen);
extern wchar_t * char2wchar(
  wchar_t wsz[],
  const char sz[],
  int maxLen);

#endif

