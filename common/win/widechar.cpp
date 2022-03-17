/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "win/widechar.h"

typedef unsigned int   uint;
typedef unsigned short word;
typedef unsigned char  byte;



int UTF16to8(char *s , const wchar_t * w, int maxLen)
{
  uint  c;
  word* p = (word*)w;
  byte* q = (byte*)s;
  byte* q0 = q;
  while( 1 ) {
    c = *p++;
    if( c==0 )
      break;
    if (maxLen-- <= 0)
      break;
    if( c<0x080 )
      *q++ = c;
    else
      if( c<0x800 )
        *q++ = 0xC0+(c>>6), *q++ = 0x80+(c&63);
      else
        *q++ = 0xE0+(c>>12), *q++ = 0x80+((c>>6)&63), *q++ = 0x80+(c&63);
  }
  *q = 0;
  return q-q0;
}

int UTF8to16(wchar_t * w, const char * s, int maxLen)
{
  uint  cache, wait, c;
  byte * p = (byte*)s;
  word * q = (word*)w; word* q0 = q;
  while(1) {
    c = *p++;
    if( c==0 )
      break;
    if (maxLen-- <= 0)
      break;
    if( c<0x80 )
      cache=c,wait=0;
    else
      if( (c>=0xC0) && (c<=0xE0) )
        cache=c&31, wait=1;
      else
        if( (c>=0xE0) )
          cache=c&15, wait=2;
        else
          if( wait )
            (cache<<=6) += c&63, wait--;
    if( wait==0 )
      *q++=cache;
  }
  *q = 0;
  return q-q0;
}


char * wchar2char(char sz[], const wchar_t wsz[], int maxLen)
{
  (void)UTF16to8(sz, wsz, maxLen);
  return sz;
}

wchar_t * char2wchar(wchar_t wsz[], const char sz[], int maxLen)
{
  (void)UTF8to16(wsz, sz, maxLen);
  return wsz;
}

