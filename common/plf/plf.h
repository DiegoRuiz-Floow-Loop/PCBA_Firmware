/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_H
#define PLF_H

#include "plf/plf_common.h"

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************/

#define PLF_VENDOR_NAME       "Flow Loop"

#define PLF_PRODUCT_NAME      "F3.0"

#define PLF_VERSION_MAJOR     1  
#define PLF_VERSION_MINOR     6

/******************************************************************************/
/* text */

#if (PLF_OS != PLF_OS_WINDOWS)
#if !defined(__SES_ARM)
//  extern char * itoa(int i);
#endif
#endif



/******************************************************************************/
/* System Tick (Time) */

#define PLF_TIME_1SEC   (1000)

#if 0
  typedef uint32_t	PlfTime_t;
  typedef uint32_t	PlfTick_t;

  #define PRINTF_PLFTIME_INT    "%d"
  #define PRINTF_PLFTIME_UINT   "%u"
  #define TIME_DIF(t2, t1) ((int32_t)((PlfTime_t)t2 - (PlfTime_t)t1))
#else 
  typedef uint64_t	PlfTime_t;
  typedef uint64_t	PlfTick_t;

  #define PRINTF_PLFTIME_INT    "%lld"
  #define PRINTF_PLFTIME_UINT   "%llu"
  #define TIME_DIF(t2, t1) ((int64_t)((PlfTime_t)t2 - (PlfTime_t)t1))
#endif
#define TIME_UP(t2, t1) (TIME_DIF(t2, t1) >= 0)	

extern PlfTime_t PlfTime(void);
extern void PlfTimeMsGet(PlfTime_t * currentTime);

extern void PlfTime_DHMSM_StrGet(char * sz);
extern char * PlfTime_DHMSM_Str(char * sz, PlfTime_t msec);

extern void PlfTime_DHMS_StrGet(char * sz);
extern char * PlfTime_DHMS_Str(char * sz, PlfTime_t mec);
extern char * PlfTime_DHMS_from_sec_Str(char * sz, PlfTime_t sec);


extern void HalDelayUs(uint32_t uSec);

extern void PlfDelay(int32_t dTim);

/******************************************************************************/
/* Utility Functions */
/******************************************************************************/
  
// s1 >= s2
extern bool StrGE(const char * s1, const char * s2);

extern uint8_t HexToVal(const char * hex);

extern void DataToHex(char * hexText, int * lenHex, const uint8_t * data, const int lenData);
extern void HexToData(uint8_t * data, int * lenData, const char * hexText, int maxLen);
  
#define LONG_BUF_SIZE (12)  // "-1111222333"
typedef char LongBuf_t[LONG_BUF_SIZE];
extern char * DecToNum(LongBuf_t buffer, int32_t val, uint8_t strLen);
  
extern char * Dec2Str(uint32_t x, char * s);

extern char * Hex2Str(uint32_t x, int minWidth, char * s);



/****************************************************************************/

extern void PlfInit(void);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // PLF_H

