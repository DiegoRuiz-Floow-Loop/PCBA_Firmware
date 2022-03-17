/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef TRC_H
#define TRC_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************/


#include <stdarg.h>
  
#include "hal/hal.h"
#include "plf/plf.h"
#include "plf/cfg/trc_cfg.h"

/******************************************************************************/
#ifdef TRC_ENABLE

#include "plf/cli/cli.h"
CLI_DECLARE_SUBTEST(trc)        /* in trc.c */

#define TRACE_ERROR(D, A) \
  TrcTrace(D, TRC_TL_FATAL, "ERROR: " __FILE__ ", " _STRIZE(__LINE__) ", " #A)
#define TRACE_ASSERT(D, A) \
  (A) ? (void)0 : TrcTrace(D, TRC_TL_FATAL, "Assertion failed: " __FILE__ ":" _STRIZE(__LINE__) ", " #A)

#define TRACE(TraceArea, TraceLevel, sz) \
  {\
    static const char xx[] = sz;\
    TrcTrace(TraceArea, TraceLevel, xx);\
  } 

#define TRACE_S2(TraceArea, TraceLevel, sz, sz2) \
  {\
    static const char xx[] = sz;\
    TrcTraceS2(TraceArea, TraceLevel, xx, sz2);\
  } 

#define TRACE_S3(TraceArea, TraceLevel, sz, sz2, sz3) \
  {\
    static const char xx[] = sz;\
    TrcTraceS3(TraceArea, TraceLevel, xx, sz2, sz3);\
  } 

#define TRACE_VA(TraceArea, TraceLevel, ...) {\
    TrcTraceArg(TraceArea, TraceLevel, __VA_ARGS__);\
}
//#define TRACE_VA(TraceArea, TraceLevel, ...) {\
//  if (IS_RUNNING_MAIN()) {\
//    TrcTraceArg(TraceArea, TraceLevel, __VA_ARGS__);\
//  } else{\
//    TrcTraceArgX(TraceArea, TraceLevel, __VA_ARGS__);\
//  }\
//}

#define TRACE_BLK(TraceArea, szInfo, data, cnt) \
{ \
  if (IS_RUNNING_MAIN()) {\
    static const char  szInfo2[] = szInfo;\
    TrcTraceBlk(TraceArea, szInfo2, data, cnt); \
  }\
}
#define TRACE_BLK_FATAL(TraceArea, szInfo, data, cnt) \
{ \
  if (IS_RUNNING_MAIN()) {\
    static const char  szInfo2[] = szInfo;\
    TrcTraceBlkFatal(TraceArea, szInfo2, data, cnt); \
  }\
}

typedef struct {
  uint8_t               areas[TRC_TA_Last];
  bool                  trcEnabled;
}                       TrcConfig_t;


extern bool TrcEnabled(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel);
extern void TrcTrace(
  const TraceArea_t           traceArea,    /* see enum above */
  const TraceLevel_t          traceLevel,   /* see enum above */
  const char                  *szInfo);

extern void TrcTraceS2(
  const TraceArea_t           traceArea,    /* see enum above */
  const TraceLevel_t          traceLevel,   /* see enum above */
  const char                  *szInfo,
  const char                  *szInfo2);

extern void TrcTraceS3(
  const TraceArea_t           traceArea,    /* see enum above */
  const TraceLevel_t          traceLevel,   /* see enum above */
  const char                  *szInfo,
  const char                  *szInfo2,
  const char                  *szInfo3);

extern void TrcTraceArg(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  char                        * format,
  ...);
// Only usinf format, not the arguments...  
extern void TrcTraceArgX(
  const TraceArea_t           traceArea,
  const TraceLevel_t          traceLevel,
  char                        *format,
  ...);


void TrcTraceBlk(
  const TraceArea_t           traceArea,    /* see enum above */
  const char                  *szInfo,
  const uint8_t               *data,
  uint16_t                    cnt);

void TrcTraceBlkFatal(
  const TraceArea_t           traceArea,    /* see enum above */
  const char                  *szInfo,
  const uint8_t               *data,
  uint16_t                    cnt);
  
typedef void (*TrcOutputFunction_t)(const char * st);  /**< Function pointer for trace output handler. */
extern TrcOutputFunction_t TrcPrint;

extern void TrcPortLock(void);
extern void TrcPortUnLock(void);
  
extern void TrcEnable(bool onOff);
extern void TrcToggle(void);
extern void TrcGetEnable(bool * onOff);

extern void TrcNvmInit(void);

extern void TrcTraceDefine(
  const TraceArea_t     traceArea,    /* see enum above */
  const TraceLevel_t    traceLevel,   /* see enum above */
  const bool          on);

extern void TrcTraceDefineLevelMask(
  TraceArea_t           traceArea,    /* see enum above */
  uint8_t               traceLevelMask);

extern void TrcTraceDefineAreaMask(
  TraceLevel_t          traceLevel,    /* see enum above */
  uint8_t               traceAreaMask);
	

TrcOutputFunction_t TrcOutputDefine(
  TrcOutputFunction_t p);

CLI_DECLARE_SUBTEST(plf_trc)        /* in trc.c */

#else

#define TRACE_ERROR(D, A) (void)0
#define TRACE_ASSERT(D, A) (void)0
#define TRACE(TraceArea, TraceLevel, ...) (void)0
#define TRACE_S2(TraceArea, TraceLevel, sz, sz2) (void)0
#define TRACE_VA(TraceArea, TraceLevel, ...) (void)0
#define TRACE_BLK(TraceArea, szInfo, data, cnt) (void)0

//#define TrcTraceArg(a,l,f, ...) (void)0

#endif
/******************************************************************************/


//#define _STRIZE(x)        _VAL(x)
//#define _VAL(x)            #x

#undef ASSERT
#define ASSERT(a) TRACE_ASSERT(TRC_TA_PLF, a)


/******************************************************************************/



extern void TrcInit(void);

/******************************************************************************/


#ifdef __cplusplus
}
#endif /* #ifdef __cplusplus */

#endif /* #ifndef TRC_H */
