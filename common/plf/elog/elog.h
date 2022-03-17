/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/


#ifndef PLF_ELOG_H
#define PLF_ELOG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*******************************************************************************/

#include <stdarg.h>
  
#include "plf/plf.h"
#include "hal/dflash/dflash.h"
#include "hal/rtc/rtc.h"


/*******************************************************************************/
  
#pragma pack(push, 1)  

/* Only the filename without path are saved: "c:\\a\\b\\c\\file.c" -> "file.c" */
typedef char            ELogFileName_t[16];

                        //  128 - 8 - 8 - 16 - 2 == 94 
#define ELOG_DATA_SIZE  (ELOG_ENTRY_SIZE - sizeof(RtcTime_t) - sizeof(PlfTime_t) - sizeof(ELogFileName_t) - sizeof(uint16_t))  

typedef uint8_t         ELogData_t[ELOG_DATA_SIZE]; // 94 byte

typedef struct {
  RtcTime_t             time;
	PlfTime_t							tick;
  ELogFileName_t        fileName;   // __MODULE__
  uint16_t              fileLine;  
  ELogData_t            dat;
}                       ELogEntry_t;

#pragma pack(pop)  


/*******************************************************************************/

extern const char * EventLogGet(int * nextNo);

/*******************************************************************************/

extern bool EventLogAddData(
  const ELogFileName_t  fileName,   // use: __MODULE__
  int                   fileLine,
  const ELogData_t      data);      // length == ELOG_DATA_SIZE 

extern bool EventLogAddS3(
  const ELogFileName_t  fileName,   // use: __MODULE__
  int                   fileLine,
  const char            * text,
  const char            * text2,
  const char            * text3);


#if (PLF_OS!=PLF_OS_WINDOWS)

#define EVENT_LOG_ADD_S(text)                  EventLogAddS3(__FILE_NAME__, __LINE__, text, NULL, NULL)
#define EVENT_LOG_ADD_S2(text, text2)          EventLogAddS3(__FILE_NAME__, __LINE__, text, text2, NULL)
#define EVENT_LOG_ADD_S3(text, text2, text3)   EventLogAddS3(__FILE_NAME__, __LINE__, text, text2, text3)
//#define EVENT_LOG_ADD_VA(format, ...)          EventLogAddVA(__MODULE__, __LINE__, format, __VA_ARGS__)


#else

#define EVENT_LOG_ADD_S2(text, text2)    ((void)0)
#define EVENT_LOG_ADD_VA(format, ...)   ((void)0)

#endif



extern void ELogInit(void);	

/*******************************************************************************/
  
#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* MW_ELOG_H */
