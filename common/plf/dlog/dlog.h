/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/


#ifndef PLF_DLOG_H
#define PLF_DLOG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*******************************************************************************/

#include <app/flc_def.h>

#include <shc/shc.h>

#include "hal/dflash/dflash.h"
#include "hal/rtc/rtc.h"

#include "plf/plf.h"

/*******************************************************************************/

#define DLOG_VERSION 1

#pragma pack(push, 1)  

typedef struct {
  FlcAppComponent_t     appComponent; // Used to lookup string name. If unused default to HW name
  float                 value;
} DLoggerModel_t;

typedef struct {
  DLoggerModel_t      LoggedComponents[HW_COMPONENT_Last];
  RtcTime_t           sessionId;       // 8 byte
  ShcState_t          state;
  uint8_t             hwConfigVersion; // FLC_CFG_HW_VERSION
  uint8_t             dlogVersion;     // DLOG_VERSION
  uint8_t             reserved[2];
}                     DataLog_t;

typedef struct {
    RtcTime_t         time;            // 8 byte    
    DataLog_t         data;
} CompleteDLog_t;
static_assert(sizeof(CompleteDLog_t) <= DLOG_ENTRY_SIZE, "DLOG_ENTRY_SIZE too small");

typedef union {
  CompleteDLog_t        log;
  uint8_t               all[DLOG_ENTRY_SIZE];
}                       DLogEntry_t;

#pragma pack(pop)  


/*******************************************************************************/

// look back and find the entry "first older then time"
extern bool DataLogFindFirst(RtcTime_t time, DLogEntry_t * logEntry);
extern bool DataLogFindOlder(DLogEntry_t * logEntry);
extern bool DataLogFindNewer(DLogEntry_t * logEntry);
extern bool DataLogFindNewest(DLogEntry_t * logEntry);

extern void DataLogAdd(DataLog_t * dataLog);
extern void DataLogClear(void);

extern void DLogInit(void);	

/*******************************************************************************/
  
#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* MWR_DLOG_H */
