/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_NVM_CFG_H
#define PLF_NVM_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*******************************************************************************/

#include "hal/rtc/rtc.h"

#include "plf/plf_common.h"
#include "plf/crc/crc16.h"

#define NVM_ENABLE  /* Define this macro if Non-Volatile Memory is available for configuration. */
#define NVM_ACCESS_ADDRESS    (1)
#define NVM_ACCESS_ID      (2)


/*******************************************************************************/
#ifdef NVM_ENABLE
#define NVM_ACCESS  NVM_ACCESS_ADDRESS

/*******************************************************************************/
#if (NVM_ACCESS==NVM_ACCESS_ID)

typedef enum {
  NVM_ID_MARK_BEGIN,
  NVM_ID_TRC,
  NVM_ID_SIF,
  NVM_ID_XYZ,

 // NVM_ID_OPQ,
  NVM_ID_Last /* never use as ID */
}                       NvmId_t;

/*******************************************************************************/
#else

#include <app/flc_def.h>

typedef struct {
   bool                   banner;
}                         Sif_t;

typedef uint32_t          DLog_t;
typedef uint32_t          ELog_t;

#define MODEM_MODEL_LENGTH  40
#define MODEM_REV_LENGTH    40
#define MODEM_IMEI_LENGTH   20

typedef struct {
  char                    model[MODEM_MODEL_LENGTH];
  char                    rev[MODEM_REV_LENGTH];
  char                    imei[MODEM_IMEI_LENGTH];  
  Crc16_t                 crc;
}                         Modem_t;

typedef struct {
  uint32_t                timingMsec[SHC_TIMING_Last];
  Crc16_t                 crc;
}                         Shc_t; 

typedef struct {
  uint16_t                positionLimitMaxPermil;
  uint16_t                positionLimitMinPermil;
  uint16_t                positionHeadMaxPermil;
  uint16_t                positionHeadOnPermil;
  uint16_t                positionHeadOffPermil;
  uint16_t                positionHandMaxPermil;
  uint16_t                positionHandOnPermil;
  uint16_t                positionHandOffPermil;
  uint16_t                powerMaxPermil;
  uint16_t                powerMinPermil;
  Crc16_t                 crc;
}                         FlowKnob_t;

typedef struct {
  uint32_t                timingMsec[HOT_SUPPLY_TIMING_Last];
  Crc16_t                 crc;
}                         HotSupply_t;

typedef struct {
  uint32_t                timingMsec[COLD_SUPPLY_TIMING_Last];
  Crc16_t                 crc;
}                         ColdSupply_t;

typedef struct {
  uint32_t                timingMsec[PRIME_PUMP_TIMING_Last];
  uint32_t                powerChangePermil;
  Crc16_t                 crc;
}                         PrimePump_t;

typedef struct {
  float                   levelTriggerPct[WATER_SENSOR_Last - WATER_SENSOR_First];
  Crc16_t                 crc;
}                         WaterSensor_t;

typedef struct {
  RtcTime_t               lastUpload;
  Crc16_t                 crc;
}                         Cif_t; 

typedef struct {
  uint16_t                loggingIntervalSec;
  Crc16_t                 crc;
}                         DLogger_t; 

typedef struct {
  uint16_t                onTimeMsec;
  uint16_t                offTimeMsec;
  uint16_t                retries;
  Crc16_t                 crc;
}                         UvLamp_t;

// All application settings are grouped together so they can easily be reset if 
// required by the application.
typedef struct {
  Shc_t                   shc;
  FlowKnob_t              flowKnob;
  HotSupply_t             hotSupply;
  ColdSupply_t            coldSupply;
  PrimePump_t             primePump;
  WaterSensor_t           waterSensor;
  Cif_t                   cif;
  DLogger_t               dlogger;
  UvLamp_t                uvLamp;
}                         App_t;

typedef struct
{
  Sif_t                   sif;
  DLog_t                  dLog;
  ELog_t                  eLog;
  Modem_t                 modem;
  App_t                   app;
}                         Appl_t;

#endif /* NVM_USE_ID */
#endif /* NVM_ENABLE */

/*******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* NVM_CFG_H */
