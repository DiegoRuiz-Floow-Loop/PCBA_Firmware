/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

 // testing the development mode --> DR

 
 

#include <app/flc_def.h>
#include <app/flc_cfg.h>

#include <plf/plf.h>
#include <plf/nvm/nvm.h>
#include <plf/trc/trc.h>
#include <plf/dlog/dlog.h>
#include <plf/evos/evos.h>
#include <plf/elog/elog.h>

#include <shc/uv_lamp.h>
#include <shc/valve.h>
#include <shc/pump.h>

#include <sup/float_switch.h>
#include <sup/water_sensor.h>
#include <sup/log_sensors.h>
#include <sup/pos_sensor.h>
#include "sup/user_button.h"
#include <hal/hal.h>

#include "dlogger.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define LOGGER_TICK_MSEC  (200u)

// Time resolution is limited to seconds instead of milliseconds because RTC 
// resolution is in seconds.
#define DEFAULT_LOGGING_INTERVAL_SEC ( 1u)
#define MIN_LOGGER_INTERVAL_SEC      ( 1u)
#define MAX_LOGGER_INTERVAL_SEC      (60u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static const FlcComponentMapping_t * configTable;

static EvosEventHandle_t loggerHandle = EVOS_UNINITIALIZED_HANDLE;

static ShcState_t currentState = SHC_IDLE;
static RtcTime_t sessionId;
static RtcTime_t previousLogTime;

static DataLog_t localLog;

static DLogger_t cfg;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static void LoggerEvent(const EvosEventParam_t param);
static void FillLog(DataLog_t * log);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void DLoggerInit(const FlcComponentMapping_t * const cfgMap) 
{
  configTable = cfgMap;
  ConfigLoad();
  loggerHandle = EvosEventRegister(LoggerEvent, "LoggerEvent");
  EvosEventSetAndReload(loggerHandle, 0, LOGGER_TICK_MSEC, 0);
  
  // Setup struct variable with a valid time. Struct all zeroes doesn't work.
  RtcGet(&previousLogTime);
}

void DLoggerStateChanged(const ShcState_t shcState)
{  
  // We are already in idle and we shouldn't log anything
  if(shcState == SHC_IDLE && currentState == SHC_IDLE) {
    return;
  }
  
  // We are leaving idle and starting a new session
  if(currentState == SHC_IDLE) {
    RtcGet(&sessionId);
  }
    
  currentState = shcState;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static void LoggerEvent(const EvosEventParam_t param)
{
  if (currentState == SHC_IDLE) {
    return;
  }
  
  RtcTime_t now;
  RtcGet(&now);
  
  if (RtcTimeDiff(&previousLogTime, &now) >= cfg.loggingIntervalSec) {
    previousLogTime = now;
    
    FillLog(&localLog);
    DataLogAdd(&localLog);
  }
}

static void FillLog(DataLog_t * const log)
{
  
  log->state = currentState;
  log->hwConfigVersion = FLC_CFG_HW_VERSION;
  log->dlogVersion = DLOG_VERSION;
  log->sessionId = sessionId;
  
  for(uint_fast8_t hwIdx = 0; hwIdx < HW_COMPONENT_Last; hwIdx++) {
    float value = 0.0f;
    const FlcComponentMapping_t cmpMap = configTable[hwIdx];
    
    switch(cmpMap.type) {
      case FLOAT_SWITCH:
        value = FloatSwitchRead(hwIdx);
        break;
      case VALVE:
        value = (float)ValveGetDutyCycle(hwIdx);        
        break;
      case UV_LAMP_BALLAST:
        value = UvLampIsOn();
        break;
      case PUMP:
        value = PumpGetPower(hwIdx);
        break;
      case LOG_SENSOR:
        LogSensorGetValue(hwIdx, &value);
        break;
      case POS_SENSOR:
        value = PosSensorGetPermil(hwIdx);
        break;
      case WATER_SENSOR:
        value = WaterSensorRead(hwIdx);
        break;
      case BUTTON:
        value = UserButtonRead(hwIdx);
        break;
      case COMPONENT_TYPE_Last:
      default: {
        static char logSz[64];
        snprintf(logSz, sizeof(logSz), "A nonexistent component was logged. Type: %u\n", cmpMap.type);
        
        TRACE_VA(TRC_TA_CIF, TRC_TL_ERROR, "%s", logSz);
        EVENT_LOG_ADD_S(logSz);
        continue;
      }
    }
    
    log->LoggedComponents[hwIdx].appComponent = cmpMap.component;
    log->LoggedComponents[hwIdx].value = value;
  }  
}

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.dlogger), (uint8_t *)&cfg, sizeof(DLogger_t));
    if (cfg.crc != CalcCrc16(offsetof(DLogger_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(DLogger_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.dlogger), (uint8_t *)&cfg, sizeof(DLogger_t));
}

static void ConfigSetDefault(void)
{
  cfg.loggingIntervalSec = DEFAULT_LOGGING_INTERVAL_SEC;
  ConfigSave();
}

/*******************************************************************************
 * CLI
 ******************************************************************************/

#ifdef CLI_ENABLE

static DLogEntry_t localLogEntry;
static int_fast16_t CliShowLogs(CliParam_t logCount, CliParam_t param2, CliParam_t param3) {
  RtcTime_t currentTime;
  RtcGet(&currentTime);
  
  if (!DataLogFindFirst(currentTime, &localLogEntry)) {
    CliPrintf("No logs found\n");
    return CLI_RESULT_OK;
  }
  
  for(uint_fast16_t idx = 0; idx < logCount; idx++) {
    DataLog_t * data = &localLogEntry.log.data;
    
    CliPrintf("SessionId: %llu Time: %llu State: %s HW Config: %u dlog version: %u\n", 
              data->sessionId.all,
              localLogEntry.log.time.all,
              ShcGetStateName(data->state),
              data->hwConfigVersion,
              data->dlogVersion);
    
    for (uint_fast8_t hwIdx = 0; hwIdx < HW_COMPONENT_Last; hwIdx++) {      
      CliPrintf("%0.2f\n",
                (double)(data->LoggedComponents[hwIdx].value));
    }
    
    if(!DataLogFindOlder(&localLogEntry)) {
      break;
    }
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetInterval(CliParam_t intervalSec, const CliParam_t param2, const CliParam_t param3)
{
  intervalSec = MIN_VAL(intervalSec, MAX_LOGGER_INTERVAL_SEC);
  intervalSec = MAX_VAL(intervalSec, MIN_LOGGER_INTERVAL_SEC);
  
  cfg.loggingIntervalSec = intervalSec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliAddLogs(CliParam_t logsToAdd, CliParam_t param2, CliParam_t param3) {
  RtcTime_t tmpSessionId;
  RtcGet(&tmpSessionId);
  
  for (uint_fast8_t logIdx = 0; logIdx < logsToAdd; logIdx++) {
    FillLog(&localLog);
    localLog.sessionId = tmpSessionId;
    localLog.state = SHC_SHOWER_LOOP;
    DataLogAdd(&localLog);
  }
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(data_logger)
  CLI_ENTRY1( "show",   "Show latest logs [no:1..max]", CliShowLogs, CLI_PARAM_INT)
  CLI_ENTRY1( "add",    "Add random session data with n logs [no:1..max]", CliAddLogs, CLI_PARAM_INT)
  CLI_ENTRY1( "int",    "Set logging interval [seconds 1-60]", CliSetInterval, CLI_PARAM_INT)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(data_logger)

#endif

/******************************************************************************/
