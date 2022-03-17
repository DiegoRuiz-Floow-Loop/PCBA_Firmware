/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "app/flc_def.h"
#include "app/flc_cfg.h"

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/nvm/nvm.h"
#include "plf/evos/evos.h"
#include "plf/vtim/vtim.h"
#include "plf/dlog/dlog.h"
#include "plf/heap/heapx.h"
#include "plf/json/jsonx.h"
#include "plf/cfg/blc_cfg.h"
#include "plf/blc/blc.h"
#include "plf/blc/blc_crc.h"
#include "plf/blc/blc_def.h"
#include "plf/ver/ver.h"

#include "net/rest/rest.h"
#include "net/modem_sim7070/modem.h"

#include "hal/hal.h"
#include "hal/cfg/pflash_cfg.h"

#include <string.h>
#include "cloud_def.h"

#include "cloud_flc.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define CIF_SM_TL                   TRC_TL_2
#define CIF_DT_TL                   TRC_TL_3

#define TICK_START_DELAY_MSEC       (1000u)
#define TICK_MSEC                   ( 100u)

#define PWR_ON_TIMEOUT_MSEC         ( 10u * 1000u)
#define PWR_OFF_TIMEOUT_MSEC        ( 10u * 1000u)
                                    
#define MODEM_CONN_TIMEOUT_MSEC     ( 5u * 60u * 1000u)
#define MODEM_STAB_TIME_MSEC        (       5u * 1000u) // MQTT connection fails if it's done too quickly after modem connection. Wait for modem to stabilize
#define MODEM_DISCONN_TIMEOUT_MSEC  ( 1u * 60u * 1000u)
                                    
#define MQTT_CONN_TIMEOUT_MSEC      ( 5u * 60u * 1000u)
#define MQTT_DISCONN_TIMEOUT_MSEC   ( 1u * 60u * 1000u)
#define MQTT_UPLOAD_TIMEOUT_MSEC    ( 1u * 60u * 1000u)
                                    
#define FW_DL_TIMEOUT_MSEC          ( 1u * 60u * 1000u) // Time between packages
                                    
#define CONNECTION_INTERVAL_MSEC    (10u * 60u * 1000u) // Time between connection retries

#define FW_MAJOR_TAG                "fw_major"
#define FW_MINOR_TAG                "fw_minor"
#define FW_INVALID                  -1

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  ON_ENTRY,
  ON_EXIT,
  ON_TICK,
  ON_EVENT_Last
} CifEvent_t;

typedef enum {
  CIF_IDLE,
  CIF_MODEM_PWR_ON,
  CIF_MODEM_CONNECT,
  CIF_MODEM_STABILIZE,
  CIF_MQTT_CONNECT,
  CIF_MQTT_UPLOAD,
  CIF_MQTT_DISCONNECT,
  CIF_CMD_DOWNLOAD,
  CIF_CMD_PARSE,
  CIF_FW_DOWNLOAD,
  CIF_MODEM_DISCONNECT,
  CIF_MODEM_PWR_OFF,
  CIF_FW_UPDATE,
  CIF_STATE_Last
} CifState_t;

typedef void (*StateFunc_t)(CifEvent_t event);

/*******************************************************************************
 * Function prototypes, state machine
 ******************************************************************************/

static void StateIdle(CifEvent_t event);
static void StateModemPwrOn(CifEvent_t event);
static void StateModemConnect(CifEvent_t event);
static void StateModemStabilize(CifEvent_t event);
static void StateMqttConnect(CifEvent_t event);
static void StateMqttUpload(CifEvent_t event);
static void StateMqttDisconnect(CifEvent_t event);
static void StateCmdDownload(CifEvent_t event);
static void StateCmdParse(CifEvent_t event);
static void StateFwDownload(CifEvent_t event);
static void StateModemDisconnect(CifEvent_t event);
static void StateModemPwrOff(CifEvent_t event);
static void StateModemFwUpdate(CifEvent_t event);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const StateFunc_t STATE_FUNCS[] = {
  StateIdle,             // CLD_IDLE
  StateModemPwrOn,       // CLD_MODEM_PWR_ON
  StateModemConnect,     // CLD_MODEM_CONNECT
  StateModemStabilize,   // CIF_MODEM_STABILIZE
  StateMqttConnect,      // CLD_MQTT_CONNECT
  StateMqttUpload,       // CLD_MQTT_UPLOAD
  StateMqttDisconnect,   // CLD_MQTT_DISCONNECT
  StateCmdDownload,      // CIF_CMD_DOWNLOAD
  StateCmdParse,         // CIF_CMD_PARSE
  StateFwDownload,       // CLD_FW_DOWNLOAD
  StateModemDisconnect,  // CLD_MODEM_DISCONNECT
  StateModemPwrOff,      // CLD_MODEM_PWR_OFF
  StateModemFwUpdate     // CIF_FW_UPDATE
};
static_assert(SIZEOF_ARRAY(STATE_FUNCS) == CIF_STATE_Last, "Wrong table row count!");

static const char * const STATE_NAMES[] = {
  "CIF_IDLE",
  "CIF_MODEM_PWR_ON",
  "CIF_MODEM_CONNECT",
  "CIF_MODEM_STABILIZE",
  "CIF_MQTT_CONNECT",
  "CIF_MQTT_UPLOAD",
  "CIF_MQTT_DISCONNECT",
  "CIF_CMD_DOWNLOAD",
  "CIF_CMD_PARSE",
  "CIF_FW_DOWNLOAD",
  "CIF_MODEM_DISCONNECT",
  "CIF_MODEM_PWR_OFF",
  "CIF_FW_UPDATE"
};
static_assert(SIZEOF_ARRAY(STATE_NAMES) == CIF_STATE_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

// State machine
static EvosEventHandle_t tick = EVOS_UNINITIALIZED_HANDLE;
static CifState_t currentState = CIF_IDLE;
static CifState_t nextState = CIF_MODEM_PWR_ON;
static bool nextIsLocked;
static VTim_t cifTimer;

// Log upload
static char mqttClientId[64];
static bool runCloud = false;
static Cif_t cfg;
static DLogEntry_t entry;

// FW Update
static VTim_t restGetTimer;
static FWDescriptor_t fwd;
static FwdBootCommand_t bc;
  
static bool cmdDownloading = false;
static bool cmdDlComplete = false;

static bool fwDownloading = false;
static bool fwDlComplete = false;

static int16_t fwMajor = FW_INVALID;
static int16_t fwMinor = FW_INVALID;

static bool canUpdateFw = false;

static bool crcIsDone = false;
static bool crcIsOk   = false;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static void EventProcess(CifEvent_t event);
static void Transistion(CifState_t state);

// Other
static void EvosEvent(EvosEventParam_t param);
static void DLogToJson(const DLogEntry_t * const logEntry, json_t * const root);
static void MqttTopicMake(char * buffer, uint8_t size, const char * deviceId, const char * dataType, UtcTime_t sessionId);
static bool FindNextLog(DLogEntry_t * log, RtcTime_t oldestTime);
static void CrcCheckDone(bool result);
static void RestGetAsyncCB(int len);
static size_t LoadCmdFile(void *buffer, size_t buflen, void * jsonReaderIndex);

// Configuration
static void AppConfigClear(void);
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void CloudFlcInit(void) 
{
  JsonxInit();
  ConfigLoad();
  
  VTimSetMsec(&cifTimer, CONNECTION_INTERVAL_MSEC);
  tick = EvosEventRegister(EvosEvent, "shc tick");
  EvosEventSetAndReload(tick, TICK_START_DELAY_MSEC, TICK_MSEC, ON_TICK);
}

void CloudFlcProcess(void)
{
  runCloud = true;
}

void CloudFlcSetCanUpdateFw(const bool canUpdate) {
  canUpdateFw = canUpdate;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static void AppConfigClear(void)
{
  // Empty application config with all bytes set to zero
  static const App_t emptyConfig;
  NvmWrite(offsetof(NvmStruct_t, appl.app), (uint8_t *)&emptyConfig, sizeof(App_t));
  
  // Do not default cloud interface settings because part of CIF settings is 
  // last log send and then a firmware update will also default this parameter 
  // resulting in all logs present in the data flash will be retransmitted to 
  // the cloud again.
  // The current CIF settings from RAM is saved again after they were overwritten
  // with zeroes.
  ConfigSave();
}

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.cif), (uint8_t *)&cfg, sizeof(Cif_t));
    if (cfg.crc != CalcCrc16(offsetof(Cif_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(Cif_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.cif), (uint8_t *)&cfg, sizeof(Cif_t));
}

static void ConfigSetDefault(void)
{
  cfg.lastUpload.all = 0;
  ConfigSave();
}

static void EvosEvent(const EvosEventParam_t param)
{
  EventProcess(param);
}

static void EventProcess(const CifEvent_t event)
{
  STATE_FUNCS[currentState](event);

  while (currentState != nextState) {
    // State transistions are not allowed on exit events to avoid transistioning
    // to a wrong state on programmer error/forgetfulness. 
    nextIsLocked = true;
    STATE_FUNCS[currentState](ON_EXIT);
    nextIsLocked = false;

    currentState = nextState;
    TrcTrace(TRC_TA_CIF, CIF_SM_TL, STATE_NAMES[currentState]);
    STATE_FUNCS[currentState](ON_ENTRY);
  }
}

static void Transistion(const CifState_t state)
{
  if (!nextIsLocked) {
    nextState = state;
  }
}

static void StateIdle(const CifEvent_t event)
{
  switch (event) {
    case ON_ENTRY:
      break; 
    case ON_TICK:      
      if (VTimIsExpired(&cifTimer) && runCloud) {
        Transistion(CIF_MODEM_PWR_ON);
      } else if (canUpdateFw) {
        canUpdateFw = false;
        Transistion(CIF_FW_UPDATE);
      }
      break;
    case ON_EXIT:
      runCloud = false;
      VTimRestart(&cifTimer);
      break;   
    
    default:
      break;
  }
}

static void StateModemPwrOn(const CifEvent_t event)
{
  static VTim_t pwrOnTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&pwrOnTimer, PWR_ON_TIMEOUT_MSEC);
      ModemPowerOn();
      break; 
    case ON_TICK:
      if(xModemPowerOn) {
        Transistion(CIF_MODEM_CONNECT);
      } else if (VTimIsExpired(&pwrOnTimer)) {
        Transistion(CIF_MODEM_PWR_OFF);
      }      
      break;
    case ON_EXIT:
      break;
      
    default:
      break;
  }
}

static void StateModemConnect(const CifEvent_t event)
{
  static VTim_t modemConnTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&modemConnTimer, MODEM_CONN_TIMEOUT_MSEC);
      ModemNetConnect();
      break;  
    case ON_TICK:
      if(xModemNetConnected) {
        Transistion(CIF_MODEM_STABILIZE);
      } else if (VTimIsExpired(&modemConnTimer)) {
        Transistion(CIF_MODEM_PWR_OFF);
      }
      break;
    case ON_EXIT:
      break;  
    
    default:
      break;
  }
}

static void StateModemStabilize(const CifEvent_t event)
{
  static VTim_t modemStabilizationTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&modemStabilizationTimer, MODEM_STAB_TIME_MSEC);
      break;   
    case ON_TICK:      
      if (VTimIsExpired(&modemStabilizationTimer)) {
        Transistion(CIF_MQTT_CONNECT);
      }
      break;
    case ON_EXIT:
      break; 
    
    default:
      break;
  }
}

static void StateMqttConnect(const CifEvent_t event)
{
  static VTim_t mqttConnTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&mqttConnTimer, MQTT_CONN_TIMEOUT_MSEC);
      snprintf(mqttClientId, sizeof(mqttClientId), "%s", xModemImei);
      ModemMqttBrokerConnect(MQTT_URL, MQTT_PORT, mqttClientId, MQTT_USER, MQTT_PW);
      break;
    case ON_TICK:
      if(xModemMqttConnected) {
        Transistion(CIF_MQTT_UPLOAD);
      } else if (VTimIsExpired(&mqttConnTimer)) {
        Transistion(CIF_MQTT_DISCONNECT);
      }
      break;
    case ON_EXIT:
      break;
    
    default:
      break;
  }
}

static void StateMqttUpload(const CifEvent_t event)
{
  static VTim_t mqttUploadTimer;
  static bool firstLogLocated = false;
  
  json_t * root;
  char   * jsonBody;
  char     topic[64];
  bool     logFound = false;
  
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&mqttUploadTimer, MQTT_UPLOAD_TIMEOUT_MSEC);
      memset(&entry, 0 , sizeof(entry));
      firstLogLocated = false;
      break; 
    case ON_TICK:
      
      if (VTimIsExpired(&mqttUploadTimer)) {
        Transistion(CIF_MQTT_DISCONNECT);
        break;
      }
      
      // MQTT bussy
      if (xModemMqttPublishing) {
        break;
      }
      
      // A log was found and uploadet. Save timestamp of log
      if (entry.log.time.all != 0) {
        cfg.lastUpload = entry.log.time;
        ConfigSave();
      }
      
      logFound = false;
      if (!firstLogLocated) {
        firstLogLocated = true;
        logFound = FindNextLog(&entry, cfg.lastUpload);
      } else if(DataLogFindNewer(&entry)) {        
        logFound = RtcTimeDiff(&(cfg.lastUpload), &(entry.log.time)) >= 0;
      }
      
      // No more logs
      if (!logFound) {
        Transistion(CIF_MQTT_DISCONNECT);
        break;
      }
      
      HeapxClean(); // HeapX does not implement freeing on EvOS and only Jansson uses HeapX - Clean entire heap.
      root = json_object();      
      DLogToJson(&entry, root);
      jsonBody = json_dumps(root, JSON_INDENT(2));
      
      const UtcTime_t sessionId = RtcToUtcTime(&entry.log.data.sessionId);
      MqttTopicMake(topic, sizeof(topic), mqttClientId, "RAW", sessionId);
      
      // Log publish failed
      if(!ModemMqttPublish(0, topic, jsonBody)) {
        Transistion(CIF_MQTT_DISCONNECT);
        break;
      }
      
      VTimRestart(&mqttUploadTimer);
      break;
    case ON_EXIT:
      break;   
    
    default:
      break;
  }
}

static void StateMqttDisconnect(const CifEvent_t event)
{
  static VTim_t mqttDisconnTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&mqttDisconnTimer, MQTT_DISCONN_TIMEOUT_MSEC);
      ModemMqttBrokerDisconnect();
      break;   
    case ON_TICK:
      if(!xModemMqttConnected || VTimIsExpired(&mqttDisconnTimer)) {
        Transistion(CIF_CMD_DOWNLOAD);
      }
      break;
    case ON_EXIT:
      break; 
    
    default:
      break;
  }
}

static void StateCmdDownload(const CifEvent_t event)
{
  static char cmdFileName[64];  
  switch (event) {
    case ON_ENTRY:    
      cmdDownloading = true;
      cmdDlComplete = false;
    
      VTimSetMsec(&restGetTimer, FW_DL_TIMEOUT_MSEC);
      snprintf(cmdFileName, sizeof(cmdFileName), "cmd/FLP%s.json", xModemImei);
    
      const int len = RestGet(cmdFileName, DFLASH_CMDFILE_START, CMDFILE_ENTRY_SIZE, RestGetAsyncCB);
      if (len < 0) {
        Transistion(CIF_MODEM_DISCONNECT);
      }
      break;
    case ON_TICK:
      if (cmdDlComplete && !xModemTcpConnected) {      
        Transistion(CIF_CMD_PARSE);
      } else if(VTimIsExpired(&restGetTimer)) {        
        Transistion(CIF_MODEM_DISCONNECT);        
      }
      break;
    case ON_EXIT:
      cmdDownloading = false;
      break;    
    
    default:
      break;
  }
}

static void StateCmdParse(const CifEvent_t event)
{
  uint32_t readerIndex = 0;
  const json_t * root;
  const json_t * tag;
  json_error_t error;
  
  switch (event) {
    case ON_ENTRY:
      fwMajor = FW_INVALID;
      fwMinor = FW_INVALID;
      crcIsDone = false;
    
      BlcCrcValidate(CrcCheckDone);
      break;    
    case ON_TICK:
      
      if(!crcIsDone) {
        break;
      }
      
      bc = BlcReadDescriptor(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd);
      
      root = json_load_callback(LoadCmdFile, &readerIndex, JSON_DISABLE_EOF_CHECK, &error);      
      tag = json_object_get(root, FW_MAJOR_TAG);
      if (tag && json_is_integer(tag)) {
        fwMajor = json_integer_value(tag);
      }
      
      tag = json_object_get(root, FW_MINOR_TAG);
      if (tag && json_is_integer(tag)) {
        fwMinor = json_integer_value(tag);
      }
      
      HeapxClean(); // HeapX does not implement freeing on EvOS and only Jansson uses HeapX - Clean entire heap.
      
        // JSON did not contain version tags.
      if(fwMajor == FW_INVALID || fwMinor == FW_INVALID) {
        Transistion(CIF_MODEM_DISCONNECT);
      } // FW Already downloaded and CRC of FW ok
      else if (crcIsOk && bc != FWD_BC_UNKNOWN && (fwd.swVerMajor == fwMajor && fwd.swVerMinor == fwMinor)) {
        Transistion(CIF_MODEM_DISCONNECT);
      } // Download new FW if it isn't the currently loaded version
      else if (fwMajor != PLF_VERSION_MAJOR || fwMinor != PLF_VERSION_MINOR) {
        Transistion(CIF_FW_DOWNLOAD);          
      }
      break;
    case ON_EXIT:
      cmdDownloading = false;
      break;    
    
    default:
      break;
  }
}

static void StateFwDownload(const CifEvent_t event)
{
  static char fileName[64];
  switch (event) {
    case ON_ENTRY:
      fwDownloading = true;
      fwDlComplete = false;
      VTimSetMsec(&restGetTimer, FW_DL_TIMEOUT_MSEC);
      snprintf(fileName, sizeof(fileName), "fw/flc_v%i_%i.dfu", fwMajor, fwMinor);
    
      const int len = RestGet(fileName, DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, RestGetAsyncCB);
      if (len < 0) {
        Transistion(CIF_MODEM_DISCONNECT);
      }      
      break;
    case ON_TICK:
      if(VTimIsExpired(&restGetTimer) || (fwDlComplete && !xModemTcpConnected)) {
        Transistion(CIF_MODEM_DISCONNECT);        
      }
      break;
    case ON_EXIT:
      fwDownloading = false;
      break;    
    
    default:
      break;
  }
}

static void StateModemDisconnect(const CifEvent_t event)
{
  static VTim_t modemDisconnTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&modemDisconnTimer, MODEM_DISCONN_TIMEOUT_MSEC);
      ModemNetDisconnect();
      break;
    case ON_TICK:
      if (!xModemNetConnected || VTimIsExpired(&modemDisconnTimer)) {
        Transistion(CIF_MODEM_PWR_OFF);
      }
      break;
    case ON_EXIT:
      break;    
    
    default:
      break;
  }
}

static void StateModemPwrOff(const CifEvent_t event)
{
  static VTim_t pwrOffTimer;
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&pwrOffTimer, PWR_OFF_TIMEOUT_MSEC);
      ModemPowerOff();
      break;
    case ON_TICK:
      if (!xModemPowerOn || VTimIsExpired(&pwrOffTimer)) {
        Transistion(CIF_IDLE);
      }
      break;
    case ON_EXIT:
      break;    
      
    default:
      break;
  }
}

static void StateModemFwUpdate(const CifEvent_t event)
{  
  switch (event) {
    case ON_ENTRY:
      crcIsDone = false;
      BlcCrcValidate(CrcCheckDone);
      break;
    case ON_TICK:
      
      if(!crcIsDone){
        break;
      }
      
      if(!crcIsOk) {
        Transistion(CIF_IDLE);
      } else { 
        bc = BlcCheckDataFlashFirmware(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd);
        
        // Only activate if FW version is different to current
        if (bc != FWD_BC_UNKNOWN && !(fwd.swVerMajor == PLF_VERSION_MAJOR && fwd.swVerMinor == PLF_VERSION_MINOR)) {
          // Flow Loop wants the application settings to be defaulted after a firmware update.
          AppConfigClear();
          
          BlcActivateNewImage(0, bc);
        } else {
          Transistion(CIF_IDLE);
        }
      }      
      break;
    case ON_EXIT:
      break;    
      
    default:
      break;
  }
}

static size_t LoadCmdFile(void * const buffer, size_t buflen, void * const param)
{
  uint32_t * const index = (uint32_t*)param;
  
  if(buflen <= 0) {
	  return 0;
	}
	
	buflen = (*index) + buflen > CMDFILE_ENTRY_SIZE ? 
	                                     CMDFILE_ENTRY_SIZE - (*index) :
	                                     buflen;
	
	HalDFlashRead(DFLASH_CMDFILE_START + (*index), buffer, buflen);  
	*index += buflen;
  
  // Look for end of file.
  for(uint_fast16_t idx = 0; idx < buflen; idx++) {
    if(((uint8_t*)buffer)[idx] == 0xff) {
      return idx;
    }
  }
	
	return buflen;
}

void XModemTcpGotData(const uint8_t * const data, const uint16_t length)
{
  VTimRestart(&restGetTimer);
  xModemTcpContinue(1, data, length);
}

void XModemTcp(const bool connect)
{
  static bool tcpConnected = false;
  if (connect) {
    if (xModemTcpContinue) {
      if (!tcpConnected) {
        tcpConnected = true;
        xModemTcpContinue(0, NULL, 0);
      } 
    }
  } else {
    if (tcpConnected) {
      tcpConnected = false;
      xModemTcpContinue(2, NULL, 0);
    }
  }
}

static void RestGetAsyncCB(const int len)
{
  if(fwDownloading) {
    fwDlComplete  = true;
  } else if (cmdDownloading) {
    cmdDlComplete = true;
  }
}

static void CrcCheckDone(const bool result)
{
  crcIsDone = true;
  crcIsOk = result;
}

static bool FindNextLog(DLogEntry_t * const logEntry, const RtcTime_t oldestTime)
{
  static char sz[sizeof("YYYY-MM-DD hh:mm:ss")];
  
  static DLogEntry_t tmplog;
  static DLogEntry_t activeLog;
  
  TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "Looking for log newer than: %s", RtcStr(sz, &oldestTime));	
  
  if(!DataLogFindNewest(&activeLog)) {
    TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "No new logs found");
    return false;
  }
  
  if(oldestTime.all != 0 && RtcTimeDiff(&oldestTime, &activeLog.log.time) <= 0) {
    TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "Newest log was not newer: %s", RtcStr(sz, &activeLog.log.time));
    return false;
  }
  
  TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "Newest log: %s", RtcStr(sz, &activeLog.log.time));
  
  while (DataLogFindOlder(&tmplog)) {
    // If tmplog time is larger than activeLog time or tmplog time is less than or queal to oldest time
    if(RtcTimeDiff(&tmplog.log.time, &activeLog.log.time) < 0 || (oldestTime.all != 0 && RtcTimeDiff(&tmplog.log.time, &oldestTime) >= 0)) {
      break;
    }
    
    activeLog = tmplog;
    TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "Found older log: %s", RtcStr(sz, &activeLog.log.time));
  }
  
  // Rewind the index to the currently active log.
  DataLogFindNewer(&tmplog);
  
  TRACE_VA(TRC_TA_CIF, CIF_DT_TL, "Oldest log: %s", RtcStr(sz, &activeLog.log.time));	
  *logEntry = activeLog;
  return true;
}

//Important! HeapX does not implement free. Call HeapxClean(); when disposing json object.
static void DLogToJson(const DLogEntry_t * const logEntry, json_t * const root)
{
  const DataLog_t * const log = &(logEntry->log.data);
  UtcTime_t utc = RtcToUtcTime(&(logEntry->log.time));
  UtcTime_t sessionId = RtcToUtcTime(&(logEntry->log.data.sessionId));
  
  json_object_set_new(root, "utc", json_integer(utc));  
  json_object_set_new(root, "state", json_string(ShcGetStateName(log->state)));  
  json_object_set_new(root, "sessId", json_integer(sessionId));

  // The current firmware version is added to the data log JSON. This is done as
  // a simple and fast solution because Flow Loop require some way to see the 
  // current firmware version on the cloud portal. A better version would be to 
  // omit this information from this JSON message and implement in some other 
  // device status JSON message.
  // NOTE: This field doesn't represent the firmware version when the log was 
  // created but the current firmware version that send the log message to the 
  // cloud.
  json_object_set_new(root, "fwVers", json_string(versionString));
  
  json_object_set_new(root, "hwCfgVers", json_integer(log->hwConfigVersion));  
  json_object_set_new(root, "dlogVers", json_integer(log->dlogVersion));
  
  json_t * valueArray = json_array();
  for (uint_fast8_t hwIdx = 0; hwIdx < HW_COMPONENT_Last; hwIdx++) {
    json_array_append_new(valueArray, json_real((double)(log->LoggedComponents[hwIdx].value)));
  }
  
  json_object_set_new(root, "values", valueArray);
}

static void MqttTopicMake(char * const buffer, const uint8_t size, const char * const deviceId, const char * const dataType, const UtcTime_t sessionId)
{
  snprintf(buffer, size, "" CLOUD_ROOT CLOUD_APPL "/%s/%s/%u", deviceId, dataType, sessionId);
}

/*******************************************************************************
 * CLI
 ******************************************************************************/

#ifdef CLI_ENABLE

static int_fast16_t CliShowJson(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3) {
  RtcTime_t currentTime;
  RtcGet(&currentTime);
  
  if (!DataLogFindNewest(&entry)) {
    CliPrintf("No logs found\n");
    return CLI_RESULT_OK;
  }
  
  json_t * const root = json_object();
  DLogToJson(&entry, root);
  
  
  char * str = json_dumps(root, JSON_INDENT(2));  
  CliPrintf("%s\n", str);
  
  HeapxClean(); // HeapX does not implement freeing on EvOS and only Jansson uses HeapX - Clean entire heap.
  return CLI_RESULT_OK;
}

static int_fast16_t CliRun(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3) {
  CloudFlcProcess();
  return CLI_RESULT_OK;
}

CLI_START_TABLE(cloud)
  CLI_ENTRY0( "json",   "Show latest log in Json format",              CliShowJson)
  CLI_ENTRY0( "trig",   "Trigger the cloud processing of logs and FW", CliRun)

  CLI_SUBTEST("trc",    "Trace system",   trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(cloud)

#endif

/******************************************************************************/
