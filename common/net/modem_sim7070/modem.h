/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef MODEM_H
#define MODEM_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/cfg/modem_cfg.h"
#include "plf/trc//trc.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

// Modem state machines
typedef enum {
  MODEM_SM_POWER,
  MODEM_SM_READY,
  MODEM_SM_NET,
#if defined(MODEM_TCP_SUPPORT) 
  MODEM_SM_TCP,
#endif  
#if defined(MODEM_MQTT_SUPPORT) 
  MODEM_SM_MQTT,
#endif  
#if defined(MODEM_FOTA_SUPPORT) 
  MODEM_SM_FOTA,
#endif  
  MODEM_SM_Last
} ModemSm_t;

typedef bool (*ModemSmNextFunc_t)(uint8_t current, uint8_t * next);

typedef void (*ModemSmOnEntryFunc_t)(void);
typedef void (*ModemSmOnExitFunc_t)(void);

typedef struct {
  ModemSmOnEntryFunc_t onEntry;
  ModemSmOnExitFunc_t onExit;
} ModemSmStateFuncs_t;

typedef bool (*ModemAtParseFunc_t)(const char * sz);

/*******************************************************************************
 * Global data variables
 ******************************************************************************/

// eXternal read-only

extern const char * xModemModel;
extern const char * xModemRevision;
extern const char * xModemImei;
extern uint64_t xModemImeiNo;

extern bool xModemAtBusy;
extern bool xModemAtOkReceived;
extern bool xModemAtErrorReceived;

extern bool xModemPowerLocked;
extern bool xModemPowerOn;

extern bool xModemReadyUart;
extern bool xModemReadySim;

extern bool xModemSimNotInserted;
extern const char * xModemSimImsi;

extern bool xModemNetLocked;
extern bool xModemNetConnected;

#define MODEM_RSSI_NOT_DETECTABLE 99
extern uint8_t xModemNetRssi;
#define MODEM_BER_NOT_DETECTABLE  99
extern uint8_t xModemNetBer;

extern bool xModemNetServiceAttached;
extern const char * xModemNetInfo;
extern const char * xModemNetMode;
extern const char * xModemNetApn;

#if defined(MODEM_TCP_SUPPORT)

extern bool xModemTcpLocked;
extern bool xModemTcpConnected;
extern const char * xModemTcpHost;
extern uint16_t xModemTcpPort;
extern bool xModemTcpDataSending;
extern bool xModemTcpDataReceiving;

typedef void (*XModemTcpContinue_t)(int step, const uint8_t * data, int len);
extern XModemTcpContinue_t xModemTcpContinue;
void XModemTcp(bool connect);
//extern int XModemTcpDataGet(
//  uint8_t * buffer);

bool ModemTcpOpen(
  const char * host, 
  uint16_t port);
bool ModemTcpClose(void);

bool ModemTcpSendData(
  const uint8_t * data, 
  uint16_t length);

#endif

#if defined(MODEM_MQTT_SUPPORT)
extern bool             xModemMqttLocked;
extern bool             xModemMqttConnected;
extern const char *     xModemMqttHost;
extern uint16_t         xModemMqttPort;
extern const char *     xModemMqttClientId;
extern const char *     xModemMqttUsername;
extern const char *     xModemMqttPassword;
extern bool xModemMqttSubscribing;
extern bool xModemMqttUnsubscribing;
extern bool xModemMqttPublishing;
extern const char * xModemMqttTopic; 
#endif
  
#if defined(MODEM_FOTA_SUPPORT)
extern bool             xModemFotaLocked;
extern const char *     xModemFotaUrl;
extern bool             xModemFotaIsDownloading;
extern bool             xModemFotaIsUpdating;
extern bool             xModemFotaIsSuccess;
#endif

/*******************************************************************************
 * Functions
 ******************************************************************************/
 
void ModemInit(void);

bool ModemPowerOn(void);
bool ModemPowerOff(void);
bool ModemForcePowerOff(void);

bool ModemNetConnect(void);
bool ModemNetDisconnect(void);

#if defined(MODEM_MQTT_SUPPORT)
bool ModemMqttBrokerConnect(
  const char * host, 
  const uint16_t port, 
  const char * client, 
  const char * user,
  const char * pw);
bool ModemMqttBrokerDisconnect(void);  
bool ModemMqttSubscribe(const char * topic);
bool ModemMqttUnsubscribe(const char * topic);
bool ModemMqttPublish(int things, const char * topic, const char * msg);  
#endif

#if defined(MODEM_FOTA_SUPPORT)
bool ModemFotaHttpUpdate(const char * url);
void ModemFotaStatusCheck(void);
#endif
  
// Call from external UART non-interrupt function
void ModemAtDataCallback(uint8_t ch);

// Call from external GPIO interrupt functions on rising and falling edges
void ModemXiStatusCb(void);   

/******************************************************************************/

// eXternal required features - implement elsewhere

// Send AT data
void XModemAtSendText(const char * txt);
void XModemAtSendData(const uint8_t * data, uint16_t length);

// Receive TCP binary data
#if defined(MODEM_TCP_SUPPORT)
void XModemTcpGotData(const uint8_t * data, uint16_t length);
#endif

// Received MQTT topic data
#if defined(MODEM_MQTT_SUPPORT)
void XModemMqttGotData(const char * topic, const char * msg);
#endif

// Status
void XModemPower(bool on);
void XModemNet(bool connect);
#if defined(MODEM_TCP_SUPPORT)
void XModemTcp(bool connect);
#endif
#if defined(MODEM_MQTT_SUPPORT)
void XModemMqtt(bool connect);
#endif

// Errors
void XModemSimNotInserted(void);

// Logging
void XModemLogFunc(const char * info, ...);
bool XModemLogShowFunc(void);
void XModemLogState(const char * info, ...);
bool XModemLogShowState(void);
void XModemLogTraffic(const char * info, ...);
bool XModemLogShowAtTraffic(void);
bool XModemLogShowDataTraffic(void);

/******************************************************************************/

// Internal required features

void ModemSmRegister(const ModemSm_t sm, ModemSmNextFunc_t next, const ModemSmStateFuncs_t * stateFuncs, const char ** texts);
void ModemSmUpdate(void);

void ModemAtRegister(ModemSm_t sm, ModemAtParseFunc_t normal, ModemAtParseFunc_t partial, ModemAtParseFunc_t afterOk);
void ModemAtSendText(const char * info, ...);
void ModemAtSetTimeoutSendText(uint32_t msec, const char * info, ...);
const char * ModemAtPreviousSendText(void);
const char * ModemAtPreviousReceivedText(void);

bool ModemProtocolLocked(void);

#if defined(MODEM_TCP_SUPPORT)
void ModemTcpBinaryDataReceive(uint16_t length);
#endif

void ModemLogBinaryData(const char * prefix, const uint8_t * data, uint16_t length);
void ModemLogAtText(const char * prefix, const char * sz);
void ModemLogFunc(const char * sz, const char * s1, const char * s2);
void ModemLogState(const char * sz);

/******************************************************************************/

#ifdef CLI_ENABLE 
  CLI_DECLARE_SUBTEST(modem)
  extern int_fast16_t TestModemShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
