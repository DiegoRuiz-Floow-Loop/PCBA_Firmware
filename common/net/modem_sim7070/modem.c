/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include <hal/hal.h>

#include <plf/evos/evos.h>

#include "modem_power.h"
#include "modem_ready.h"
#include "modem_net.h"
#include "modem_tcp.h"
#include "modem_mqtt.h"
#include "modem_fota.h"

#include "modem.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  ModemSmNextFunc_t next[MODEM_SM_Last];
  const ModemSmStateFuncs_t * state[MODEM_SM_Last];
} ModemSmFuncs_t;

typedef struct {
  ModemAtParseFunc_t normal[MODEM_SM_Last];
  ModemAtParseFunc_t partial[MODEM_SM_Last];
  ModemAtParseFunc_t afterOk[MODEM_SM_Last];
} ModemAtFuncs_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const char TX_NEW_LINE[] = "\r";

#define AT_TIMEOUT_MSEC (1000)

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t atCmdTimer = EVOS_UNINITIALIZED_HANDLE;

static uint8_t atTx[MODEM_AT_TX_BUFFER_SIZE];
static uint8_t atRx[MODEM_AT_RX_BUFFER_SIZE];
static uint8_t atRxPrevious[MODEM_AT_RX_BUFFER_SIZE];
static uint_fast16_t atRxIdx;

static uint8_t state[MODEM_SM_Last];
static const char ** smTexts[MODEM_SM_Last];
static ModemSmFuncs_t smFuncs;
static ModemAtFuncs_t atFuncs;

#if defined(MODEM_TCP_SUPPORT)
static uint_fast16_t tcpRxLength;
#endif

#if defined(MODEM_MQTT_SUPPORT)
//static uint_fast16_t mqttRxLength;
#endif

/*******************************************************************************
 * Global data variables
 ******************************************************************************/

const char * xModemModel = "";
const char * xModemRevision = "";
const char * xModemImei = "";
uint64_t xModemImeiNo;

bool xModemAtBusy;
bool xModemAtOkReceived;
bool xModemAtErrorReceived;

bool xModemPowerLocked;
bool xModemPowerOn;

bool xModemReadyUart;
bool xModemReadySim;

bool xModemSimNotInserted;
const char * xModemSimImsi = "";

bool xModemNetLocked;
bool xModemNetConnected;

uint8_t xModemNetRssi = MODEM_RSSI_NOT_DETECTABLE;

uint8_t xModemNetBer = MODEM_BER_NOT_DETECTABLE;

bool xModemNetServiceAttached;
const char * xModemNetInfo = "";
const char * xModemNetMode = "";
const char * xModemNetApn = "";

#if defined(MODEM_TCP_SUPPORT)
bool xModemTcpLocked;
bool xModemTcpConnected;
const char * xModemTcpHost = "";
uint16_t xModemTcpPort;
bool xModemTcpDataSending;
bool xModemTcpDataReceiving;
#endif

#if defined(MODEM_MQTT_SUPPORT)
bool xModemMqttLocked;
bool xModemMqttConnected;
const char * xModemMqttHost = "";
uint16_t xModemMqttPort;
const char * xModemMqttClientId = "";
const char * xModemMqttUsername = "";
const char * xModemMqttPassword = "";  
bool xModemMqttSubscribing;
bool xModemMqttUnsubscribing;
bool xModemMqttPublishing;
const char * xModemMqttTopic = "";  
#endif

#if defined(MODEM_FOTA_SUPPORT)
bool             xModemFotaLocked;
const char *     xModemFotaUrl = "";
bool             xModemFotaIsDownloading;
bool             xModemFotaIsUpdating;
bool             xModemFotaIsSuccess;
#endif

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool UpdateSm(uint8_t * current, ModemSmNextFunc_t nextFunc, const ModemSmStateFuncs_t * funcs, const char ** texts);

// AT building/parsing
static void AtSend(uint32_t msec, const char * info, va_list ap);
static void AtAddChar(uint8_t ch);
static void AtParse(const char * sz);
static void AtParsePartial(const char * sz);

// AT command timer
static void AtCmdTimerSet(uint32_t msec);
static void AtCmdTimerClear(void); 
static void AtCmdTimeoutEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemInit(void)
{
  ModemLogFunc("ModemInit()", NULL, NULL);
  
  atCmdTimer = EvosEventRegister(AtCmdTimeoutEvent, "modem at cmd timer");
  
  ModemPowerInit();
  ModemReadyInit();
  ModemNetInit();
#if defined(MODEM_TCP_SUPPORT)  
  ModemTcpInit();
#endif  
#if defined(MODEM_MQTT_SUPPORT)  
  ModemMqttInit();
#endif 
#if defined(MODEM_FOTA_SUPPORT)
  ModemFotaInit();
#endif  
}

void ModemAtDataCallback(const uint8_t ch)
{
  AtAddChar(ch);
}

void ModemSmRegister(const ModemSm_t sm, ModemSmNextFunc_t next, const ModemSmStateFuncs_t * const stateFuncs, const char ** const texts)
{
  if (sm >= MODEM_SM_Last) return;
  
  smFuncs.next[sm] = next;
  smFuncs.state[sm] = stateFuncs;
  smTexts[sm] = texts;
}

void ModemSmUpdate(void)
{
  bool isChanged;
  do {
    isChanged = false;
    
    for (uint_fast8_t index = 0; index < MODEM_SM_Last; index++) {
      isChanged |= UpdateSm(&state[index], smFuncs.next[index], smFuncs.state[index], smTexts[index]);
    }
  } while(isChanged);  
}

void ModemAtRegister(ModemSm_t sm, ModemAtParseFunc_t normal, ModemAtParseFunc_t partial, ModemAtParseFunc_t afterOk)
{
  if (sm >= MODEM_SM_Last) return;
  
  atFuncs.normal[sm]  = normal;
  atFuncs.partial[sm] = partial;
  atFuncs.afterOk[sm] = afterOk;
}

void ModemAtSendText(const char * info, ...)
{
  va_list ap;
  va_start(ap, info);
  AtSend(AT_TIMEOUT_MSEC, info, ap);
  va_end(ap);
}

void ModemAtSetTimeoutSendText(const uint32_t msec, const char * info, ...)
{
  va_list ap;
  va_start(ap, info);
  // Add additional normal command timeout so the important timeouts are not as
  // close to their datasheet limits
  AtSend(msec + AT_TIMEOUT_MSEC, info, ap);
  va_end(ap);
}

const char * ModemAtPreviousSendText(void)
{
  return (const char *)atTx;
}

const char * ModemAtPreviousReceivedText(void)
{
  return (const char *)atRxPrevious;
}

bool ModemProtocolLocked(void)
{
  bool locked = false;
#if defined(MODEM_TCP_SUPPORT)
  locked |= xModemTcpLocked;
#endif
#if defined(MODEM_MQTT_SUPPORT)
  locked |= xModemMqttLocked;
#endif    
  return locked;
}

#if defined(MODEM_TCP_SUPPORT)
void ModemTcpBinaryDataReceive(const uint16_t length)
{
  tcpRxLength = length;
}
#endif

#if defined(MODEM_MQTT_SUPPORT)
void ModemMqttBinaryDataReceive(const uint16_t length)
{
//  mqttRxLength = length;
}
#endif

void ModemLogBinaryData(const char * const prefix, const uint8_t * const data, const uint16_t length)
{
  if (!XModemLogShowDataTraffic()) return;
  if (!length) return;
  
  const uint_fast16_t rowLength = (length / 16) + ((length % 16) ? 1 : 0);
  for (uint_fast16_t row = 0; row < rowLength; row++) {
    char sz[64];
    strcpy(sz, prefix);
    for (uint_fast8_t index = 0; index < MIN_VAL(16, (length - row * 16)); index++) {
      sprintf(&sz[strlen(sz)], " %02X", data[row * 16 + index]);
    }
    XModemLogTraffic(sz);
  }  
}

void ModemLogAtText(const char * prefix, const char * sz)
{
  if (!XModemLogShowAtTraffic()) return;
  
  XModemLogTraffic("%s %s", prefix, sz);
}

void ModemLogFunc(const char * sz, const char * s1, const char * s2)
{
  if (!XModemLogShowFunc()) return;
  
  if (s2)
    XModemLogFunc(sz, s1, s2);
  else if (s1)
    XModemLogFunc(sz, s1);
  else
    XModemLogFunc(sz);
}

void ModemLogState(const char * const sz)
{
  if (!XModemLogShowState()) return;
  
  XModemLogState("modem %s", sz);
}

/*******************************************************************************
 * Public functions, implemented elsewhere
 ******************************************************************************/

// For __weak keyword support include hal.h and it will be handled by CMSIS
// framework for Keil ARM compiler V6.

__weak void XModemAtSendText(const char * const txt)
{
}

__weak void XModemAtSendData(const uint8_t * const data, const uint16_t length)
{
}

#if defined(MODEM_TCP_SUPPORT)
__weak void XModemTcpGotData(const uint8_t * const data, const uint16_t length)
{
}
#endif

#if defined(MODEM_MQTT_SUPPORT)
__weak void XModemMqttGotData(const char * const topic, const char * const msg)
{
}
#endif

__weak void XModemPower(const bool on)
{
}

__weak void XModemNet(const bool connect)
{
}

#if defined(MODEM_TCP_SUPPORT)
__weak void XModemTcp(const bool connect)
{
}
#endif

#if defined(MODEM_MQTT_SUPPORT)
__weak void XModemMqtt(const bool connect)
{
}
#endif

__weak void XModemSimNotInserted(void)
{
}

__weak void XModemLogFunc(const char * info, ...)
{
}

__weak bool XModemLogShowFunc(void)
{
  return false;
}

__weak void XModemLogState(const char * const info, ...)
{
}

__weak bool XModemLogShowState(void)
{
  return false;
}

__weak void XModemLogTraffic(const char * const info, ...)
{
}

__weak bool XModemLogShowAtTraffic(void)
{
  return false;
}

__weak bool XModemLogShowDataTraffic(void)
{
  return false;
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static bool UpdateSm(uint8_t * const current, 
                     ModemSmNextFunc_t nextFunc, 
                     const ModemSmStateFuncs_t * const funcs, 
                     const char ** const texts)
{
  if ((nextFunc == NULL) || (funcs == NULL) || (texts == NULL)) return false;
  
  bool anyChange = false;

  bool isChanged;
  do {
    uint8_t next;
    isChanged = nextFunc(*current, &next);

    if (isChanged) {
      anyChange = true;
      
      if (funcs[*current].onExit != NULL) funcs[*current].onExit();
      if (texts[next] != NULL)            ModemLogState(texts[next]);
      if (funcs[next].onEntry != NULL)    funcs[next].onEntry();
      *current = next;
    }
  } while(isChanged);
  
  return anyChange;
}

/*******************************************************************************
 * Local functions, AT building/parsing
 ******************************************************************************/

static void AtSend(uint32_t msec, const char * info, va_list ap)
{
  vsnprintf((char *)atTx, sizeof(atTx) - sizeof(TX_NEW_LINE), info, ap);
  ModemLogAtText(">>", (const char *)atTx);
  
  strcat((char *)atTx, TX_NEW_LINE);

  xModemAtOkReceived = false;
  xModemAtErrorReceived = false;
  xModemAtBusy = true;
  
  XModemAtSendText((char *)atTx);
	AtCmdTimerSet(msec); 
}

static void AtAddChar(const uint8_t ch)
{
#if defined(MODEM_TCP_SUPPORT)
  // Receive binary TCP socket data
  if (tcpRxLength) {
	  atRx[atRxIdx++] = ch;
    if (atRxIdx >= tcpRxLength) {
      XModemTcpGotData(atRx, tcpRxLength);
      ModemLogBinaryData("<<", atRx, tcpRxLength);
      tcpRxLength = 0;      
      atRxIdx = 0;
    }  
  } else
#endif  
  
  // Text based AT data
  if (atRxIdx >= MODEM_AT_RX_BUFFER_SIZE) {
    atRx[atRxIdx - 1] = '\0';
    AtParse((const char *)atRx);
    atRxIdx = 0;
  } else if (ch == '\r') {
    if (atRxIdx > 0) {
      atRx[atRxIdx] = '\0';
      AtParse((const char *)atRx);
      atRxIdx = 0;
    }
  } else
  
  if (ch == ',') {
    atRx[atRxIdx] = '\0';
    AtParsePartial((const char *)atRx);
#if defined(MODEM_TCP_SUPPORT)    
    if (tcpRxLength) {
      atRxIdx = 0;
    } else {
#endif
      atRx[atRxIdx++] = ',';
#if defined(MODEM_TCP_SUPPORT)      
    }
#endif
  } else
  
  if (ch >= ' ') {
    atRx[atRxIdx++] = ch;
    
#if defined(MODEM_TCP_SUPPORT)    
    // Check if binary socket data is to be send
    if ((atRxIdx == 2) && (atRx[0] == '>') && (atRx[1] == ' ')) {
      atRx[atRxIdx] = '\0';
      AtParsePartial((const char *)atRx);
      atRxIdx = 0;
    }
#endif    
  }
}

static void AtParse(const char * const sz)
{
  bool isRecognized = false;
  
  ModemLogAtText("<<", sz);
  
  if (StrGE(sz, "OK")) {
    xModemAtOkReceived = true;
    xModemAtBusy = false;
    AtCmdTimerClear();
    isRecognized = true;
    
    for (uint_fast8_t index = 0; index < MODEM_SM_Last; index++) {
      if (atFuncs.afterOk[index] != NULL) {
        if (atFuncs.afterOk[index](sz)) break;
      }
    }
  }
  else if (StrGE(sz, "ERROR")) {
    xModemAtErrorReceived = true;
    xModemAtBusy = false;
    AtCmdTimerClear();
    isRecognized = true;
  }
  else {
    for (uint_fast8_t index = 0; index < MODEM_SM_Last; index++) {
      if (atFuncs.normal[index] != NULL) {
        if (atFuncs.normal[index](sz)) {
          isRecognized = true;
          break;
        }
      }
    }    
  }
  
  strncpy((char *)atRxPrevious, sz, MODEM_AT_RX_BUFFER_SIZE);
  if (isRecognized) {
    ModemSmUpdate();
  }
}

static void AtParsePartial(const char * const sz)
{
  // No overall logging of partial AT commands.
  // Each AT command recognized in this function handles the logging of the
  // received AT command.
  
  bool isRecognized = false;
  
  for (uint_fast8_t index = 0; index < MODEM_SM_Last; index++) {
    if (atFuncs.partial[index] != NULL) {
      if (atFuncs.partial[index](sz)) {
        isRecognized = true;
        break;
      }
    }
  }  
  
  if (isRecognized) {
    ModemSmUpdate();
  }  
}

/*******************************************************************************
 * Local functions, AT command timer
 ******************************************************************************/

static void AtCmdTimerSet(const uint32_t msec)
{
  EvosEventSetDelta2(atCmdTimer, msec, 0);
}

static void AtCmdTimerClear(void)
{
  EvosEventClear(atCmdTimer);
}

static void AtCmdTimeoutEvent(const EvosEventParam_t param)
{
  ModemLogState("at cmd timeout");
  xModemAtBusy = false;
  ModemSmUpdate();
}

/******************************************************************************/

