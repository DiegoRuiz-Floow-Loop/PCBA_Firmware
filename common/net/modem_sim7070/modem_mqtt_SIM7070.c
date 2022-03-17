/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "modem.h"

#if defined(MODEM_MQTT_SUPPORT)

#include <stdio.h>
#include <string.h>

#include "plf/evos/evos.h"

#include "modem_mqtt.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  MQTT_IDLE,
  MQTT_URL_SET,
  MQTT_KEEP_TIME_SET,
  MQTT_CLEAN_SESSION_SET,
  MQTT_QOS_SET,
  MQTT_CLIENT_SET,
  MQTT_USER_SET,
  MQTT_PASS_SET,
  MQTT_CONNECT,
  MQTT_DISCONNECT,
  MQTT_CONNECTED_CHECK,
  MQTT_SUBSCRIBE,
  MQTT_UNSUBSCRIBE,
  MQTT_PUBLISH,  
  MQTT_Last
} MqttState_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryIdle(void);
static void OnEntryUrlSet(void);
static void OnEntryKeepTimeSet(void);
static void OnEntryCleanSessionSet(void);
static void OnEntryQosSet(void);
static void OnEntryClientSet(void);
static void OnEntryUserSet(void);
static void OnEntryPassSet(void);
static void OnEntryConnect(void);
static void OnExitConnect(void);
static void OnEntryDisconnect(void);
static void OnExitDisconnect(void);
static void OnEntryConnectedCheck(void);
static void OnExitConnectedCheck(void);
static void OnEntrySubscribe(void);
static void OnExitSubscribe(void);
static void OnEntryUnsubscribe(void);
static void OnExitUnsubscribe(void);
static void OnEntryPublish(void);
static void OnExitPublish(void);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t MQTT_FUNCS[MQTT_Last] = {
  // .onEntry,               .onExit
  { OnEntryIdle,             DoNothing },               // MQTT_IDLE
  { OnEntryUrlSet,           DoNothing },               // MQTT_URL_SET
  { OnEntryKeepTimeSet,      DoNothing },               // MQTT_KEEP_TIME_SET
  { OnEntryCleanSessionSet,  DoNothing },               // MQTT_CLEAN_SESSION_SET
  { OnEntryQosSet,           DoNothing },               // MQTT_QOS_SET
  { OnEntryClientSet,        DoNothing },               // MQTT_CLIENT_SET
  { OnEntryUserSet,          DoNothing },               // MQTT_USER_SET
  { OnEntryPassSet,          DoNothing },               // MQTT_PASS_SET
  { OnEntryConnect,          OnExitConnect },           // MQTT_CONNECT
  { OnEntryDisconnect,       OnExitDisconnect },        // MQTT_DISCONNECT
  { OnEntryConnectedCheck,   OnExitConnectedCheck },    // MQTT_CONNECTED_CHECK
  { OnEntrySubscribe,        OnExitSubscribe },         // MQTT_SUBSCRIBE
  { OnEntryUnsubscribe,      OnExitUnsubscribe },       // MQTT_UNSUBSCRIBE
  { OnEntryPublish,          OnExitPublish },           // MQTT_PUBLISH
};

static const char * MQTT_STATE_TEXTS[MQTT_Last] = {
  "mqtt idle",              // MQTT_IDLE
  "mqtt url set",           // MQTT_URL_SET
  "mqtt keep time set",     // MQTT_KEEP_TIME_SET
  "mqtt clean session set", // MQTT_CLEAN_SESSION_SET
  "mqtt qos set",           // MQTT_QOS_SET
  "mqtt client set",        // MQTT_CLIENT_SET
  "mqtt user set",          // MQTT_USER_SET
  "mqtt pass set",          // MQTT_PASS_SET
  "mqtt connect",           // MQTT_CONNECT
  "mqtt disconnect",        // MQTT_DISCONNECT
  "mqtt connected check",   // MQTT_CONNECTED_CHECK
  "mqtt subscribe",         // MQTT_SUBSCRIBE
  "mqtt unsubscribe",       // MQTT_UNSUBSCRIBE
  "mqtt publish",           // MQTT_PUBLISH
};

//static const uint8_t QOS = 0;
#define QOS     (1)

// No timeouts mentioned in SIM7070G datasheets for MQTT. Using the same timings
// as TCP because MQTT protocol uses TCP.
static const uint32_t AT_CONNECT_TIMEOUT_MSEC = 75 * 1000;
static const uint32_t AT_SEND_TIMEOUT_MSEC = 5000;


/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t pollTimer = EVOS_UNINITIALIZED_HANDLE;

static bool connectRequired;
static bool disconnectRequired;
static bool pollTimeout;
//static uint8_t mThings = 0;

static const char * publishMsg = "";

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextMqttState(uint8_t current, uint8_t * next);

// AT parse
static bool AtParse(const char * sz);
static bool AtParsePartial(const char * sz);
static bool AtParseAfterOk(const char * sz);

// Other
static void PollTimerSet(void);
static void PollTimerClear(void);
static void PollTimeoutEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemMqttInit(void)
{
  pollTimer = EvosEventRegister(PollTimeoutEvent, "modem mqtt poll");
  
  ModemSmRegister(MODEM_SM_MQTT, NextMqttState, MQTT_FUNCS, MQTT_STATE_TEXTS);
  ModemAtRegister(MODEM_SM_MQTT, AtParse, AtParsePartial, AtParseAfterOk);
}

bool ModemMqttBrokerConnect(
  const char *  host, 
  const uint16_t port, 
  const char *  client, 
  const char *  user, 
  const char *  pw)
{
  if (!xModemNetConnected || xModemMqttLocked) return false;
  
  ModemLogFunc("ModemMqttBrokerConnect(%s, %s, ...)", host, client);
  
  connectRequired = true;
  xModemMqttLocked = true;
  xModemMqttHost = host;
  xModemMqttPort = port;
  xModemMqttClientId = client;
  xModemMqttUsername = user;
  xModemMqttPassword = pw;
  ModemSmUpdate();
  return true;
}

bool ModemMqttBrokerDisconnect(void)
{
  ModemLogFunc("ModemMqttBrokerDisconnect()", NULL, NULL);
  
  if (xModemMqttLocked) {
    connectRequired = false;
    disconnectRequired = true;
    ModemSmUpdate();
  }
  return true;
}

bool ModemMqttSubscribe(const char * const topic)
{
  if (!xModemMqttConnected) return false;
  if (strlen(topic) > MQTT_TOPIC_MAX_LENGTH) return false;
  if (xModemMqttSubscribing || xModemMqttUnsubscribing || xModemMqttPublishing) return false;
  
  ModemLogFunc("ModemMqttSubscribe(%s)", topic, NULL);
  
  xModemMqttSubscribing = true;
  xModemMqttTopic = topic;
  ModemSmUpdate();
  return true;  
}

bool ModemMqttUnsubscribe(const char * topic)
{
  if (!xModemMqttConnected) return false;
  if (strlen(topic) > MQTT_TOPIC_MAX_LENGTH) return false;
  if (xModemMqttSubscribing || xModemMqttUnsubscribing || xModemMqttPublishing) return false;

  ModemLogFunc("ModemMqttUnsubscribe(%s)", topic, NULL);
  
  xModemMqttUnsubscribing = true;
  xModemMqttTopic = topic;
  ModemSmUpdate();
  return true;  
}

bool ModemMqttPublish(int things, const char * const topic, const char * const msg)
{
  if (!xModemMqttConnected) return false;
  if (strlen(topic) > MQTT_TOPIC_MAX_LENGTH) return false;
  if (xModemMqttSubscribing || xModemMqttUnsubscribing || xModemMqttPublishing) return false;

  ModemLogFunc("ModemMqttPublish(%s, %s)", topic, msg);  
//  mThings = things;
  xModemMqttPublishing = true;
  xModemMqttTopic = topic; 
  publishMsg = msg;
  ModemSmUpdate();
  return true;
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextMqttState(const uint8_t current, uint8_t * const next)
{
  if (xModemAtBusy) return false;
  
  *next = MQTT_Last;
  switch (current) {
    case MQTT_IDLE:
      if (xModemMqttConnected && !xModemNetConnected) 
        *next = MQTT_DISCONNECT;
      else if (!xModemNetConnected) 
        break;

      else if (!xModemMqttConnected && connectRequired) 
        *next = MQTT_URL_SET;
      else if (disconnectRequired) 
        *next = MQTT_DISCONNECT;
      else if (xModemMqttUnsubscribing) 
        *next = MQTT_UNSUBSCRIBE;
      else if (xModemMqttSubscribing) 
        *next = MQTT_SUBSCRIBE;
      else if (xModemMqttPublishing) 
        *next = MQTT_PUBLISH;
      else if (xModemMqttConnected && pollTimeout) 
        *next = MQTT_CONNECTED_CHECK;
      break;

    case MQTT_URL_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_KEEP_TIME_SET;
      break;

    case MQTT_KEEP_TIME_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_CLEAN_SESSION_SET;      
      break;

    case MQTT_CLEAN_SESSION_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_QOS_SET;      
      break;
    
    case MQTT_QOS_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_CLIENT_SET;      
      break;    

    case MQTT_CLIENT_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_USER_SET;      
      break;

    case MQTT_USER_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else *next = MQTT_PASS_SET;      
      break;

    case MQTT_PASS_SET:
      if (xModemAtErrorReceived) *next = MQTT_IDLE;
      else if (!xModemNetConnected) *next = MQTT_IDLE;
      else *next = MQTT_CONNECT;      
      break;

    case MQTT_CONNECT:
    case MQTT_DISCONNECT:
    case MQTT_CONNECTED_CHECK:
    case MQTT_SUBSCRIBE:
    case MQTT_UNSUBSCRIBE:
    case MQTT_PUBLISH:
      *next = MQTT_IDLE;
      break;

    default:
      break;
  }
  return (*next < MQTT_Last);
}

static void OnEntryIdle(void)
{
  xModemMqttLocked = xModemMqttConnected;

  if (!xModemMqttConnected) {
    xModemMqttSubscribing = false;
    xModemMqttUnsubscribing = false;
    xModemMqttPublishing = false;
  }
  
  if (connectRequired || disconnectRequired) {
    connectRequired = false;
    disconnectRequired = false;
    
    XModemMqtt(xModemMqttConnected);
  }
}

static void OnEntryUrlSet(void)
{
  ModemAtSendText("AT+SMCONF=\"URL\",%s,%u", xModemMqttHost, xModemMqttPort);
}

static void OnEntryKeepTimeSet(void)
{
  ModemAtSendText("AT+SMCONF=\"KEEPTIME\",60");
}

static void OnEntryCleanSessionSet(void)
{
  ModemAtSendText("AT+SMCONF=\"CLEANSS\",1");
}

static void OnEntryQosSet(void)
{
  ModemAtSendText("AT+SMCONF=\"QOS\",%d", QOS);
}

static void OnEntryClientSet(void)
{
  ModemAtSendText("AT+SMCONF=\"CLIENTID\",\"%s\"", xModemMqttClientId);
}

static void OnEntryUserSet(void)
{
  ModemAtSendText("AT+SMCONF=\"USERNAME\",\"%s\"", xModemMqttUsername);
}

static void OnEntryPassSet(void)
{
  ModemAtSendText("AT+SMCONF=\"PASSWORD\",\"%s\"", xModemMqttPassword);
}

static void OnEntryConnect(void)
{
  ModemAtSetTimeoutSendText(AT_CONNECT_TIMEOUT_MSEC, "AT+SMCONN");
}

static void OnExitConnect(void)
{
  if (xModemMqttConnected) {
    PollTimerSet();
  }
}

static void OnEntryDisconnect(void)
{
  PollTimerClear();
  ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, "AT+SMDISC");
}

static void OnExitDisconnect(void)
{
  xModemMqttConnected = false;
}

static void OnEntryConnectedCheck(void)
{
  ModemAtSendText("AT+SMSTATE?");
}

static void OnExitConnectedCheck(void)
{
  if (xModemMqttConnected) {
    PollTimerSet();
  }  
}

static void OnEntrySubscribe(void)
{
  ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, "AT+SMSUB=\"%s\",%d", xModemMqttTopic, QOS);
}

static void OnExitSubscribe(void)
{
  xModemMqttSubscribing = false;
}

static void OnEntryUnsubscribe(void)
{
  ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, "AT+SMUNSUB=\"%s\"", xModemMqttTopic);
}

static void OnExitUnsubscribe(void)
{
  xModemMqttUnsubscribing = false;
}

static void OnEntryPublish(void)
{
  ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, "AT+SMPUB=\"%s\",%d,%d,1", xModemMqttTopic, strlen(publishMsg), QOS);
  //ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, publishMsg);
}

static void OnExitPublish(void)
{
  xModemMqttPublishing = false;
}

/*******************************************************************************
 * Local functions, AT parse
 ******************************************************************************/

static bool AtParse(const char * sz)
{
//  static char msg[MODEM_AT_RX_BUFFER_SIZE];

  int d1;
//  char s1[MQTT_TOPIC_MAX_LENGTH + 1];
//  s1[0] = '\0';  
  
  if (sscanf(sz, "+SMSTATE: %d", &d1) == 1) {
    xModemMqttConnected = (d1 == 1);
    if (xModemMqttConnected) {
      PollTimerSet();
    } else {
      XModemMqtt(xModemMqttConnected);
    }
    return true;
  }
  
  if (StrGE(sz, "+SMSUB: ")) {
    char * t = (char *)&sz[9];
    char * m = t;
    do {
      if (*m == '\"') *m++ = 0;
      if (*m == '{') break;
    } while (*m++);        
    m[strlen(m)-1] = 0;
    XModemMqttGotData(t, m);
    return true;
  }
  
  return false;
}

static bool AtParsePartial(const char * const sz)
{
  //int d1, d2;
  //char s1[MQTT_TOPIC_MAX_LENGTH + 1];
  //s1[0] = '\0';
  
  if (StrGE(sz, ">")) {
    // "AT+SMPUB=\"%s\",%d,%d,1", xModemMqttTopic, strlen(publishMsg), QOS);
    const char * ss = ModemAtPreviousSendText();
    //int i = sscanf(ss, "AT+SMPUB=\"%s\",%d,%d,1", s1, &d1, &d2);
    //if (i == 3) {
    if (StrGE(ss, "AT+SMPUB=")) {
      XModemAtSendText(publishMsg);
      
      ModemLogAtText("<<", sz);
      ModemLogAtText(">>", publishMsg);
      return true;
    }
  }   
  
  return false;
}

static bool AtParseAfterOk(const char * const sz)
{
  if (StrGE(ModemAtPreviousSendText(), "AT+SMCONN")) {
    xModemMqttConnected = true;
    return true;
  }
  
  return false;
}

/*******************************************************************************
 * Local functions, other
 ******************************************************************************/

static void PollTimerSet(void)
{
  EvosEventSetDelta2(pollTimer, MODEM_MQTT_CONNECTED_POLL, 0);
  pollTimeout = false;
}

static void PollTimerClear(void)
{
  EvosEventClear(pollTimer);
  pollTimeout = false;
}

static void PollTimeoutEvent(const EvosEventParam_t param)
{
  ModemLogState("mqtt poll timeout");
  
  pollTimeout = true;
  ModemSmUpdate();
}

/******************************************************************************/
#endif
