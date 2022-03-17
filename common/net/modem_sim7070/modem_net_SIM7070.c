/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <stdio.h>
#include <string.h>

#include "modem.h"  // includes also modem_cfg.h

#include <plf/evos/evos.h>
#if defined(MODEM_NTP_SYNCHRONIZE)
#include <hal/rtc/rtc.h>
#endif

#include "modem_net.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define NET_INFO_LENGTH     48
#define NET_MODE_LENGTH     12
#define NET_APN_LENGTH      48

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  NET_IDLE,
  NET_SIGNAL_WAIT,
  NET_SIGNAL_CHECK,
  NET_SERVICE_CHECK,
  NET_INFO_QUERY,
  NET_APN_QUERY,
  NET_APN_SET,
  NET_CONNECT,
  NET_DISCONNECT,
#if defined(MODEM_NTP_SYNCHRONIZE)  
  NET_NTP_SETUP,  
  NET_NTP_SYNC,
#endif  
  NET_Last
} NetState_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryNetIdle(void);
static void OnEntryNetSignalWait(void);
static void OnExitNetSignalWait(void);
static void OnEntryNetSignalCheck(void);
static void OnEntryNetServiceCheck(void);
static void OnEntryNetInfoQuery(void);
static void OnEntryNetApnQuery(void);
static void OnEntryNetApnSet(void);
static void OnEntryNetConnect(void);
static void OnEntryNetDisconnect(void);
#if defined(MODEM_NTP_SYNCHRONIZE)
static void OnEntryNtpSetup(void);
static void OnEntryNtpSync(void);
#endif

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t NET_FUNCS[NET_Last] = {
  // .onEntry,               .onExit
  { OnEntryNetIdle,          DoNothing },               // NET_IDLE
  { OnEntryNetSignalWait,    OnExitNetSignalWait },     // NET_SIGNAL_WAIT
  { OnEntryNetSignalCheck,   DoNothing },               // NET_SIGNAL_CHECK
  { OnEntryNetServiceCheck,  DoNothing },               // NET_SERVICE_CHECK
  { OnEntryNetInfoQuery,     DoNothing },               // NET_INFO_QUERY
  { OnEntryNetApnQuery,      DoNothing },               // NET_APN_QUERY
  { OnEntryNetApnSet,        DoNothing },               // NET_APN_SET
  { OnEntryNetConnect,       DoNothing },               // NET_CONNECT 
  { OnEntryNetDisconnect,    DoNothing },               // NET_DISCONNECT   
#if defined(MODEM_NTP_SYNCHRONIZE)  
  { OnEntryNtpSetup,         DoNothing },               // NET_NTP_SETUP
  { OnEntryNtpSync,          DoNothing },               // NET_NTP_SYNC  
#endif  
};

static const char * NET_STATE_TEXTS[NET_Last] = {
  "net idle",             // NET_IDLE
  "net signal wait",      // NET_SIGNAL_WAIT
  "net signal check",     // NET_SIGNAL_CHECK
  "net service check",    // NET_SERVICE_CHECK
  "net info query",       // NET_INFO_QUERY
  "net apn query",        // NET_APN_QUERY
  "net apn config",       // NET_APN_SET
  "net connect",          // NET_CONNECT
  "net disconnect",       // NET_DISCONNECT  
#if defined(MODEM_NTP_SYNCHRONIZE)  
  "net ntp setup",        // NET_NTP_SETUP
  "net ntp sync",         // NET_NTP_SYNC  
#endif
};

static const uint32_t POLL_WAIT_MSEC = 1000;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static bool connectRequired;
static bool disconnectRequired;
static bool timeout;
#if defined(MODEM_NTP_SYNCHRONIZE)    
static bool ntpRequired = true;
#endif

static char netInfo[NET_INFO_LENGTH];
static char netMode[NET_MODE_LENGTH];
static char netApn[NET_APN_LENGTH];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextNetState(uint8_t current, uint8_t * next);

// AT parse
static bool AtParse(const char * sz);

// Timer
static void TimerSet(uint32_t msec);
static void TimerClear(void);
static void TimeoutEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemNetInit(void)
{
  timer = EvosEventRegister(TimeoutEvent, "modem net timer");
  
  xModemNetInfo = netInfo;
  xModemNetMode = netMode;
  xModemNetApn = netApn;
  
  ModemSmRegister(MODEM_SM_NET, NextNetState, NET_FUNCS, NET_STATE_TEXTS);
  ModemAtRegister(MODEM_SM_NET, AtParse, NULL, NULL);
}

bool ModemNetConnect(void)
{
  if (!xModemPowerOn || xModemNetLocked) return false;
  
  connectRequired = true;
  xModemNetLocked = true;
  ModemSmUpdate();
  return true;
}

bool ModemNetDisconnect(void)
{
  if (ModemProtocolLocked()) return false;
  
  if (xModemNetLocked) {
    connectRequired = false;
    disconnectRequired = true;
    ModemSmUpdate();
  }  
  return true;
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextNetState(const uint8_t current, uint8_t * const next)
{
  if (xModemAtBusy) return false;
  
  *next = NET_Last;
  switch (current) {
    case NET_IDLE:
      if (!xModemReadyUart && (!xModemNetConnected != !xModemNetLocked)) *next = NET_IDLE;
      if (!xModemReadyUart || !xModemReadySim) break;
      else if (!xModemNetConnected && connectRequired) *next = NET_SIGNAL_CHECK;
      else if (!xModemNetConnected != !xModemNetLocked) *next = NET_IDLE;
      else if (xModemNetConnected && disconnectRequired) *next = NET_DISCONNECT;
#if defined(MODEM_NTP_SYNCHRONIZE)        
      else if (xModemNetConnected && ntpRequired) *next = NET_NTP_SETUP;
#endif    
      break;
    
    case NET_SIGNAL_WAIT:
      if (!xModemReadyUart) *next = NET_IDLE;
      else if (timeout) *next = NET_SIGNAL_CHECK;
      break;

    case NET_SIGNAL_CHECK:
      if (xModemAtOkReceived && ((xModemNetRssi > 0) && (xModemNetRssi != MODEM_RSSI_NOT_DETECTABLE))) *next = NET_SERVICE_CHECK;
      else *next = NET_SIGNAL_WAIT;
      break;
    
    case NET_SERVICE_CHECK:
      if (xModemAtOkReceived && xModemNetServiceAttached) *next = NET_INFO_QUERY;
      else *next = NET_SIGNAL_WAIT;
      break;
    
    case NET_INFO_QUERY:
      if (xModemAtOkReceived && (xModemNetInfo[0] != '\0')) *next = NET_APN_QUERY;
      else *next = NET_IDLE;
      break;
    
    case NET_APN_QUERY:
      if (xModemAtOkReceived && (xModemNetApn[0] != '\0')) *next = NET_APN_SET;
      else *next = NET_IDLE;
      break;
    
    case NET_APN_SET:
      if (xModemAtOkReceived) *next = NET_CONNECT;
      else *next = NET_IDLE;
      break;

#if defined(MODEM_NTP_SYNCHRONIZE)    
    case NET_NTP_SETUP:
      if (xModemAtOkReceived) *next = NET_NTP_SYNC;
      else *next = NET_IDLE;
      break;
    
    case NET_NTP_SYNC:
      *next = NET_IDLE;
      break;      
#endif    
      
    case NET_CONNECT:
    case NET_DISCONNECT:
    default:      
      *next = NET_IDLE;
      break;
  }
  return (*next < NET_Last);
}

static void OnEntryNetIdle(void)
{
  connectRequired = false;
  disconnectRequired = false;
  
  xModemNetLocked = xModemNetConnected;
  if (!xModemNetConnected) {
#if defined(MODEM_NTP_SYNCHRONIZE)        
    ntpRequired = true;
#endif    
  }
  
  XModemNet(xModemNetConnected);
}

static void OnEntryNetSignalWait(void)
{
  TimerSet(POLL_WAIT_MSEC);
}

static void OnExitNetSignalWait(void)
{
  TimerClear();
}

static void OnEntryNetSignalCheck(void)
{
  ModemAtSendText("AT+CSQ");
}

static void OnEntryNetServiceCheck(void)
{
  ModemAtSendText("AT+CGATT?");
}

static void OnEntryNetInfoQuery(void)
{
  ModemAtSendText("AT+COPS?");
}

static void OnEntryNetApnQuery(void)
{
  ModemAtSendText("AT+CGNAPN");
}

static void OnEntryNetApnSet(void)
{
  ModemAtSendText("AT+CNCFG=0,1,%s", xModemNetApn);
}

static void OnEntryNetConnect(void)
{
  ModemAtSendText("AT+CNACT=0,1");
  connectRequired = false;
}

static void OnEntryNetDisconnect(void)
{
  ModemAtSendText("AT+CNACT=0,0");
  disconnectRequired = false;
}

#if defined(MODEM_NTP_SYNCHRONIZE)
static void OnEntryNtpSetup(void)
{
  // NTP mode 1 is set so NTP result is outputted to AT interface and not 
  // updating modem localtime. This part of a work-around for when the modem 
  // returns localtime instead of the NTP sync time.
  ModemAtSendText("AT+CNTP=\"pool.ntp.org\",0,0,1");
  ntpRequired = false;
}

static void OnEntryNtpSync(void)
{
  ModemAtSendText("AT+CNTP");
}
#endif

/*******************************************************************************
 * Local functions, AT parse
 ******************************************************************************/

static bool AtParse(const char * const sz)
{
  int d1, d2, d3;
#if defined(MODEM_NTP_SYNCHRONIZE)      
  int d4, d5, d6;
#endif  
  char s1[129];
  s1[0] = '\0';    

  if (sscanf(sz, "+CSQ: %d,%d", &d1, &d2) == 2) {
    xModemNetRssi = (uint8_t)d1;
    xModemNetBer = (uint8_t)d2;
    return true;
  }
  
  if (sscanf(sz, "+CGATT: %d", &d1) == 1) {
    xModemNetServiceAttached = !!d1;
    return true;
  }
  
  if (sscanf(sz, "+COPS: %d,%d,%[^,],%d", &d1, &d2, s1, &d3) == 4) {
    strncpy(netInfo, s1, sizeof(netInfo));
    if (d3 == 3) {
      strncpy(netMode, "EGPRS", sizeof(netMode));
    } else if (d3 == 7) {
      strncpy(netMode, "LTE M1", sizeof(netMode));
    } else if (d3 == 9) {
      strncpy(netMode, "LTE NB", sizeof(netMode));
    } else {
      strncpy(netMode, "", sizeof(netMode));
    }
    return true;
  }

  if (sscanf(sz, "+CGNAPN: %d,%s", &d1, s1) == 2) {
    if (!d1) {
      s1[0] = '\0'; 
    } else {
      strncpy(netApn, s1, sizeof(netApn));
    }
    return true;
  }
  
  if (sscanf(sz, "+APP PDP: %d,%s", &d1, s1) == 2) {
    xModemNetConnected = (strcmp("ACTIVE", s1) == 0);
    return true;
  }
  
#if defined(MODEM_NTP_SYNCHRONIZE)  
  if (sscanf(sz, "+CNTP: 1,\"%u/%u/%u,%u:%u:%u\"", &d1, &d2, &d3, &d4, &d5, &d6) == 6) {
    // SIM7070 modem sometimes returns localtime instead of the ntp sync time.
    // As a work-around years lower than 2020 is ignored. The SIM7070G starts
    // with year 1980 after a power up.
    if (d1 >= 2020) {
      RtcSetItems(d1, d2, d3, d4, d5, d6);
    }
    return true;
  }
#endif  
  
  return false;
}

/*******************************************************************************
 * Local functions, timer
 ******************************************************************************/

static void TimerSet(const uint32_t msec)
{
  EvosEventSetDelta2(timer, msec, 0);
  timeout = false;
}

static void TimerClear(void)
{
  EvosEventClear(timer);
  timeout = false;  
}

static void TimeoutEvent(const EvosEventParam_t param)
{
  ModemLogState("net timeout");
  timeout = true;
  ModemSmUpdate();
}

/******************************************************************************/
