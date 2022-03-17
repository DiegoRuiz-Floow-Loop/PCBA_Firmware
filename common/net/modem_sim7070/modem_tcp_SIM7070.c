/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "modem.h"

#if defined(MODEM_TCP_SUPPORT)

#include <stdio.h>

#include <plf/evos/evos.h>

#include "modem_tcp.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  TCP_IDLE,
  TCP_SSL_SET,
  TCP_CONNECT,
  TCP_DISCONNECT,
  TCP_RECEIVE,
  TCP_SEND_CHECK,
  TCP_SEND,
  TCP_Last
} TcpState_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryTcpIdle(void);
static void OnEntryTcpSslSet(void);
static void OnEntryTcpConnect(void);
static void OnEntryTcpDisconnect(void);
static void OnExitTcpDisconnect(void);
static void OnEntryTcpReceive(void);
static void OnEntryTcpSendCheck(void);
static void OnExitTcpSendCheck(void);
static void OnEntryTcpSend(void);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t TCP_FUNCS[TCP_Last] = {
  // .onEntry,               .onExit
  { OnEntryTcpIdle,          DoNothing },               // TCP_IDLE
  { OnEntryTcpSslSet,        DoNothing },               // TCP_SSL_SET
  { OnEntryTcpConnect,       DoNothing },               // TCP_CONNECT
  { OnEntryTcpDisconnect,    OnExitTcpDisconnect },     // TCP_DISCONNECT
  { OnEntryTcpReceive,       DoNothing },               // TCP_RECEIVE
  { OnEntryTcpSendCheck,     OnExitTcpSendCheck },      // TCP_SEND_CHECK
  { OnEntryTcpSend,          DoNothing },               // TCP_SEND
};

static const char * TCP_STATE_TEXTS[TCP_Last] = {
  "tcp idle",             // TCP_IDLE
  "tcp ssl set",          // TCP_SSL_SET
  "tcp connect",          // TCP_CONNECT
  "tcp disconnect",       // TCP_DISCONNECT
  "tcp rx",               // TCP_RECEIVE
  "tcp tx check buffer",  // TCP_SEND_CHECK
  "tcp tx",               // TCP_SEND
};

static const uint32_t AT_CONNECT_TIMEOUT_MSEC = 75 * 1000;
static const uint32_t AT_SEND_TIMEOUT_MSEC = 5000;

static const uint32_t TX_BUFFER_RETRY_MSEC = 100;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static bool timeout;
static bool connectRequired;
static bool disconnectRequired;
static bool socketInUse;

static const uint8_t * tcpTx;
static uint_fast16_t tcpTxLength;
static uint_fast16_t tcpTxIdx;

static uint_fast16_t modemTxFree;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextTcpState(uint8_t current, uint8_t * next);

// AT parse
static bool AtParse(const char * sz);
static bool AtParsePartial(const char * sz);
static bool AtParseAfterOk(const char * sz);

// Other
static void TimerSet(uint32_t msec);
static void TimerClear(void);
static void TimeoutEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemTcpInit(void)
{
  timer = EvosEventRegister(TimeoutEvent, "modem tcp timer");
  
  ModemSmRegister(MODEM_SM_TCP, NextTcpState, TCP_FUNCS, TCP_STATE_TEXTS);
  ModemAtRegister(MODEM_SM_TCP, AtParse, AtParsePartial, AtParseAfterOk);
}

bool ModemTcpOpen(const char * host, uint16_t port)
{
  if (!xModemNetConnected || xModemTcpLocked) return false;
  
  connectRequired = true;
  xModemTcpLocked = true;
  xModemTcpHost = host;
  xModemTcpPort = port;
  ModemSmUpdate();
  return true;
}

bool ModemTcpClose(void)
{
  if (!xModemTcpConnected) {
    connectRequired = false;
    disconnectRequired = false;
    return false;
  }
  
  if (xModemTcpLocked) {
    connectRequired = false;
    disconnectRequired = true;
    ModemSmUpdate();
  }
  return true;
}

bool ModemTcpSendData(const uint8_t * data, uint16_t length)
{
  if (!xModemTcpConnected) return false;
  
  xModemTcpDataSending = true;
  tcpTx = data;
  tcpTxLength = length;
  tcpTxIdx = 0;
  ModemSmUpdate();
  return false;
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextTcpState(const uint8_t current, uint8_t * const next)
{
  if (xModemAtBusy) return false;
  
  *next = TCP_Last;
  switch (current) {
    case TCP_IDLE:
      if ((!xModemTcpConnected || !xModemNetConnected) && socketInUse) *next = TCP_DISCONNECT;
      else if (!xModemNetConnected) break;
      else if (!xModemTcpConnected && connectRequired) *next = TCP_SSL_SET;
      else if (!xModemTcpConnected) break;
      else if (disconnectRequired) *next = TCP_DISCONNECT;  
      else if (xModemTcpDataReceiving) *next = TCP_RECEIVE;
      else if (xModemTcpDataSending) *next = TCP_SEND_CHECK;
      break;
    
    case TCP_SSL_SET:
      if (xModemAtErrorReceived) *next = TCP_DISCONNECT;
      else *next = TCP_CONNECT;
      break;
    
    case TCP_CONNECT:
    case TCP_DISCONNECT:      
      *next = TCP_IDLE;
      break;
   
    case TCP_RECEIVE:
      if (xModemAtErrorReceived) *next = TCP_IDLE;
      else if (xModemTcpDataReceiving) *next = TCP_RECEIVE;
      else *next = TCP_IDLE;
      break;
    
    case TCP_SEND_CHECK:
      if (xModemAtErrorReceived) *next = TCP_IDLE;
      else if (modemTxFree > 0) *next = TCP_SEND;
      else if (timeout) *next = TCP_SEND_CHECK;
      break;
    
    case TCP_SEND:
      if (xModemAtErrorReceived) *next = TCP_IDLE;
      else if (xModemTcpDataSending && (modemTxFree > 0)) *next = TCP_SEND;
      else if (xModemTcpDataSending) *next = TCP_SEND_CHECK;
      else *next = TCP_IDLE;
      break;
    
    default:
      break;
  }
  return (*next < TCP_Last);
}

static void OnEntryTcpIdle(void)
{
  if (!xModemTcpConnected) {
    xModemTcpDataSending = false;
    xModemTcpLocked = socketInUse;
  }
  
  XModemTcp(xModemTcpConnected);
}

static void OnEntryTcpSslSet(void)
{
  // Disable SSL
  ModemAtSendText("AT+CASSLCFG=0,\"SSL\",0");
}

static void OnEntryTcpConnect(void)
{
  ModemAtSetTimeoutSendText(AT_CONNECT_TIMEOUT_MSEC, "AT+CAOPEN=0,0,\"TCP\",\"%s\",%u", xModemTcpHost, xModemTcpPort);
  connectRequired = false;  
}

static void OnEntryTcpDisconnect(void)
{
  ModemAtSendText("AT+CACLOSE=0");
  disconnectRequired = false;  
}

static void OnExitTcpDisconnect(void)
{
  socketInUse = false;
  xModemTcpConnected = false;
}

static void OnEntryTcpReceive(void)
{
  ModemAtSendText("AT+CARECV=0,%u", TCP_RX_MAX_LENGTH);
}

static void OnEntryTcpSendCheck(void)
{
  modemTxFree = 0;
  ModemAtSendText("AT+CASEND=0");
  TimerSet(TX_BUFFER_RETRY_MSEC);
}

static void OnExitTcpSendCheck(void)
{
  TimerClear();
}

static void OnEntryTcpSend(void)
{
  uint_fast16_t length = MIN_VAL(modemTxFree, (tcpTxLength - tcpTxIdx));
  length = MIN_VAL(length, TCP_TX_MAX_LENGTH);
  
  ModemAtSetTimeoutSendText(AT_SEND_TIMEOUT_MSEC, "AT+CASEND=0,%u", length);
  
  modemTxFree -= length;
}

/*******************************************************************************
 * Local functions, AT parse
 ******************************************************************************/

static bool AtParse(const char * const sz)
{
  int d1;

  if (sscanf(sz, "+CAOPEN: 0,%d", &d1) == 1) {
    if (d1 == 0) {
      socketInUse = true;
      xModemTcpConnected = true;
    }
    return true;
  }
  
  if (sscanf(sz, "+CASTATE: 0,%d", &d1) == 1) {
    xModemTcpConnected = (d1 == 1);
    if (!xModemTcpConnected) {
      xModemTcpDataReceiving = false;
      xModemTcpDataSending = false;
    }
    return true;
  }
  
  if (StrGE(sz, "+CADATAIND: 0")) {
    xModemTcpDataReceiving = true;
    return true;
  }
  
  if (StrGE(sz, "+CARECV: 0")) {
    xModemTcpDataReceiving = false;
    return true;
  }
  
  if (sscanf(sz, "+CASEND: 0,%d", &d1) == 1) {
    return true;
  } else if (sscanf(sz, "+CASEND: %d", &d1) == 1) {
    // Testing in development showed that the SIM7070G had a TCP tx buffer size
    // of 1460 bytes. First time filling the buffer with 1460 bytes works but 
    // sending another 1460 bytes right after failed even if the modem reported 
    // 1460 bytes free.
    // A solution found by testing was to always substract 1 from the TCP tx 
    // buffer free bytes count and then the second time sending 1460 - 1 bytes
    // always worked.
    modemTxFree = ((d1 > 0) ? (d1 - 1) : 0);
    return true;
  }
  
  return false;
}

static bool AtParsePartial(const char * const sz)
{
  // No overall logging of partial AT commands.
  // Each AT command recognized in this function handles the logging of the
  // received AT command.
  
  int d1;

  if (StrGE(sz, "> ")) {
    if (sscanf(ModemAtPreviousSendText(), "AT+CASEND=0,%d", &d1) == 1) {
      XModemAtSendData(&tcpTx[tcpTxIdx], d1);
      
      ModemLogAtText("<<", sz);
      ModemLogBinaryData(">>", &tcpTx[tcpTxIdx], d1);
      
      tcpTxIdx += d1;
      if (tcpTxIdx >= tcpTxLength) {
        xModemTcpDataSending = false;
      }
      return true;      
    }
  } 
  
  if (sscanf(sz, "+CARECV: %d", &d1) == 1) {
    ModemLogAtText("<<", sz);
    
    xModemTcpDataReceiving = (d1 >= TCP_RX_MAX_LENGTH);
    ModemTcpBinaryDataReceive((uint16_t)d1);
    return true;
  }
  
  return false;
}

static bool AtParseAfterOk(const char * const sz)
{
  if (StrGE(ModemAtPreviousSendText(), "AT+CACLOSE=0")) {
    xModemTcpConnected = false;
    xModemTcpDataReceiving = false;
    xModemTcpDataSending = false;
    return true;
  }
  
  return false;
}

/*******************************************************************************
 * Local functions, other
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
  ModemLogState("tcp timeout");
  
  timeout = true;
  ModemSmUpdate();
}

/******************************************************************************/
#endif
