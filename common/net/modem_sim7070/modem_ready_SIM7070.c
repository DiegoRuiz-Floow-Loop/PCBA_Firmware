/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <plf/evos/evos.h>
#include <plf/nvm/nvm.h>

#include "modem.h"
#include "modem_ready.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define IMSI_LENGTH   20

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  READY_IDLE,
  READY_CLEAR,
  READY_UART_CHECK,
  READY_ECHO_DISABLE,
  READY_MODEL,
  READY_REVISION,
  READY_IMEI,
  READY_SIM_CHECK,
  READY_SIM_IMSI,
  READY_Last
} ReadyState_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryReadyClear(void);
static void OnEntryReadyUartCheck(void);
static void OnEntryReadyEchoDisable(void);
static void OnExitReadyEchoDisable(void);
static void OnEntryModel(void);
static void OnEntryRevision(void);
static void OnEntryImei(void);
static void OnEntrySimCheck(void);
static void OnEntrySimImsi(void);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t READY_FUNCS[READY_Last] = {
  // .onEntry,               .onExit
  { DoNothing,               DoNothing },               // READY_IDLE
  { OnEntryReadyClear,       DoNothing },               // READY_CLEAR
  { OnEntryReadyUartCheck,   DoNothing },               // READY_UART_CHECK
  { OnEntryReadyEchoDisable, OnExitReadyEchoDisable },  // READY_ECHO_DISABLE
  { OnEntryModel,            DoNothing },               // READY_MODEL
  { OnEntryRevision,         DoNothing },               // READY_REVISION
  { OnEntryImei,             DoNothing },               // READY_IMEI
  { OnEntrySimCheck,         DoNothing },               // READY_SIM_CHECK
  { OnEntrySimImsi,          DoNothing },               // READY_SIM_IMSI
};

static const char * READY_STATE_TEXTS[READY_Last] = {
  "rdy idle",             // READY_IDLE
  "rdy clear",            // READY_CLEAR
  "rdy at check",         // READY_UART_CHECK
  "rdy disable echo",     // READY_ECHO_DISABLE
  "rdy model",            // READY_MODEL
  "rdy revision",         // READY_REVISION
  "rdy imei",             // READY_IMEI
  "rdy sim check",        // READY_SIM_CHECK
  "rdy sim imsi",         // READY_SIM_IMSI
};

static const uint32_t AT_SIM_TIMEOUT_MSEC = 5000;
static const uint32_t AT_IMSI_TIMEOUT_MSEC = 20 * 1000;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t configSave = EVOS_UNINITIALIZED_HANDLE;

static Modem_t cfg;

static char imsi[IMSI_LENGTH];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextReadyState(uint8_t current, uint8_t * next);

// AT parse
static bool AtParse(const char * sz);
static bool AtParseAfterOk(const char * sz);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(const EvosEventParam_t param);
static void ConfigSetDefault(void);
static void ConfigStringCopy(char * cfg, const char * string, uint8_t length);
static uint64_t ImeiToUint64(const char * const imei);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemReadyInit(void)
{
  configSave = EvosEventRegister(ConfigSave, "modem cfg save");

  ConfigLoad();
  xModemModel = cfg.model;
  xModemRevision = cfg.rev;
  xModemImei = cfg.imei;
  xModemImeiNo = ImeiToUint64(cfg.imei);
  xModemSimImsi = imsi;
  
  ModemSmRegister(MODEM_SM_READY, NextReadyState, READY_FUNCS, READY_STATE_TEXTS);
  ModemAtRegister(MODEM_SM_READY, AtParse, NULL, AtParseAfterOk);
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextReadyState(const uint8_t current, uint8_t * const next)
{
  if (xModemAtBusy) return false;

  *next = READY_Last;
  switch (current) {
    case READY_IDLE:
      if (xModemPowerOn && !xModemReadyUart) *next = READY_UART_CHECK;
      else if (!xModemPowerOn && xModemReadyUart) *next = READY_CLEAR;
      break;
    
    case READY_UART_CHECK:
      if (!xModemPowerOn) *next = READY_IDLE;
      else if (xModemAtOkReceived) *next = READY_ECHO_DISABLE;
      else *next = READY_UART_CHECK;
      break;

    case READY_ECHO_DISABLE:
      *next = READY_MODEL;
      break;
    
    case READY_MODEL:
      *next = READY_REVISION;
      break;
    
    case READY_REVISION:
      *next = READY_IMEI;
      break;

    case READY_IMEI:
      *next = READY_SIM_CHECK;
      break;
    
    case READY_SIM_CHECK:
      if (xModemAtOkReceived && xModemReadySim) *next = READY_SIM_IMSI;
      else *next = READY_IDLE;
      break;
    
    case READY_CLEAR:
    case READY_SIM_IMSI:
    default:
      *next = READY_IDLE;
      break;
  }
  return (*next < READY_Last);
}

static void OnEntryReadyClear(void)
{
  xModemReadyUart = false;
  xModemReadySim = false;
}

static void OnEntryReadyUartCheck(void)
{
  ModemAtSendText("AT");
}

static void OnEntryReadyEchoDisable(void)
{
  ModemAtSendText("ATE0");
}

static void OnExitReadyEchoDisable(void)
{
  xModemReadyUart = true;
}

static void OnEntryModel(void)
{
  ModemAtSendText("AT+CGMM");
}

static void OnEntryRevision(void)
{
  ModemAtSendText("AT+CGMR");
}

static void OnEntryImei(void)
{
  ModemAtSendText("AT+CGSN");
}

static void OnEntrySimCheck(void)
{
  ModemAtSetTimeoutSendText(AT_SIM_TIMEOUT_MSEC, "AT+CPIN?");
}

static void OnEntrySimImsi(void)
{
  ModemAtSetTimeoutSendText(AT_IMSI_TIMEOUT_MSEC, "AT+CIMI");
}

/*******************************************************************************
 * Local functions, AT parse
 ******************************************************************************/

static bool AtParse(const char * const sz)
{
  if (StrGE(sz, "+CPIN: READY")) {
    xModemReadySim = true;
    xModemSimNotInserted = false;
    return true;
  }

  if (StrGE(sz, "+CPIN: NOT INSERTED")) {
    xModemSimNotInserted = true;
    XModemSimNotInserted();
    return true;
  }
  
  return false;
}

static bool AtParseAfterOk(const char * const sz)
{
  if (StrGE(ModemAtPreviousSendText(), "AT+CGMM")) {
    ConfigStringCopy(cfg.model, ModemAtPreviousReceivedText(), MODEM_MODEL_LENGTH);
    return true;
  }
  
  if (StrGE(ModemAtPreviousSendText(), "AT+CGMR")) {
    const char * const text = ModemAtPreviousReceivedText();
    uint_fast8_t offset = 0;
    if ((strncmp("Revision:", text, sizeof("Revision:") - 1) == 0)) {
      offset = sizeof("Revision:") - 1;
    }
    
    ConfigStringCopy(cfg.rev, &text[offset], MODEM_REV_LENGTH);
    return true;
  }
  
  if (StrGE(ModemAtPreviousSendText(), "AT+CGSN")) {
    ConfigStringCopy(cfg.imei, ModemAtPreviousReceivedText(), MODEM_IMEI_LENGTH);
    return true;
  }
  
  if (StrGE(ModemAtPreviousSendText(), "AT+CIMI")) {
    strncpy(imsi, ModemAtPreviousReceivedText(), IMSI_LENGTH);
    return true;
  } 
  
  return false;
}

/*******************************************************************************
 * Local functions, configuration
 ******************************************************************************/

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.modem), (uint8_t *)&cfg, sizeof(Modem_t));
    if (cfg.crc != CalcCrc16(offsetof(Modem_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(const EvosEventParam_t param)
{
  cfg.crc = CalcCrc16(offsetof(Modem_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.modem), (uint8_t *)&cfg, sizeof(Modem_t));    
}

static void ConfigSetDefault(void)
{
  cfg.model[0] = '\0';
  cfg.rev[0] = '\0';
  cfg.imei[0] = '\0';
  ConfigSave(0);
}

static void ConfigStringCopy(char * const config, const char * const sz, const uint8_t length)
{
  if (strncmp(config, sz, length) != 0) {
    strncpy(config, sz, length);
    if (config == cfg.imei) {
      xModemImeiNo = ImeiToUint64(cfg.imei);
    }
    EvosEventSetNow(configSave, 0);
  }  
}

static uint64_t ImeiToUint64(const char * const imei)
{
  char * endptr;
  return (uint64_t)strtoull(imei, &endptr, 10);
}

/******************************************************************************/
