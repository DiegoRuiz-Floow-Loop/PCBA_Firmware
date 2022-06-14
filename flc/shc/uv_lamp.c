/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/nvm/nvm.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"
#include "plf/elog/elog.h"

#include "hal/dio/dio.h"

#include "pow/pow.h"

#include "uv_lamp.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define MAX_T_ON_MSEC  (1000u)
#define MIN_T_ON_MSEC  (  10u)

#define MAX_T_OFF_MSEC (1000u)
#define MIN_T_OFF_MSEC (  10u)

#define MAX_RETRIES (    100u)
#define MIN_RETRIES (      1u)

#define DEFAULT_T_ON_MSEC  ( 990u)
#define DEFAULT_T_OFF_MSEC  ( 10u)
#define DEFAULT_RETRIES  		( 30u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static UvLamp_t cfg;

static bool lampTargetIsOn = false;
static bool lampIsOn = false;
static uint16_t retryCounter = 0;

static EvosEventHandle_t lampTask = EVOS_UNINITIALIZED_HANDLE;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/
 
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);
static void LampEvent(const EvosEventParam_t param);
static void SetLampOutput(bool on);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool UvLampInit(const FlcComponentMapping_t * const cfgMap)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "UvLampInit()");
  
  lampTask = EvosEventRegister(LampEvent, "UV lamp task");
  ConfigLoad();
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = UV_LAMP_First; idx < UV_LAMP_Last; idx++) {
    if (cfgMap[idx].type != UV_LAMP_BALLAST) {
      TRACE(TRC_TA_SHC, TRC_TL_FATAL, "UV lamp config invalid");
      EVENT_LOG_ADD_S("UV lamp config invalid");
      return false;
    }
  }
  
  return true;
}

void UvLampSet(const bool on)
{
  if(lampTargetIsOn == on) {
    return;
  }
  
  lampTargetIsOn = on;
  lampIsOn = false;
  retryCounter = 0;
  
  if(lampTargetIsOn) {
    EvosEventSetNow(lampTask, 0);
  } else {
    EvosEventClear(lampTask);
    SetLampOutput(false);
  }
}

bool UvLampIsOn(void)
{
#if defined(EVK)
  return (HAL_DIO_PIN_GET(MP_ITEM_OUT_1) == GPIO_PIN_SET);
#else  
  return (HAL_DIO_PIN_GET(MP_UV_ON) == GPIO_PIN_SET);
#endif  
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static void SetLampOutput(const bool on)
{  
#if defined(EVK)
  HAL_DIO_PIN_WRITE(MP_ITEM_OUT_1, (on ? GPIO_PIN_SET : GPIO_PIN_RESET));
#else
  PowerSet(POWER_UVLAMP, on);
#endif
}

static void LampEvent(const EvosEventParam_t param)
{  
  // Lamp was turned on successfully
  if(UvLampIsOn() && lampIsOn) {
    return;
  }
  
  // Retreies exceeded or lamp is turned off
  if(retryCounter >= cfg.retries || !lampTargetIsOn) {
    TRACE(TRC_TA_SHC, TRC_TL_ERROR, "UV Lamp failed to turn on");
    EVENT_LOG_ADD_S("UV Lamp failed to turn on");
    
    lampTargetIsOn = false;
    SetLampOutput(false);
    return;
  }
  
  lampIsOn = !lampIsOn;  
  SetLampOutput(lampIsOn);
  EvosEventSetDelta2(lampTask, (lampIsOn ? cfg.onTimeMsec : cfg.offTimeMsec), 0);  
  retryCounter += (lampIsOn ? 0 : 1);
}

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.uvLamp), (uint8_t *)&cfg, sizeof(UvLamp_t));
    if (cfg.crc != CalcCrc16(offsetof(UvLamp_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(UvLamp_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.uvLamp), (uint8_t *)&cfg, sizeof(UvLamp_t));
}

static void ConfigSetDefault(void)
{
  cfg.onTimeMsec = DEFAULT_T_ON_MSEC;
  cfg.offTimeMsec = DEFAULT_T_OFF_MSEC;
  cfg.retries = DEFAULT_RETRIES;
  
  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/


static int_fast16_t CliSetOnTime(CliParam_t onMsec, const CliParam_t param2, const CliParam_t param3)
{
  onMsec = MIN_VAL(onMsec, MAX_T_ON_MSEC);
  onMsec = MAX_VAL(onMsec, MIN_T_ON_MSEC);
  
  cfg.onTimeMsec = onMsec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetOffTime(CliParam_t offMsec, const CliParam_t param2, const CliParam_t param3)
{
  offMsec = MIN_VAL(offMsec, MAX_T_OFF_MSEC);
  offMsec = MAX_VAL(offMsec, MIN_T_OFF_MSEC);
  
  cfg.offTimeMsec = offMsec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetRetries(CliParam_t retries, const CliParam_t param2, const CliParam_t param3)
{
  retries = MIN_VAL(retries, MAX_RETRIES);
  retries = MAX_VAL(retries, MIN_RETRIES);
  
  cfg.retries = retries;
  ConfigSave();
  
  return CLI_RESULT_OK;
}


int_fast16_t CliUvLampShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  CliWrite("UV Lamp State..." CLI_NL);
  CliPrintf("  UV Lamp 1: target\t%s\tfeedback:\t%s" CLI_NL, (lampTargetIsOn ? "ON" : "OFF"), (UvLampIsOn() ? "ON" : "OFF"));
  CliPrintf("  On time:\t%i msec" CLI_NL, cfg.onTimeMsec);
  CliPrintf("  Off time:\t%i msec" CLI_NL, cfg.offTimeMsec);
  CliPrintf("  Retries:\t%i" CLI_NL, cfg.retries);
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSet(CliParam_t on, CliParam_t param2, CliParam_t param3)
{
  UvLampSet(on);
  return CLI_RESULT_OK;
}

CLI_START_TABLE(uv_lamp)
  CLI_ENTRY0( "show",  "Show UV Lamp status", CliUvLampShowAll)
  CLI_ENTRY1( "set",   "Set UV Lamp output [on/off]", CliSet, CLI_PARAM_ONOFF)
  CLI_ENTRY1( "ton",   "Set on time [msec:10-1000]", CliSetOnTime, CLI_PARAM_UINT32)
  CLI_ENTRY1( "toff",  "Set off time [msec:10-1000]", CliSetOffTime, CLI_PARAM_UINT32)
  CLI_ENTRY1( "ret",   "Set retries limit [retries:1-100]", CliSetRetries, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(uv_lamp)

/******************************************************************************/
#endif
