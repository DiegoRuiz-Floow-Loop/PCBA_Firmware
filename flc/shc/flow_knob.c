/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <string.h>

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"
#include "plf/nvm/nvm.h"

#include "app/flc_cfg.h"

#include "sup/pos_sensor.h"

#include "flow_knob.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define PERMIL_MAX  (1000u)

// The position sensor ADC input is measured every 100 ms, so the flow knob 
// update interval is setup to be atleast 150 ms to ensure the buffered ADC 
// input value is ready.
#define KNOB_UPDATE_INTERVAL_MSEC     ( 150u)

#define DEFAULT_POS_LIMIT_MIN_PERMIL  ( 150u)
#define DEFAULT_POS_LIMIT_MAX_PERMIL  ( 850u)
    
#define DEFAULT_POS_HEAD_MAX_PERMIL   ( 750u)
#define DEFAULT_POS_HEAD_ON_PERMIL    ( 550u)
#define DEFAULT_POS_HEAD_OFF_PERMIL   ( 530u)
                                      
#define DEFAULT_POS_HAND_MAX_PERMIL   ( 250u)
#define DEFAULT_POS_HAND_ON_PERMIL    ( 450u)
#define DEFAULT_POS_HAND_OFF_PERMIL   ( 470u)
                                  
#define DEFAULT_POWER_MAX_PERMIL      ( 700u)
#define DEFAULT_POWER_MIN_PERMIL      (  50u)

#define KNOB_TL                       TRC_TL_3
#define KNOB_TRC_POWER_CHANGE_PERMIL  (20)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  KNOB_HEAD,
  KNOB_HAND,
  KNOB_Last
} Knob_t;

typedef struct {
  const char * const name;
  struct {
    const uint16_t * const pOffPermil;
    const uint16_t * const pOnPermil;
    const uint16_t * const pMaxPermil;
  } pos;
  bool isOn;
  uint16_t powerPermil;
} KnobValues_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static FlowKnob_t cfg;

static KnobValues_t knobs[] = {
  { .name = "head", .pos.pOffPermil = &cfg.positionHeadOffPermil, .pos.pOnPermil = &cfg.positionHeadOnPermil, .pos.pMaxPermil = &cfg.positionHeadMaxPermil }, // KNOB_HEAD
  { .name = "hand", .pos.pOffPermil = &cfg.positionHandOffPermil, .pos.pOnPermil = &cfg.positionHandOnPermil, .pos.pMaxPermil = &cfg.positionHandMaxPermil }, // KNOB_HAND
};
static_assert(SIZEOF_ARRAY(knobs) == KNOB_Last, "Wrong table row count!");

static uint16_t sensorPosPermil;
static bool sensorIsOutOfBounds;

static EvosEventHandle_t handleTimer = EVOS_UNINITIALIZED_HANDLE;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// General
static void SensorUpdateEvent(EvosEventParam_t param);
static uint16_t SensorGetPosPermil(void);
static bool PosIsOutOfBounds(uint16_t permil);
static void KnobUpdate(KnobValues_t * knob);
static void KnobUpdateNormalDirection(KnobValues_t * knob);
static void KnobUpdateReverseDirection(KnobValues_t * knob);
static void KnobSetStatus(KnobValues_t * knob, bool isOn, uint16_t powerPermil);
static uint16_t KnobLinearInterpolation(uint16_t in, uint16_t inMax, uint16_t inMin);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void FlowKnobInit(void)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "FlowKnobInit()");
  
  ConfigLoad();
  
  handleTimer = EvosEventRegister(SensorUpdateEvent, "flow knob timer");
  EvosEventSetAndReload(handleTimer, KNOB_UPDATE_INTERVAL_MSEC, KNOB_UPDATE_INTERVAL_MSEC, 0);
}

bool FlowKnobIsHeadOn(void)
{
  return knobs[KNOB_HEAD].isOn;
}  

uint16_t FlowKnobGetHeadPowerPermil(void)
{
  return knobs[KNOB_HEAD].powerPermil;
}

bool FlowKnobIsHandOn(void)
{
  return knobs[KNOB_HAND].isOn;
}  

uint16_t FlowKnobGetHandPowerPermil(void)
{
  return knobs[KNOB_HAND].powerPermil;
}

__WEAK void XFlowKnobOutOfBoundsChanged(const bool isOutOfBounds)
{
}

/*******************************************************************************
 * Local functions, general
 ******************************************************************************/

static void SensorUpdateEvent(const EvosEventParam_t param)
{
  sensorPosPermil = SensorGetPosPermil();
  
  const bool updateIsOutOfBounds = PosIsOutOfBounds(sensorPosPermil);
  if (BOOL_NOT_EQUAL(updateIsOutOfBounds, sensorIsOutOfBounds)) {
    sensorIsOutOfBounds = updateIsOutOfBounds;
    XFlowKnobOutOfBoundsChanged(sensorIsOutOfBounds);
  }
 
  KnobUpdate(&knobs[KNOB_HEAD]);
  KnobUpdate(&knobs[KNOB_HAND]);
}

static uint16_t SensorGetPosPermil(void)
{
  const FlcHwComponent_t hwIdx = FlcCfgAppToHwComponent(FLOW_KNOB_POSITION_SENSOR);
  return PosSensorGetPermil(hwIdx);
}

static bool PosIsOutOfBounds(const uint16_t permil)
{
  return ((permil < cfg.positionLimitMinPermil) || (permil > cfg.positionLimitMaxPermil));
}

static void KnobUpdate(KnobValues_t * const knob)
{
  if (*knob->pos.pMaxPermil >= *knob->pos.pOffPermil) {
    KnobUpdateNormalDirection(knob);
  } else {
    KnobUpdateReverseDirection(knob);
  }
}

static void KnobUpdateNormalDirection(KnobValues_t * const knob)
{
  bool isOn;
  uint16_t powerPermil;
  
  // Sensor position is out of range
  if (sensorIsOutOfBounds || (sensorPosPermil <= *knob->pos.pOffPermil)) {
    isOn = false;
    powerPermil = 0;
  }
  // Sensor position is in on range
  else if (sensorPosPermil >= *knob->pos.pOnPermil) {
    isOn = true;
    
    if (sensorPosPermil >= *knob->pos.pMaxPermil) {
      powerPermil = cfg.powerMaxPermil;
    } else {
      powerPermil = cfg.powerMinPermil + KnobLinearInterpolation(sensorPosPermil, *knob->pos.pMaxPermil, *knob->pos.pOnPermil);
    }
  }
  // Sensor position is in hysteresis range
  else {
    isOn = knob->isOn;
    powerPermil = (knob->isOn ? cfg.powerMinPermil : 0);
  }
  
  KnobSetStatus(knob, isOn, powerPermil);
}

static void KnobUpdateReverseDirection(KnobValues_t * const knob)
{
  bool isOn;
  uint16_t powerPermil;
  
  // Sensor position is out of range
  if (sensorIsOutOfBounds || (sensorPosPermil >= *knob->pos.pOffPermil)) {
    isOn = false;
    powerPermil = 0;
  }
  // Sensor position is in on range
  else if (sensorPosPermil <= *knob->pos.pOnPermil) {
    isOn = true;
    
    if (sensorPosPermil <= *knob->pos.pMaxPermil) {
      powerPermil = cfg.powerMaxPermil;
    } else {
      powerPermil = cfg.powerMaxPermil - KnobLinearInterpolation(sensorPosPermil, *knob->pos.pOnPermil, *knob->pos.pMaxPermil);
    }
  }
  // Sensor position is in hysteresis range
  else {
    isOn = knob->isOn;
    powerPermil = (knob->isOn ? cfg.powerMinPermil : 0);
  }
  
  KnobSetStatus(knob, isOn, powerPermil);
}

static void KnobSetStatus(KnobValues_t * const knob, const bool isOn, const uint16_t powerPermil)
{
  // The reason for this function is to determine when to trace output knob changes
  
  const bool isStateChanged = BOOL_NOT_EQUAL(knob->isOn, isOn);
  const bool isPowerChanged = (abs(((int)knob->powerPermil) - ((int)powerPermil)) >= KNOB_TRC_POWER_CHANGE_PERMIL);

  knob->isOn = isOn;
  knob->powerPermil = powerPermil;
  
  if (!isStateChanged && !isPowerChanged) {
    return;
  }
 
  TRACE_VA(TRC_TA_SHC, KNOB_TL, "knob %s %s  power %3u%%  pos %3u%%", 
                                knob->name, 
                                (knob->isOn ? "on " : "off"),
                                knob->powerPermil / 10u,
                                sensorPosPermil / 10u);
}

static uint16_t KnobLinearInterpolation(const uint16_t in, const uint16_t inMax, const uint16_t inMin)
{
  //          IN - IN_MIN
  // OUT = ----------------- * (POWER_PERMIL_MAX - POWER_PERMIL_MIN)
  //        IN_MAX - IN_MIN
  uint32_t calc = in - inMin;
  calc *= cfg.powerMaxPermil - cfg.powerMinPermil;
  calc /= inMax - inMin;
  return (uint16_t)calc;
}

/*******************************************************************************
 * Local functions, configuration
 ******************************************************************************/

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.flowKnob), (uint8_t *)&cfg, sizeof(FlowKnob_t));
    if (cfg.crc != CalcCrc16(offsetof(FlowKnob_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(FlowKnob_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.flowKnob), (uint8_t *)&cfg, sizeof(FlowKnob_t));
}

static void ConfigSetDefault(void)
{
  cfg.positionLimitMaxPermil = DEFAULT_POS_LIMIT_MAX_PERMIL;
  cfg.positionLimitMinPermil = DEFAULT_POS_LIMIT_MIN_PERMIL;
  
  cfg.positionHeadMaxPermil  = DEFAULT_POS_HEAD_MAX_PERMIL;
  cfg.positionHeadOnPermil   = DEFAULT_POS_HEAD_ON_PERMIL;
  cfg.positionHeadOffPermil  = DEFAULT_POS_HEAD_OFF_PERMIL;
  
  cfg.positionHandMaxPermil  = DEFAULT_POS_HAND_MAX_PERMIL;
  cfg.positionHandOnPermil   = DEFAULT_POS_HAND_ON_PERMIL;
  cfg.positionHandOffPermil  = DEFAULT_POS_HAND_OFF_PERMIL;
  
  cfg.powerMaxPermil    = DEFAULT_POWER_MAX_PERMIL;
  cfg.powerMinPermil    = DEFAULT_POWER_MIN_PERMIL;
  
  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

static void PrintKnobStatus(const char * const text, const bool isOn, const uint16_t permil)
{
  CliPrintf("  Knob   :\t%s %s  \tpower %4u permil" CLI_NL,
            text,
            (isOn ? "on " : "off"),
            permil);
} 

int_fast16_t CliFlowKnobShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Flow Knob Controller..." CLI_NL);
  CliPrintf("  Sensor :\tlimit %s\tpos   %4u permil" CLI_NL, 
            (sensorIsOutOfBounds ? "error" : "ok   "),
            sensorPosPermil);

  PrintKnobStatus("head", FlowKnobIsHeadOn(), FlowKnobGetHeadPowerPermil());
  PrintKnobStatus("hand", FlowKnobIsHandOn(), FlowKnobGetHandPowerPermil());
  
  return CLI_RESULT_OK;
}

static void PrintKnobConfig(const char * const text, const uint16_t offPermil, const uint16_t onPermil, const uint16_t maxPermil)
{
  CliPrintf("  Pos %s :\toff %3u permil\ton  %3u permil\tmax %3u permil" CLI_NL, 
            text,
            offPermil,
            onPermil,
            maxPermil);
}

static int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Flow Knob Config..." CLI_NL);
  
  PrintKnobConfig("head", cfg.positionHeadOffPermil, cfg.positionHeadOnPermil, cfg.positionHeadMaxPermil);
  PrintKnobConfig("hand", cfg.positionHandOffPermil, cfg.positionHandOnPermil, cfg.positionHandMaxPermil);

  CliPrintf("  Pos limit:\tmin %3u permil\tmax %3u permil" CLI_NL, 
            cfg.positionLimitMinPermil, 
            cfg.positionLimitMaxPermil);
  
  CliPrintf("  Power    :\tmin %3u permil\tmax %3u permil" CLI_NL, 
            cfg.powerMinPermil, 
            cfg.powerMaxPermil);  

  return CLI_RESULT_OK;
}

static int_fast16_t CliSetKnobPosOff(const CliParam_t knob, CliParam_t permil, const CliParam_t param3)
{
  permil = MIN_VAL(permil, PERMIL_MAX);
  
  if (strcmp("head", (char *)knob) == 0) {
    cfg.positionHeadOffPermil = permil;
  } else if (strcmp("hand", (char *)knob) == 0) {
    cfg.positionHandOffPermil = permil;
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  

  ConfigSave();
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetKnobPosOn(const CliParam_t knob, CliParam_t permil, const CliParam_t param3)
{
  permil = MIN_VAL(permil, PERMIL_MAX);
  
  if (strcmp("head", (char *)knob) == 0) {
    cfg.positionHeadOnPermil = permil;
  } else if (strcmp("hand", (char *)knob) == 0) {
    cfg.positionHandOnPermil = permil;
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  

  ConfigSave();
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetKnobPosMax(const CliParam_t knob, CliParam_t permil, const CliParam_t param3)
{
  permil = MIN_VAL(permil, PERMIL_MAX);
  
  if (strcmp("head", (char *)knob) == 0) {
    cfg.positionHeadMaxPermil = permil;
  } else if (strcmp("hand", (char *)knob) == 0) {
    cfg.positionHandMaxPermil = permil;
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  

  ConfigSave();
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetKnobLimit(const CliParam_t setting, CliParam_t permil, const CliParam_t param3)
{
  permil = MIN_VAL(permil, PERMIL_MAX);
  
  if (strcmp("min", (char *)setting) == 0) {
    cfg.positionLimitMinPermil = permil;
  } else if (strcmp("max", (char *)setting) == 0) {
    cfg.positionLimitMaxPermil = permil;
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  

  ConfigSave();
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetKnobPower(const CliParam_t setting, CliParam_t permil, const CliParam_t param3)
{
  permil = MIN_VAL(permil, PERMIL_MAX);
  
  if (strcmp("min", (char *)setting) == 0) {
    cfg.powerMinPermil = permil;
  } else if (strcmp("max", (char *)setting) == 0) {
    cfg.powerMaxPermil = permil;
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  

  ConfigSave();
  return CLI_RESULT_OK;
}

CLI_START_TABLE(flow_knob)
  CLI_ENTRY0( "show",   "Show flow knob status", CliFlowKnobShowAll)
  CLI_ENTRY0( "scfg",   "Show flow knob configuration", CliShowConfig)

  CLI_ENTRY2( "lim",    "Set min/max allowed knob position limits [cfg:min/max, pos:0-1000]", CliSetKnobLimit, CLI_PARAM_STRING, CLI_PARAM_UINT32)

  CLI_ENTRY2( "off",    "Set knob off position setting [knob:hand/head, pos:0-1000]", CliSetKnobPosOff, CLI_PARAM_STRING, CLI_PARAM_UINT32)
  CLI_ENTRY2( "on",     "Set knob on position setting [knob:hand/head, pos:0-1000]", CliSetKnobPosOn, CLI_PARAM_STRING, CLI_PARAM_UINT32)
  CLI_ENTRY2( "max",    "Set knob max position setting [knob:hand/head, pos:0-1000]", CliSetKnobPosMax, CLI_PARAM_STRING, CLI_PARAM_UINT32)

  CLI_ENTRY2( "pow",    "Set knob min/max power settings [cfg:min/max, pos:0-1000]", CliSetKnobPower, CLI_PARAM_STRING, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(flow_knob)

/******************************************************************************/
#endif
