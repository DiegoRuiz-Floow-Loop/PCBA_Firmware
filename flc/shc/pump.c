/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <plf/evos/evos.h>
#include <plf/vtim/vtim.h>
#include <plf/elog/elog.h>

#include <hal/adc/adc.h>
#include <hal/dio/dio.h>
#include <hal/pwm/pwm.h>

#include <sup/adc_mux.h>

#include "pump.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT                (PUMP_Last - PUMP_First)

#define NO_ADC                  ADC_MUX_Last

// According to schematic maximum allowed PWM duty cycle is 99% for the HW pump
// driver. Very important or the pump HW driver output will produce magic smoke!
// Changed to 97% after agreement with HW/KVI.
//
// The maximum PWM duty cycle have been further decreased to 70% by John from
// Flow Loop.
#define POWER_PERMIL_MAX        (700u)

#define ENABLE_DELAY_MSEC       (100u) //(100) is standard value
#define RAMP_TICK_MSEC          ( 10u)
#define RAMP_MAX_CHANGE_PERMIL  ( 30u) //(100u) is standard value

// The current ADC inputs are measured every 100 ms, so a measurement delay is 
// setup to be atleast 150 ms to ensure the buffered ADC input value is ready.
#define OFFSET_MEAS_DELAY_MSEC  (150u)

// ACS711 Hall effect sensor (HES), 12AB subtype, 110mV/A, VCC/2 = 0A. The 
// voltage to current ratio changes with different subtypes.
// VCC = 3.3V (from schematic).
#define VOLT_ZERO_POINT         (3.3f / 2.0f)
#define VOLT_TO_CURRENT_RATIO   (0.11f)

#define PIN_DIR_WHEN_OFF        PIN_DIR_FORWARD

#define PUMP_TL                 TRC_TL_5

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  PIN_DIR_FORWARD,
  PIN_DIR_REVERSE,
  PIN_DIR_Last
} PinDirState_t;

typedef struct {
  McuPin_t pinEnable;
  McuPin_t pinFault;
  McuPin_t pinDirection;
  HalPwmCh_t pwmPower;
  AdcMuxChannels_t adcCurrent;
} PumpHwCfg_t;

typedef struct {
  PumpState_t state;
  VTim_t enableDelayTimer;
  uint16_t pwmTargetPowerPermil;
  uint16_t pwmOutputPowerPermil;
  float currentAmpOffset;
} PumpValues_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const PumpHwCfg_t HW_CFG[] = {
#if defined(EVK)
  //PIN ENABLE       PIN FAULT       PIN DIR       PWM POWER  ADC CURRENT
  { MCUPIN_NA,       MCUPIN_NA,      MCUPIN_NA,    PWM_PUMP1, NO_ADC },
#else
  //PIN ENABLE       PIN FAULT       PIN DIR       PWM POWER  ADC CURRENT
  { MP_PWR_EN_PUMP1, MP_PUMP_FAULT1, MP_PUMP_DIR1, PWM_PUMP1, ADC_MUX_PUMP_CUR1 },
  { MP_PWR_EN_PUMP2, MP_PUMP_FAULT2, MP_PUMP_DIR2, PWM_PUMP2, ADC_MUX_PUMP_CUR2 },
  { MP_PWR_EN_PUMP3, MP_PUMP_FAULT3, MP_PUMP_DIR3, PWM_PUMP3, ADC_MUX_PUMP_CUR3 },
#endif  
};
static_assert(SIZEOF_ARRAY(HW_CFG) == HW_COUNT, "Wrong table row size!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static const FlcComponentMapping_t * appCfg;

static EvosEventHandle_t ramp = EVOS_UNINITIALIZED_HANDLE;
static EvosEventHandle_t fault = EVOS_UNINITIALIZED_HANDLE;
static EvosEventHandle_t offset = EVOS_UNINITIALIZED_HANDLE;

static PumpValues_t pumpVal[HW_COUNT];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static bool CompIndexIsValid(FlcHwComponent_t pump);
static void SetPowerEnablePin(uint_fast8_t index, bool enable);
static void SetPowerDirectionPin(uint_fast8_t index, PinDirState_t pinDir);
static uint_fast8_t CompIndexToHwIndex(FlcHwComponent_t pump);
static void ZeroPwmPowerOutput(uint_fast8_t index);
static void RampUpdatePowerOutput(uint_fast8_t index);
static void RampUpdateEvent(EvosEventParam_t param);
static void CurrentFaultEvent(EvosEventParam_t param);
static void CurrentOffsetEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool PumpInit(const FlcComponentMapping_t * const cfg)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "PumpInit()");

  // Validate the configuration table
  for (FlcHwComponent_t idx = PUMP_First; idx < PUMP_Last; idx++) {
    if (cfg[idx].type != PUMP) {
      TRACE(TRC_TA_SHC, TRC_TL_FATAL, "Pump config invalid");
      EVENT_LOG_ADD_S("Pump config invalid");
      return false;
    }
  }
  appCfg = cfg;
  
  ramp = EvosEventRegister(RampUpdateEvent, "pump ramp");
  EvosEventSetAndReload(ramp, ENABLE_DELAY_MSEC, RAMP_TICK_MSEC, 0);
  
  fault = EvosEventRegister(CurrentFaultEvent, "pump fault");
  
  offset = EvosEventRegister(CurrentOffsetEvent, "pump offset");
  EvosEventSetDelta2(offset, OFFSET_MEAS_DELAY_MSEC, 0);
  
  // Pwm output is started and set to zero percent instead of stopped.
  // This is done to ensure the mcu pwm peripheral's output is still enabled.  
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    HalPwmStartPermil(HW_CFG[idx].pwmPower, 0);
  }
  
  return true;
}

void PumpSetState(const FlcHwComponent_t pump, const PumpState_t state)
{
  if (!CompIndexIsValid(pump) || (state >= PUMP_STATE_Last)) {
    return;
  }
  const uint_fast8_t index = CompIndexToHwIndex(pump);

  if (state == pumpVal[index].state) {
    return;
  }
  
  // PWM power is always set to zero when switching state. This is done to avoid
  // having the PWM output set when switching from one direction to another.
  // Direction pin is not changed here because the driver will wait for the pump
  // power output to slowly drop to zero.
  ZeroPwmPowerOutput(index);
  SetPowerEnablePin(index, false);
  VTimSetMsec(&pumpVal[index].enableDelayTimer, ENABLE_DELAY_MSEC);

  pumpVal[index].state = state;  
}

void PumpSetPower(const FlcHwComponent_t pump, uint16_t permil)
{
  if (!CompIndexIsValid(pump)) {
    return;
  }
  const uint_fast8_t index = CompIndexToHwIndex(pump);

  if (pumpVal[index].state == PUMP_STOP) {
    return;
  }
  
  pumpVal[index].pwmTargetPowerPermil = permil;
}

PumpState_t PumpGetState(const FlcHwComponent_t pump)
{
  if (!CompIndexIsValid(pump)) {
    return PUMP_STOP;
  }
  const uint_fast8_t index = CompIndexToHwIndex(pump);

  return pumpVal[index].state;
}

uint16_t PumpGetPower(const FlcHwComponent_t pump)
{
  if (!CompIndexIsValid(pump)) {
    return 0u;
  }  
  const uint_fast8_t index = CompIndexToHwIndex(pump);
  
  return pumpVal[index].pwmTargetPowerPermil;
}

bool PumpGetCurrentFault(const FlcHwComponent_t pump)
{
  if (!CompIndexIsValid(pump)) {
    return false;
  }  
  const uint_fast8_t index = CompIndexToHwIndex(pump);
  
  // ACS711 Hall effect sensor's fault pin is active low and latched until VCC
  // has been turned off for 200us.
  // The firmware will not reset the latched fault pin.
  return !HAL_DIO_PIN_GET(HW_CFG[index].pinFault);
}

float PumpGetCurrentAmp(const FlcHwComponent_t pump)
{
#if defined(EVK)
  return 0.0f;
#else  
  if (!CompIndexIsValid(pump)) {
    return 0.0f;
  }
  const uint_fast8_t index = CompIndexToHwIndex(pump);  

  const float voltIn  = AdcMuxGetVolt(HW_CFG[index].adcCurrent);  

  // I = (Uin - U0) / (Uratio / 1A)
  const float current = ((voltIn - VOLT_ZERO_POINT) / VOLT_TO_CURRENT_RATIO);
  return (current - pumpVal[index].currentAmpOffset);
#endif  
}

void PumpXiCurrentFaultCb(const FlcHwComponent_t pump)
{
  EvosEventSetNow(fault, pump);
}

__WEAK void XPumpCurrentFault(const FlcHwComponent_t pump)
{
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool CompIndexIsValid(const FlcHwComponent_t pump)
{
  return ((pump >= PUMP_First) && (pump < PUMP_Last));
}

static uint_fast8_t CompIndexToHwIndex(const FlcHwComponent_t pump)
{
  return (pump - PUMP_First);
}

static void SetPowerEnablePin(const uint_fast8_t index, const bool enable)
{
  if (BOOL_EQUAL(HAL_DIO_PIN_GET(HW_CFG[index].pinEnable), enable)) return;

  HAL_DIO_PIN_WRITE(HW_CFG[index].pinEnable, enable);
  TRACE_VA(TRC_TA_SHC, PUMP_TL, "pump %u enable %s", index + 1u, (enable ? "set" : "clear"));
}

static void SetPowerDirectionPin(const uint_fast8_t index, const PinDirState_t pinDir)
{
  if (BOOL_EQUAL(HAL_DIO_PIN_GET(HW_CFG[index].pinDirection), pinDir)) return;
  
  HAL_DIO_PIN_WRITE(HW_CFG[index].pinDirection, pinDir);
  TRACE_VA(TRC_TA_SHC, PUMP_TL, "pump %u direction %s set", index + 1u, ((pinDir == PIN_DIR_FORWARD) ? "fwd" : "rev"));
}

static void ZeroPwmPowerOutput(const uint_fast8_t index)
{
  // It is not required for pump pwm power to be ramped down when turned off so
  // the pwm output is instantly set to zero.

  pumpVal[index].pwmTargetPowerPermil = 0;
  
  if (pumpVal[index].pwmOutputPowerPermil == 0) {
    return;
  }
  pumpVal[index].pwmOutputPowerPermil = 0;

  // Pwm output is started and set to zero percent instead of stopped.
  // This is done to ensure the mcu pwm peripheral's output is still enabled.   
  HalPwmStartPermil(HW_CFG[index].pwmPower, 0);
  
  TRACE_VA(TRC_TA_SHC, PUMP_TL, "pump %u out 0%%", index + 1u);
}

static void RampUpdatePowerOutput(const uint_fast8_t index)
{
  if (pumpVal[index].state == PUMP_STOP) {
    // The direction pin is set in a low power mode after a delay where the 
    // pump power output slowly has dropped to zero.
    if ((HAL_DIO_PIN_GET(HW_CFG[index].pinDirection) != PIN_DIR_WHEN_OFF) && 
        VTimIsExpired(&pumpVal[index].enableDelayTimer)) {
      SetPowerDirectionPin(index, PIN_DIR_WHEN_OFF);
    }
    return;
  }

  // Enable and direction pins are first changed after the ramp delay to ensure 
  // PWM power output has fallen to zero before switching to the new direction.
  if (!VTimIsExpired(&pumpVal[index].enableDelayTimer)) {
    return;
  }
  if (!HAL_DIO_PIN_GET(HW_CFG[index].pinEnable)) {
    SetPowerDirectionPin(index, ((pumpVal[index].state == PUMP_FORWARD) ? PIN_DIR_FORWARD : 
                                                                          PIN_DIR_REVERSE));
    SetPowerEnablePin(index, true);
    return;
  }

  // Very important or the pump HW driver output will produce magic smoke!
  const uint_fast16_t target = MIN_VAL(pumpVal[index].pwmTargetPowerPermil, POWER_PERMIL_MAX);
  
  if (target == pumpVal[index].pwmOutputPowerPermil) {
    return;
  }

  if (target > pumpVal[index].pwmOutputPowerPermil) {
    uint_fast16_t diff = target - pumpVal[index].pwmOutputPowerPermil;
    diff = MIN_VAL(diff, RAMP_MAX_CHANGE_PERMIL);
    pumpVal[index].pwmOutputPowerPermil += diff;
  } else {
    uint_fast16_t diff = pumpVal[index].pwmOutputPowerPermil - target;
    diff = MIN_VAL(diff, RAMP_MAX_CHANGE_PERMIL);
    pumpVal[index].pwmOutputPowerPermil -= diff;  
  }
  
  // Pwm output is started and set to zero percent instead of stopped.
  // This is done to ensure the mcu pwm peripheral's output is still enabled.  
  HalPwmStartPermil(HW_CFG[index].pwmPower, pumpVal[index].pwmOutputPowerPermil);
  
  TRACE_VA(TRC_TA_SHC, PUMP_TL, "pump %u out %u%%", 
                                index + 1u, 
                                pumpVal[index].pwmOutputPowerPermil / 10u);  
}

static void RampUpdateEvent(const EvosEventParam_t param)
{
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    RampUpdatePowerOutput(idx);
  }
}

static void CurrentFaultEvent(const EvosEventParam_t param)
{
  XPumpCurrentFault(param);
}

static void CurrentOffsetEvent(const EvosEventParam_t param)
{
  TRACE(TRC_TA_SHC, PUMP_TL, "pump meas current offsets");
  
  for (FlcHwComponent_t pump = PUMP_First; pump < PUMP_Last; pump++) {
    const uint_fast8_t index = CompIndexToHwIndex(pump);
    
    // The current offset are zero after init, so the public get current 
    // function can be used.
    pumpVal[index].currentAmpOffset = PumpGetCurrentAmp(pump);
  }
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

#include <string.h>

static const char * const STATE_TEXT[] = {
  "STOPPED",        // PUMP_STOP
  "FORWARD",        // PUMP_FORWARD
  "REVERSE",        // PUMP_REVERSE
};
static_assert(SIZEOF_ARRAY(STATE_TEXT) == PUMP_STATE_Last, "Wrong table row size!");

int_fast16_t CliPumpShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Pump State..." CLI_NL);
  for (uint_fast8_t index = 0; index < HW_COUNT; index++) {
    const FlcHwComponent_t pump = index + PUMP_First;
    CliPrintf("  Pump %u:\t%s\ttarget %3u%%\tout %3u%%\t%0.3fA %s" CLI_NL, 
              index + 1, 
              STATE_TEXT[pumpVal[index].state],
              pumpVal[index].pwmTargetPowerPermil / 10u,
              pumpVal[index].pwmOutputPowerPermil / 10u,
              (double)PumpGetCurrentAmp(pump),
              (PumpGetCurrentFault(pump) ? "(fault)" : ""));
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetState(const CliParam_t no, const CliParam_t state, const  CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  const FlcHwComponent_t pump = no - 1 + PUMP_First;
  
  if (strcmp("stop", (char *)state) == 0) {
    PumpSetState(pump, PUMP_STOP);
  } else if (strcmp("fwd", (char *)state) == 0) {
    PumpSetState(pump, PUMP_FORWARD);
  } else if (strcmp("rev", (char *)state) == 0) {
    PumpSetState(pump, PUMP_REVERSE);
  } else {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetPower(const CliParam_t no, const CliParam_t pct, const CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  if (pct > 100) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;  
  }
  
  const FlcHwComponent_t pump = no - 1 + PUMP_First;
  PumpSetPower(pump, pct * 10u);
  return CLI_RESULT_OK;
}

CLI_START_TABLE(pump)
  CLI_ENTRY0( "show",   "Show all pumps", CliPumpShowAll)
  CLI_ENTRY2( "set",    "Set pump state [no:1..max, state:stop/fwd/rev]", CliSetState, CLI_PARAM_UINT32, CLI_PARAM_STRING)
  CLI_ENTRY2( "pwr",    "Set pump power [no:1..max, pct:0..100]", CliSetPower, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(pump)

/******************************************************************************/
#endif
