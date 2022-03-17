/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <plf/evos/evos.h>
#include <plf/elog/elog.h>

#include <hal/dio/dio.h>

#include "adc_mux.h"
#include "float_switch.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT              (FLOAT_SWITCH_Last - FLOAT_SWITCH_First)

#define POLL_START_DELAY_MSEC (100u)

// Debounce requirement about 400-500 msec
#define POLL_MSEC             ( 50u)
#define DEBOUNCE_COUNT        (  8u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const McuPin_t FLOAT_SWITCHES[] = {
#if defined(EVK)
  MP_USER_BTN
#else
  MP_TANK_SENSOR1,
  MP_TANK_SENSOR2,
  MP_TANK_SENSOR3,
  MP_TANK_SENSOR4,
  MP_TANK_SENSOR5
#endif
};
static_assert(SIZEOF_ARRAY(FLOAT_SWITCHES) == HW_COUNT, "FLOAT_SWITCHES unexpected size");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t poll = EVOS_UNINITIALIZED_HANDLE;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static bool ComponentIndexIsValid(FlcHwComponent_t floatSwitch);
static void PollEvent(EvosEventParam_t param);
static bool GetState(uint_fast8_t hwIdx);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool FloatSwitchInit(const FlcComponentMapping_t * const cfg)
{
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "FloatSwitchInit()");  
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = FLOAT_SWITCH_First; idx < FLOAT_SWITCH_Last; idx++) {
    if ((cfg[idx].type != FLOAT_SWITCH)) {
      TRACE(TRC_TA_SUP, TRC_TL_FATAL, "Float switch config invalid");
      EVENT_LOG_ADD_S("Float switch config invalid");
      return false;
    }
  }
  
  poll = EvosEventRegister(PollEvent, "float switch poll");
  EvosEventSetAndReload(poll, POLL_START_DELAY_MSEC, POLL_MSEC, 0);
  return true;
}

bool FloatSwitchRead(const FlcHwComponent_t floatSwitch)
{
  if (!ComponentIndexIsValid(floatSwitch)) {
    return false;
  }
  
  return GetState(floatSwitch - FLOAT_SWITCH_First);
}

__WEAK void XFloatSwitchChanged(const FlcHwComponent_t comp, const bool level)
{
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool ComponentIndexIsValid(const FlcHwComponent_t floatSwitch)
{
  return ((floatSwitch >= FLOAT_SWITCH_First) && (floatSwitch < FLOAT_SWITCH_Last));
}

static void PollEvent(const EvosEventParam_t param)
{
  static uint_fast8_t floatSwitchStates = 0x00;
  
  // Get the current float swich states
  uint8_t newStates = 0x00;
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    if (GetState(idx)) {
      newStates |= 0x1 << idx;
    }
  }
  
  // Debounce functionality for how many times the combined bit states of all
  // inputs needs to be the same before actual input states are updated.
  static uint_fast8_t previousStates = 0x00;
  static uint_fast8_t debounceCount = 0;    
  if (previousStates != newStates) {
    previousStates = newStates;
    debounceCount = 1;
  } else {
    debounceCount++;
    debounceCount = MIN_VAL(debounceCount, DEBOUNCE_COUNT + 1U);
  }
  if (debounceCount == DEBOUNCE_COUNT) {
      
    // Callback support when changed is merged into debounce code implementation
    const uint_fast8_t changedStates = floatSwitchStates ^ previousStates;
    for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
      const uint_fast8_t mask = 0x1 << idx;
      if (changedStates & mask) {
        XFloatSwitchChanged(idx + FLOAT_SWITCH_First, ((previousStates & mask) != 0x00));
      }
    }
    
    floatSwitchStates = previousStates;
  }  
}

static bool GetState(const uint_fast8_t hwIdx)
{
  return !HAL_DIO_PIN_GET(FLOAT_SWITCHES[hwIdx]);
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

int_fast16_t CliFloatSwitchShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Float Switch State..." CLI_NL);  
  for(uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    CliPrintf("  Float switch %u:\t%s" CLI_NL, 
    idx + 1,
    (FloatSwitchRead(idx + FLOAT_SWITCH_First) ? "HIGH" : "LOW"));
  }
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(float_switches)
  CLI_ENTRY0( "show",   "Show all float switches", CliFloatSwitchShowAll)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(float_switches)

/******************************************************************************/
#endif
