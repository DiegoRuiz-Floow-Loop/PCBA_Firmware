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

#define HW_COUNT              (USER_BUTTON_Last - USER_BUTTON_First)

#define POLL_START_DELAY_MSEC (100u)

// Debounce requirement about 400-500 msec
#define POLL_MSEC             ( 50u)
#define DEBOUNCE_COUNT        (  1u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const McuPin_t USER_BUTTONS[] = {
#if defined(EVK)
  MP_USER_BTN
#else
  // MP_BTN_1_NC,
  // MP_BTN_2_NC,
  MP_BTN_1_NO,
  MP_BTN_2_NO,
#endif
};
static_assert(SIZEOF_ARRAY(USER_BUTTONS) == (HW_COUNT), "USER_BUTTONS unexpected size");

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

static bool ComponentIndexIsValid(FlcHwComponent_t userButton);
static void PollEvent(EvosEventParam_t param);
static bool GetState(uint_fast8_t hwIdx);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool UserButtonInit(const FlcComponentMapping_t * const cfg)
{
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "UserButtonInit()");  
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = USER_BUTTON_First; idx < USER_BUTTON_Last; idx++) {
    if ((cfg[idx].type != BUTTON)) {
      TRACE(TRC_TA_SUP, TRC_TL_FATAL, "User button config invalid");
      EVENT_LOG_ADD_S("User button config invalid");
      return false;
    }
  }
  
  poll = EvosEventRegister(PollEvent, "user button poll");
  EvosEventSetAndReload(poll, POLL_START_DELAY_MSEC, POLL_MSEC, 0);
  return true;
}

bool UserButtonRead(const FlcHwComponent_t button)
{
  if (!ComponentIndexIsValid(button)) {
    return false;
  }
  
  return GetState(button - USER_BUTTON_First);
}

__WEAK void XUserButtonChanged(const FlcHwComponent_t comp, const bool level)
{
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool ComponentIndexIsValid(const FlcHwComponent_t button)
{
  return ((button >= USER_BUTTON_First) && (button < USER_BUTTON_Last));
}

static void PollEvent(const EvosEventParam_t param)
{
  static uint_fast8_t userButtonStates = 0x00;
  
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
    const uint_fast8_t changedStates = userButtonStates ^ previousStates;
    for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
      const uint_fast8_t mask = 0x1 << idx;
      if (changedStates & mask) {
        XUserButtonChanged(idx + FLOAT_SWITCH_First, ((previousStates & mask) != 0x00));
      }
    }
    
    userButtonStates = previousStates;
  }  
}

static bool GetState(const uint_fast8_t hwIdx)
{
  return HAL_DIO_PIN_GET(USER_BUTTONS[hwIdx]);
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

int_fast16_t CliUserButtonsShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Float Switch State..." CLI_NL);  
  for(uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    CliPrintf("  Float switch %u:\t%s" CLI_NL, 
    idx + 1,
    (UserButtonRead(idx + USER_BUTTON_First) ? "HIGH" : "LOW"));
  }
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(user_buttons)
  CLI_ENTRY0( "show",   "Show all user buttons", CliUserButtonsShowAll)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(user_buttons)

/******************************************************************************/
#endif
