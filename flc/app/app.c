/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/key/key.h"
#include "plf/led/led.h"
#include "plf/elog/elog.h"

#include "app/flc_cfg.h"
#include "app/dlogger.h"

#include "cif/cloud_flc.h"

#include "sup/sup.h"

#include "shc/shc.h"
#include "shc/flow_knob.h"
#include "shc/hot_supply.h"
#include "shc/cold_supply.h"
#include "shc/prime_pump.h"

#include "app.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HEARTBEAT_ON_MSEC     ( 100u)
#define HEARTBEAT_OFF_MSEC    (1900u)

#define SYSTEM_ERROR_ON_MSEC  ( 100u)
#define SYSTEM_ERROR_OFF_MSEC ( 400u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static bool systemInError;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void APP_Init(void)
{
  EVENT_LOG_ADD_S("System booted");
  TRACE(TRC_TA_APP, TRC_TL_COMPONENT, "APP_Init()");
  
  FlcCfgInit();
  
  // SUP and SHC modules initialize all HW component drivers. Those drivers will
  // fail on init if the FLC HW configuration is invalid.
  if (!systemInError) {
    systemInError = !SUP_Init(); 
  }
  if (!systemInError) {
    systemInError = !SHC_Init(); 
  }
  
  if (systemInError) {
    TRACE(TRC_TA_APP, TRC_TL_FATAL, "System Error - Shower application halted!");
    EVENT_LOG_ADD_S("System Error - Shower application halted!");
    LedFlash(LED_HEARTBEAT, SYSTEM_ERROR_ON_MSEC, SYSTEM_ERROR_OFF_MSEC);
    return;
  }
  
  FlowKnobInit();
  HotSupplyInit();
  ColdSupplyInit();
  PrimePumpInit();
  DLoggerInit(flcCfg);
  
  LedFlash(LED_HEARTBEAT, HEARTBEAT_ON_MSEC, HEARTBEAT_OFF_MSEC);
  KeyScanner(true);
}

void XKeyChanged(const Key_t key, const bool level) 
{
  TRACE_VA(TRC_TA_APP, TRC_TL_5, "XKeyChanged(key=%u, level=%u)", key, level);
}

void XShcStateChanged(const ShcState_t state)
{
  HotSupplySetShowerStateChange(state);
  ColdSupplySetShowerStateChange(state);
  PrimePumpSetShowerStateChange(state);
  DLoggerStateChanged(state);
}

void XShcShowerIsDone(void)
{
  TRACE(TRC_TA_APP, TRC_TL_2, "XShcShowerIsDone()");
  CloudFlcProcess();
}

void XShcLowPowerChanged(const bool isLowPower)
{
  TRACE_VA(TRC_TA_APP, TRC_TL_2, "XShcLowPowerChanged(lowpower=%u)", isLowPower);
  CloudFlcSetCanUpdateFw(isLowPower);  
}

void XFloatSwitchChanged(const FlcHwComponent_t comp, const bool level)
{
  TRACE_VA(TRC_TA_APP, TRC_TL_2, "XFloatSwitchChanged(comp=%u, level=%u)", comp, level);
  
  if (flcCfg[comp].component == HOT_TANK_FLOAT_SWITCH) {
    HotSupplySetTankLevelChanged(level);
  }
  else if (flcCfg[comp].component == MIXED_TANK_COLD_FLOAT_SWITCH) {
    ColdSupplySetTankColdLevelChanged(level);
    PrimePumpSetTankColdLevelChanged(level);
  }
  else if (flcCfg[comp].component == MIXED_TANK_LOOP_FLOAT_SWITCH) {
    ColdSupplySetTankLoopLevelChanged(level);
    PrimePumpSetTankLoopLevelChanged(level);
  }
}

void XFlowKnobOutOfBoundsChanged(const bool isOutOfBounds)
{
#ifndef PCB_TEST
  TRACE_VA(TRC_TA_APP, TRC_TL_2, "XFlowKnobOutOfBoundsChanged(outOfBounds=%u)", isOutOfBounds);
  
  if (!systemInError && isOutOfBounds) {
    systemInError = true;
    ShcSetEmergencyStop();
    
    TRACE(TRC_TA_APP, TRC_TL_FATAL, "System Error - Flow knob sensor position is out of bounds!");
    EVENT_LOG_ADD_S("System Error - Flow knob sensor position is out of bounds!");
    LedFlash(LED_HEARTBEAT, SYSTEM_ERROR_ON_MSEC, SYSTEM_ERROR_OFF_MSEC);
  }
#endif
}

void XUserButtonChanged(const FlcHwComponent_t comp, const bool level)
{
  TRACE_VA(TRC_TA_APP, TRC_TL_2, "XUserButtonChanged(comp=%u, level=%u)", comp, level);
  // switch (comp) {
  //   case 0:
  //   {
  //     if (level) {
  //       LedOn(LED_BTN_1);
  //     } else {
  //       LedOff(LED_BTN_1);
  //     }
  //     break;
  //   }

  //   case 1:
  //   {
  //     if (level) {
  //       LedOn(LED_BTN_2);
  //     } else {
  //       LedOff(LED_BTN_2);
  //     }
  //     break;      
  //   }
  // }

  if (comp == 0) {
    SetShowerLoopButton(level);
  }
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

/******************************************************************************/
