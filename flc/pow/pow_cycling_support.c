/*******************************************************************************
 * TekPartner A/S
 * $Header: https://svn.tekpartner.dk/svn/FLP_P01/sw/trunk/flc/shc/valve.c 142 2021-07-09 09:43:49Z ett17934 $
 ******************************************************************************/

// The Current-Limited Load Switch AOZ1361DI-01 was not available for board 
// production and was replaced with AOZ1361DI-02 on some boards.
// The AOZ1361DI-02 version doesn't have auto-restart feature and is latched off
// until a power cycle has occured.
// The firmware will need to treat all AOZ1361DI as version 2 without 
// auto-restart feature. The auto-restart feature is implemented in firmware.

// AOZ1361DIs on the board
// - Supported: Valves, UV lamp, MMI interface, Water sensors
// - Not used : Future Use +24V

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"
#include "plf/vtim/vtim.h"

#include "hal/dio/dio.h"

#include "pow.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define POWER_CYCLE_MSEC        (200)
#define POWER_CYCLE_UPDATE_MSEC (  5)

#define TA_POWER  (TRC_TA_APP)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  McuPin_t pinEnable;
  McuPin_t pinFault;
} PowerHwCfg_t;

typedef struct {
  bool isEnabled;
  struct {
    bool isDetected;
    VTim_t timerPowerCycle;
  } fault;
} PowerValues_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const PowerHwCfg_t HW_CFG[] = {
  // Enable pin      // Fault pin
  { MP_PWR_EN_VALVE, MP_PWR_FAULT_24V },   // POWER_VALVES
  { MP_PWR_EN_UV,    MP_PWR_FAULT_24V },   // POWER_UVLAMP
  { MP_PWR_EN_MMI,   MP_PWR_FAULT_24V },   // POWER_MMI
};
static_assert(SIZEOF_ARRAY(HW_CFG) == POWER_Last, "Wrong table row count!");

static const char * const POWER_TEXTS[] = {
  "valves",   // POWER_VALVES
  "uv lamp",  // POWER_UVLAMP
  "mmi",      // POWER_MMI
};
static_assert(SIZEOF_ARRAY(POWER_TEXTS) == POWER_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t handleFault = EVOS_UNINITIALIZED_HANDLE;
static EvosEventHandle_t handlePowerCycle = EVOS_UNINITIALIZED_HANDLE;

static PowerValues_t powers[POWER_Last];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static void FaultEvent(EvosEventParam_t param);
static void PowerCycleTask(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void POW_Init(void)
{
  TRACE(TA_POWER, TRC_TL_COMPONENT, "POW_Init()");
  
  handleFault = EvosEventRegister(FaultEvent, "power fault");
  handlePowerCycle = EvosEventRegister(PowerCycleTask, "power cycle");
  
  // Power sources not controlled by external drivers
  PowerSet(POWER_MMI, true);
}

void PowerSet(const Power_t idx, const bool enable)
{
  if (idx >= POWER_Last) return;

  if (powers[idx].fault.isDetected) {
    // Don't disturb required power cycle after a fault is detected
  } else {
    HAL_DIO_PIN_WRITE(HW_CFG[idx].pinEnable, enable);
  }
  powers[idx].isEnabled = enable;    
}

void PowerXi24VFaultCb(void)
{
  EvosEventSetNow(handleFault, 0);
}

void PowerXiSensorFaultCb(void)
{
  EvosEventSetNow(handleFault, 0);
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static void FaultEvent(const EvosEventParam_t param)
{
  bool anyNewFaultsDetected = false;
  bool newFaultDetected[POWER_Last] = {false};

  // Faults are detected first before any power enable pins are cleared for 
  // power cycle. This is done to ensure that all shared power faults are 
  // registered before the power is disabled.
  // Delays by trace printouts have been seen the cause of not all shared faults
  // correctly detected.
  
  for (uint_fast8_t idx = 0; idx < POWER_Last; idx++) {
    // Fault signals are active low
    if (!powers[idx].fault.isDetected && !HAL_DIO_PIN_GET(HW_CFG[idx].pinFault)) {
      anyNewFaultsDetected = true;
      newFaultDetected[idx] = true;
    }
  }
  
  if (!anyNewFaultsDetected) return;
  
  for (uint_fast8_t idx = 0; idx < POWER_Last; idx++) {
    if (newFaultDetected[idx]) {
      powers[idx].fault.isDetected = true;
      HAL_DIO_PIN_CLR(HW_CFG[idx].pinEnable);
      VTimSetMsec(&powers[idx].fault.timerPowerCycle, POWER_CYCLE_MSEC);
      
      EvosEventSetNow(handlePowerCycle, 0);
      TRACE_VA(TA_POWER, TRC_TL_ERROR, "power %s fault", POWER_TEXTS[idx]);      
    }
  }
}

static void PowerCycleTask(const EvosEventParam_t param)
{
  bool isPowerCycling = false;
  
  for (uint_fast8_t idx = 0; idx < POWER_Last; idx++) {
    if (powers[idx].fault.isDetected) {
      if (!VTimIsExpired(&powers[idx].fault.timerPowerCycle)) {
        isPowerCycling = true;
      } else {
        powers[idx].fault.isDetected = false;
        HAL_DIO_PIN_WRITE(HW_CFG[idx].pinEnable, powers[idx].isEnabled);
        
        if (powers[idx].isEnabled) {
          TRACE_VA(TA_POWER, TRC_TL_2, "power %s cycled", POWER_TEXTS[idx]);
        }
      }
    }
  }
  
  if (isPowerCycling) {
    EvosEventSetDelta(handlePowerCycle, POWER_CYCLE_UPDATE_MSEC, 0);
  }
}

/******************************************************************************/
