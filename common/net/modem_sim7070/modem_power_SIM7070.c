/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/evos/evos.h>

#include <hal/dio/dio.h>

#include "modem.h"
#include "modem_power.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#if defined(EVK)
#define PWR_KEY_HIGH()    HAL_DIO_PIN_CLR(MP_GSM_PWR_KEY)
#define PWR_KEY_LOW()     HAL_DIO_PIN_SET(MP_GSM_PWR_KEY)
#define STATUS_IS_HIGH()  (HAL_DIO_PIN_GET(MP_GSM_STATUS) == GPIO_PIN_SET)
#else
#define PWR_KEY_HIGH()    HAL_DIO_PIN_CLR(MP_MDM_PWR_KEY)
#define PWR_KEY_LOW()     HAL_DIO_PIN_SET(MP_MDM_PWR_KEY)
#define STATUS_IS_HIGH()  (HAL_DIO_PIN_GET(MP_MDM_STATUS) == GPIO_PIN_SET)
#endif

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  POWER_START_DELAY,
  POWER_IDLE,
  POWER_OFF_PULSE,
  POWER_OFF_WAIT,
  POWER_OFF_DELAY,
  POWER_ON_PULSE,
  POWER_ON_WAIT,
  POWER_Last
} PowerState_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryPowerStartDelay(void);
static void OnExitPowerStartDelay(void);
static void OnEntryPowerIdle(void);
static void OnExitPowerIdle(void);
static void OnEntryPowerOffPulse(void);
static void OnExitPowerOffPulse(void);
static void OnEntryPowerOffWait(void);
static void OnExitPowerOffWait(void);
static void OnEntryPowerOffDelay(void);
static void OnEntryPowerOnPulse(void);
static void OnExitPowerOnPulse(void);
static void OnEntryPowerOnWait(void);
static void OnExitPowerOnWait(void);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t POWER_FUNCS[POWER_Last] = {
  // .onEntry,              .onExit
  { OnEntryPowerStartDelay, OnExitPowerStartDelay },  // POWER_START_DELAY
  { OnEntryPowerIdle,       OnExitPowerIdle },        // POWER_IDLE
  { OnEntryPowerOffPulse,   OnExitPowerOffPulse },    // POWER_OFF_PULSE
  { OnEntryPowerOffWait,    OnExitPowerOffWait },     // POWER_OFF_WAIT
  { OnEntryPowerOffDelay,   DoNothing },              // POWER_OFF_DELAY
  { OnEntryPowerOnPulse,    OnExitPowerOnPulse },     // POWER_ON_PULSE
  { OnEntryPowerOnWait,     OnExitPowerOnWait },      // POWER_ON_WAIT
};

static const char * POWER_STATE_TEXTS[POWER_Last] = {
  "pwr start delay",  // POWER_START_DELAY
  "pwr idle",         // POWER_IDLE
  "pwr off pulse",    // POWER_OFF_PULSE
  "pwr off wait",     // POWER_OFF_WAIT
  "pwr off delay",    // POWER_OFF_DELAY
  "pwr on pulse",     // POWER_ON_PULSE
  "pwr on wait",      // POWER_ON_WAIT
};

static const uint32_t START_DELAY_MSEC = MODEM_POWER_START_DELAY;

static const uint32_t POWER_OFF_PULSE_MSEC = 1200;
static const uint32_t POWER_OFF_WAIT_MSEC = 10000;
static const uint32_t POWER_OFF_DELAY_MSEC = 2000;

static const uint32_t POWER_ON_PULSE_MSEC = 1000;
static const uint32_t POWER_ON_WAIT_MSEC = 10000;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;
static EvosEventHandle_t power = EVOS_UNINITIALIZED_HANDLE;

static bool powerOnRequired;
static bool powerOffRequired;
static bool timeout;
static bool powerStatusFromIsr;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextPowerState(uint8_t current, uint8_t * next);

// Other
static void TimerSet(uint32_t msec);
static void TimerClear(void);
static void TimeoutEvent(EvosEventParam_t param);
static void PowerEvent(EvosEventParam_t param);
static void LogIO(const char * sz, bool high);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemPowerInit(void)
{
  timer = EvosEventRegister(TimeoutEvent, "modem pwr timer");
  power = EvosEventRegister(PowerEvent, "modem pwr xi");

  ModemSmRegister(MODEM_SM_POWER, NextPowerState, POWER_FUNCS, POWER_STATE_TEXTS);
  
  LogIO("pwr di", STATUS_IS_HIGH());
  
  powerOffRequired = true;
  POWER_FUNCS[0].onEntry();
}

bool ModemPowerOn(void)
{
  if (xModemPowerLocked) 
    return false;
  
  powerOnRequired = true;
  powerOffRequired = false;
  ModemSmUpdate();
  return true;
}

bool ModemPowerOff(void)
{
  if (xModemPowerLocked || xModemNetLocked) return false;
  
  powerOnRequired = false;
  powerOffRequired = true;
  ModemSmUpdate();
  return true;
}

bool ModemForcePowerOff(void)
{  
  powerOnRequired = false;
  powerOffRequired = true;
  ModemSmUpdate();
  return true;
}

// Called from external interrupt function
void ModemXiStatusCb(void)
{
  // Param set as 1 to indicate interrupt function as source.
  EvosEventSetNow(power, 1);
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextPowerState(const uint8_t current, uint8_t * const next)
{
  *next = POWER_Last;
  switch (current) {
    case POWER_START_DELAY:
      if (powerStatusFromIsr || timeout) 
        *next = POWER_IDLE;
      break;
    
    case POWER_IDLE:
      if (!xModemPowerOn && powerOnRequired) 
        *next = POWER_ON_PULSE;
      else if (xModemPowerOn && powerOffRequired) 
        *next = POWER_OFF_PULSE;
      break;
    
    case POWER_OFF_PULSE:
      if (timeout) 
        *next = POWER_OFF_WAIT;
      break;
    
    case POWER_OFF_WAIT:
      if (!xModemPowerOn) 
        *next = POWER_OFF_DELAY;
      else if (timeout) 
        *next = POWER_IDLE;
      break;
    
    case POWER_OFF_DELAY:
      if (timeout) 
        *next = POWER_IDLE;
      break;
    
    case POWER_ON_PULSE:
      if (timeout) 
        *next = POWER_ON_WAIT;
      break;
    
    case POWER_ON_WAIT:
      if (xModemPowerOn || timeout) 
        *next = POWER_IDLE;
      break;
    
    default:
      return POWER_IDLE;    
  }
  return (*next < POWER_Last);
}

static void OnEntryPowerStartDelay(void)
{
  TimerSet(START_DELAY_MSEC);
}

static void OnExitPowerStartDelay(void)
{
  TimerClear();
  
  // Ensure that the modem power status is read after the delay.
  if (!powerStatusFromIsr) {
    // Param set as 0 to indicate non-interrupt function as source.
    EvosEventSetNow(power, 0);
  }
}

static void OnEntryPowerIdle(void)
{
  xModemPowerLocked = false;
}

static void OnExitPowerIdle(void)
{
  xModemPowerLocked = true;
}

static void OnEntryPowerOffPulse(void)
{
  PWR_KEY_LOW();
  TimerSet(POWER_OFF_PULSE_MSEC);
  powerOffRequired = false;
}

static void OnExitPowerOffPulse(void)
{
  PWR_KEY_HIGH();
}

static void OnEntryPowerOffWait(void)
{
  TimerSet(POWER_OFF_WAIT_MSEC);
}

static void OnExitPowerOffWait(void)
{
  TimerClear();
  XModemPower(xModemPowerOn);
}

static void OnEntryPowerOffDelay(void)
{
  TimerSet(POWER_OFF_DELAY_MSEC);
  xModemPowerLocked = false;  
}

static void OnEntryPowerOnPulse(void)
{
  PWR_KEY_LOW();
  TimerSet(POWER_ON_PULSE_MSEC);
  powerOnRequired = false;
}

static void OnExitPowerOnPulse(void)
{
  PWR_KEY_HIGH();
}

static void OnEntryPowerOnWait(void)
{
  TimerSet(POWER_ON_WAIT_MSEC);  
}

static void OnExitPowerOnWait(void)
{
  TimerClear();
  XModemPower(xModemPowerOn);
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
  ModemLogState("pwr timeout");
  
  timeout = true;
  ModemSmUpdate();
}

static void PowerEvent(const EvosEventParam_t fromIsr)
{
  powerStatusFromIsr = fromIsr;
  xModemPowerOn = STATUS_IS_HIGH();
  
  if (fromIsr) {
    LogIO("pwr xi", xModemPowerOn);
  } else {
    XModemPower(xModemPowerOn);
    LogIO("pwr di", xModemPowerOn);
  }
  ModemSmUpdate();
}

static void LogIO(const char * const text, const bool high)
{
  char sz[24];
  snprintf(sz, 24, "%s %u", text, high);
  ModemLogState(sz);
}

/******************************************************************************/
