/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>
#include <plf/evos/evos.h>
#include <plf/nvm/nvm.h>

#include <app/flc_cfg.h>

#include <sup/float_switch.h>

#include <shc/shc.h>
#include <shc/valve.h>

#include "hot_supply.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define CLOSE_DELAY_MSEC  (   0u)
#define OPEN_DELAY_MSEC   ( 200u)

#define CLOSE             (false)
#define OPEN              (true)

#define SUPPLY_FSM_TL     TRC_TL_4

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  SUPPLY_INACTIVE,
  SUPPLY_IDLE,
  SUPPLY_DELAYED_OPEN,
  SUPPLY_DELAYED_CLOSE,
  SUPPLY_STATE_Last
} State_t;

typedef enum {
  ON_ENTRY,
  ON_EXIT,
  ON_TIMEOUT,
  ON_LEVEL_CHANGE,
  ON_ACTIVATE,
  ON_DEACTIVATE,
  ON_EVENT_Last
} Event_t;

typedef void (*StateFunc_t)(Event_t event);

/*******************************************************************************
 * Function prototypes, state machine
 ******************************************************************************/

static void StateInactive(Event_t event);
static void StateIdle(Event_t event);
static void StateDelayedOpen(Event_t event);
static void StateDelayedClose(Event_t event);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const StateFunc_t STATE_FUNCS[] = {
  StateInactive,        // SUPPLY_INACTIVE
  StateIdle,            // SUPPLY_IDLE
  StateDelayedOpen,     // SUPPLY_DELAYED_OPEN
  StateDelayedClose,    // SUPPLY_DELAYED_CLOSE
};
static_assert(SIZEOF_ARRAY(STATE_FUNCS) == SUPPLY_STATE_Last, "Wrong table row count!");

static const char * const STATE_TEXTS[] = {
  "hot supply inactive",        // SUPPLY_INACTIVE
  "hot supply idle",            // SUPPLY_IDLE
  "hot supply delayed open",    // SUPPLY_DELAYED_OPEN
  "hot supply delayed close",   // SUPPLY_DELAYED_CLOSE
};
static_assert(SIZEOF_ARRAY(STATE_TEXTS) == SUPPLY_STATE_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static HotSupply_t cfg;

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static State_t currentState;
static State_t nextState;
static bool nextIsLocked;

static bool tankLevel;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static void EventProcess(Event_t event);
static void Transistion(State_t state);

// State machine helpers
static void TimerSet(uint32_t msec);
static void TimerClear(void);
static void EvosEvent(EvosEventParam_t param);
static bool ActiveInShowerState(ShcState_t state);
static void SetHotSupplyValve(bool open);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void HotSupplyInit(void)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "HotSupplyInit()");
  
  ConfigLoad();
  
  timer = EvosEventRegister(EvosEvent, "hot supply timer");
}

void HotSupplySetShowerStateChange(const ShcState_t state)
{
  static ShcState_t lastShowerState;
  
  if (state == lastShowerState) return;
  
  const bool lastIsActive = ActiveInShowerState(lastShowerState);
  const bool nowIsActive  = ActiveInShowerState(state);
  if (BOOL_NOT_EQUAL(lastIsActive, nowIsActive)) {
    EventProcess((nowIsActive ? ON_ACTIVATE : ON_DEACTIVATE));  
  }
  
  lastShowerState = state;
}

void HotSupplySetTankLevelChanged(const bool level)
{
  if (level == tankLevel) return;
  
  tankLevel = level;
  EventProcess(ON_LEVEL_CHANGE);
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void EventProcess(const Event_t event)
{
  STATE_FUNCS[currentState](event);

  while (currentState != nextState) {
    // State transistions are not allowed on exit events to avoid transistioning
    // to a wrong state on programmer error/forgetfulness. 
    nextIsLocked = true;
    STATE_FUNCS[currentState](ON_EXIT);
    nextIsLocked = false;

    currentState = nextState;
    TrcTrace(TRC_TA_SHC, SUPPLY_FSM_TL, STATE_TEXTS[currentState]);
    STATE_FUNCS[currentState](ON_ENTRY);
  }
}

static void Transistion(const State_t state)
{
  if (!nextIsLocked) {
    nextState = state;
  }
}

static void StateInactive(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      SetHotSupplyValve(CLOSE);
      break;

    case ON_EXIT:
      SetHotSupplyValve(tankLevel ? CLOSE : OPEN);
      break;
    
    case ON_ACTIVATE:
      Transistion(SUPPLY_IDLE);
      break;

    case ON_TIMEOUT:
    case ON_LEVEL_CHANGE:      
    case ON_DEACTIVATE:
    case ON_EVENT_Last:
      break;
  }  
}

static void StateIdle(const Event_t event)
{
  switch (event) {
    case ON_LEVEL_CHANGE:
      Transistion((tankLevel ? SUPPLY_DELAYED_CLOSE : SUPPLY_DELAYED_OPEN));
      break;
    
    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;
    
    case ON_ENTRY:
    case ON_EXIT:      
    case ON_TIMEOUT:  
    case ON_ACTIVATE:
    case ON_EVENT_Last:
      break;
  }  
  
}

static void StateDelayedOpen(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      TimerSet(cfg.timingMsec[HOT_SUPPLY_TIMING_DELAYED_OPEN]);
      break;
    
    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetHotSupplyValve(OPEN);
      Transistion(SUPPLY_IDLE);      
      break;
    
    case ON_LEVEL_CHANGE:
      Transistion(SUPPLY_IDLE);
      break;
    
    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;

    case ON_ACTIVATE:
    case ON_EVENT_Last:
      break;
  }  
}

static void StateDelayedClose(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      TimerSet(cfg.timingMsec[HOT_SUPPLY_TIMING_DELAYED_CLOSE]);
      break;

    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetHotSupplyValve(CLOSE);
      Transistion(SUPPLY_IDLE);
      break;
    
    case ON_LEVEL_CHANGE:
      Transistion(SUPPLY_IDLE);
      break;

    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;

    case ON_ACTIVATE:      
    case ON_EVENT_Last:
      break;
  }  
}

/*******************************************************************************
 * Local functions, state machine helpers
 ******************************************************************************/

static void TimerSet(const uint32_t msec)
{
  EvosEventSetDelta2(timer, msec, ON_TIMEOUT);
}

static void TimerClear(void)
{
  EvosEventClear(timer);
}

static void EvosEvent(const EvosEventParam_t param)
{
  EventProcess(param);
}

static bool ActiveInShowerState(const ShcState_t state)
{
  switch (state) {
    case SHC_IDLE:
    case SHC_FLOW_STOP:
      return false;
    
    case SHC_PRE_BACKWASH:
    case SHC_SHOWER:
    case SHC_SHOWER_LOOP:
    case SHC_EMPTY_AIRGAP:
    case SHC_POST_BACKWASH:
    case SHC_EMPTY_FINAL:  
      return true;
    
    default:
      return false;
  }
}

static void SetHotSupplyValve(const bool open)
{
  const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(HOT_TANK_VALVE);
  if (open) {
    ValveOpen(hwComp);
  } else {
    ValveClose(hwComp);
  }
}

/*******************************************************************************
 * Local functions, configuration
 ******************************************************************************/

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.hotSupply), (uint8_t *)&cfg, sizeof(HotSupply_t));
    if (cfg.crc != CalcCrc16(offsetof(HotSupply_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(HotSupply_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.hotSupply), (uint8_t *)&cfg, sizeof(HotSupply_t));
}

static void ConfigSetDefault(void)
{
  cfg.timingMsec[HOT_SUPPLY_TIMING_DELAYED_CLOSE] = CLOSE_DELAY_MSEC;
  cfg.timingMsec[HOT_SUPPLY_TIMING_DELAYED_OPEN]  = OPEN_DELAY_MSEC;

  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

static const char * const TIMING_TEXTS[] = {
  "hot supply close delay",  // HOT_SUPPLY_TIMING_DELAYED_CLOSE
  "hot supply open delay",   // HOT_SUPPLY_TIMING_DELAYED_OPEN
};
static_assert(SIZEOF_ARRAY(TIMING_TEXTS) == HOT_SUPPLY_TIMING_Last, "Wrong table row size!");

int_fast16_t CliHotSupplyShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Hot Supply Controller..." CLI_NL);
  CliPrintf("  State  :\t%s" CLI_NL, STATE_TEXTS[currentState]);
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Hot Supply Timings Config..." CLI_NL);
  for (uint_fast8_t idx = 0; idx < HOT_SUPPLY_TIMING_Last; idx++) {
    CliPrintf("  Timing %2u:\t%5u msec\t%s" CLI_NL, idx + 1, cfg.timingMsec[idx], TIMING_TEXTS[idx]);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetTiming(const CliParam_t no, CliParam_t msec, const CliParam_t param3)
{
  if ((no == 0) || (no > HOT_SUPPLY_TIMING_Last)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  msec = MIN_VAL(msec, 60u * 1000u);
  
  cfg.timingMsec[no - 1] = msec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(hot_supply)
  CLI_ENTRY0( "show",   "Show hot supply controller", CliHotSupplyShowAll)
  CLI_ENTRY0( "scfg",   "Show hot supply configuration", CliShowConfig)

  CLI_ENTRY2( "tim",    "Set hot supply timing [no:1..max, msec:0-60000]", CliSetTiming, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(hot_supply)

/******************************************************************************/
#endif
