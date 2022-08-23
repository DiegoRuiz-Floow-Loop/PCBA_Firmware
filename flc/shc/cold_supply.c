/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"
#include "plf/nvm/nvm.h"
#include "plf/vtim/vtim.h"

#include "app/flc_cfg.h"

#include "sup/float_switch.h"

#include "shc/shc.h"
#include "shc/valve.h"

#include "cold_supply.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define CLOSE_DELAY_MSEC        (   0u)
#define NORMAL_OPEN_DELAY_MSEC  ( 200u)
#define IN_LOOP_OPEN_DELAY_MSEC (4000u)

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
  ON_IN_LOOP_CHANGE,
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
  "cold supply inactive",        // SUPPLY_INACTIVE
  "cold supply idle",            // SUPPLY_IDLE
  "cold supply delayed open",    // SUPPLY_DELAYED_OPEN
  "cold supply delayed close",   // SUPPLY_DELAYED_CLOSE
};
static_assert(SIZEOF_ARRAY(STATE_TEXTS) == SUPPLY_STATE_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static ColdSupply_t cfg;

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static State_t currentState;
static State_t nextState;
static bool nextIsLocked;

static bool showerInLoop;
static bool loopTankLevelDontUseDirectly;
static bool coldTankLevelDontUseDirectly;

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
static void SetColdSupplyValve(bool open);
static bool TankLevelIsReached(void);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ColdSupplyInit(void)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "ColdSupplyInit()");
  
  ConfigLoad();
  
  timer = EvosEventRegister(EvosEvent, "cold supply timer");
}

void ColdSupplySetShowerStateChange(const ShcState_t state)
{
  static ShcState_t lastShowerState;
  
  if (state == lastShowerState) return;
  
  const bool lastIsActive = ActiveInShowerState(lastShowerState);
  const bool nowIsActive  = ActiveInShowerState(state);
  if (BOOL_NOT_EQUAL(lastIsActive, nowIsActive)) {
    EventProcess((nowIsActive ? ON_ACTIVATE : ON_DEACTIVATE));  
  }
  
  const bool nowInLoop = (state == SHC_SHOWER_LOOP);
  if (BOOL_NOT_EQUAL(showerInLoop, nowInLoop)) {
    showerInLoop = nowInLoop; 
    EventProcess(ON_IN_LOOP_CHANGE);  
  }
  
  lastShowerState = state;
}

void ColdSupplySetTankLoopLevelChanged(const bool level)
{
  if (level == loopTankLevelDontUseDirectly) return;
  
  // Tank level is dependent on both tank loop level and cold level so tank 
  // level function wrapper is used before and after setting loop level variable
  // to find out if there is an level change event.
  const bool oldWrappedLevel = TankLevelIsReached();
  loopTankLevelDontUseDirectly = level;
  const bool newWrappedLevel = TankLevelIsReached();  
  
  if (BOOL_NOT_EQUAL(oldWrappedLevel, newWrappedLevel)) {
    EventProcess(ON_LEVEL_CHANGE);
  }
}

void ColdSupplySetTankColdLevelChanged(const bool level)
{
  if (level == coldTankLevelDontUseDirectly) return;
  
  // Tank level is dependent on both tank loop level and cold level so tank 
  // level function wrapper is used before and after setting cold level variable
  // to find out if there is an level change event.
  const bool oldWrappedLevel = TankLevelIsReached();
  coldTankLevelDontUseDirectly = level;
  const bool newWrappedLevel = TankLevelIsReached();  
  
  if (BOOL_NOT_EQUAL(oldWrappedLevel, newWrappedLevel)) {
    EventProcess(ON_LEVEL_CHANGE);
  }  
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
      SetColdSupplyValve(CLOSE);
      break;

    case ON_EXIT:
      SetColdSupplyValve(TankLevelIsReached() ? CLOSE : OPEN);
      break;
    
    case ON_ACTIVATE:
      Transistion(SUPPLY_IDLE);
      break;

    case ON_TIMEOUT:
    case ON_LEVEL_CHANGE:      
    case ON_DEACTIVATE:
    case ON_IN_LOOP_CHANGE:  
    case ON_EVENT_Last:
      break;
  }  
}

static void StateIdle(const Event_t event)
{
  switch (event) {
    case ON_LEVEL_CHANGE:
      Transistion((TankLevelIsReached() ? SUPPLY_DELAYED_CLOSE : SUPPLY_DELAYED_OPEN));
      break;
    
    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;
    
    case ON_ENTRY:
    case ON_EXIT:      
    case ON_TIMEOUT:  
    case ON_ACTIVATE:
    case ON_IN_LOOP_CHANGE:  
    case ON_EVENT_Last:
      break;
  }  
  
}

static void StateDelayedOpen(const Event_t event)
{
  static VTim_t normalOpenTimer;
  static VTim_t inLoopOpenTimer;
  
  switch (event) {
    case ON_ENTRY:
      if (showerInLoop) {
        TimerSet(cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_IN_LOOP_OPEN]);
      } else {
        TimerSet(cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_NORMAL_OPEN]);
      }
      
      VTimSetMsec(&normalOpenTimer, cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_NORMAL_OPEN]);
      VTimSetMsec(&inLoopOpenTimer, cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_IN_LOOP_OPEN]);
      break;
    
    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetColdSupplyValve(OPEN);
      Transistion(SUPPLY_IDLE);      
      break;
    
    case ON_LEVEL_CHANGE:
      Transistion(SUPPLY_IDLE);
      break;
    
    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;

    case ON_IN_LOOP_CHANGE:
      if ((showerInLoop  && VTimIsExpired(&inLoopOpenTimer)) ||
          (!showerInLoop && VTimIsExpired(&normalOpenTimer))) {
        SetColdSupplyValve(OPEN);
        Transistion(SUPPLY_IDLE);
      } else {
        TimerSet((showerInLoop ? VTimGetMsecLeft(&inLoopOpenTimer) : VTimGetMsecLeft(&normalOpenTimer)));
      }
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
      TimerSet(cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_CLOSE]);
      break;

    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetColdSupplyValve(CLOSE);
      Transistion(SUPPLY_IDLE);
      break;
    
    case ON_LEVEL_CHANGE:
      Transistion(SUPPLY_IDLE);
      break;

    case ON_DEACTIVATE:
      Transistion(SUPPLY_INACTIVE);
      break;

    case ON_ACTIVATE:
    case ON_IN_LOOP_CHANGE:  
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
  if (state > SHC_STATE_Last) return false;
  
  switch (state) {
    case SHC_PRE_BACKWASH:
    case SHC_SHOWER:
    case SHC_SHOWER_LOOP:
    case SHC_EMPTY_AIRGAP:	
      return true;
    
    case SHC_IDLE:
    case SHC_FLOW_STOP:
    case SHC_POST_BACKWASH:
    case SHC_EMPTY_FINAL:
    case SHC_EMERGENCY_STATE:  
    case SHC_STATE_Last:
      return false;
  }
}

static void SetColdSupplyValve(const bool open)
{
  const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(MIXED_TANK_VALVE);
  if (open) {
    ValveOpen(hwComp);
  } else {
    ValveClose(hwComp);
  }
}

static bool TankLevelIsReached(void)
{
  if (loopTankLevelDontUseDirectly) {
    return true;
  }
  
  return coldTankLevelDontUseDirectly;
}

/*******************************************************************************
 * Local functions, configuration
 ******************************************************************************/

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.coldSupply), (uint8_t *)&cfg, sizeof(ColdSupply_t));
    if (cfg.crc != CalcCrc16(offsetof(ColdSupply_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(ColdSupply_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.coldSupply), (uint8_t *)&cfg, sizeof(ColdSupply_t));
}

static void ConfigSetDefault(void)
{
  cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_CLOSE]        = CLOSE_DELAY_MSEC;
  cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_NORMAL_OPEN]  = NORMAL_OPEN_DELAY_MSEC;
  cfg.timingMsec[COLD_SUPPLY_TIMING_DELAYED_IN_LOOP_OPEN] = IN_LOOP_OPEN_DELAY_MSEC;

  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

static const char * const TIMING_TEXTS[] = {
  "cold supply close delay",          // COLD_SUPPLY_TIMING_DELAYED_CLOSE
  "cold supply normal open delay",    // COLD_SUPPLY_TIMING_DELAYED_NORMAL_OPEN
  "cold supply in loop open delay",   // COLD_SUPPLY_TIMING_DELAYED_IN_LOOP_OPEN
};
static_assert(SIZEOF_ARRAY(TIMING_TEXTS) == COLD_SUPPLY_TIMING_Last, "Wrong table row size!");

int_fast16_t CliColdSupplyShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Cold Supply Controller..." CLI_NL);
  CliPrintf("  State  :\t%s" CLI_NL, STATE_TEXTS[currentState]);
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Cold Supply Timings Config..." CLI_NL);
  for (uint_fast8_t idx = 0; idx < COLD_SUPPLY_TIMING_Last; idx++) {
    CliPrintf("  Timing %2u:\t%5u msec\t%s" CLI_NL, idx + 1, cfg.timingMsec[idx], TIMING_TEXTS[idx]);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetTiming(const CliParam_t no, CliParam_t msec, const CliParam_t param3)
{
  if ((no == 0) || (no > COLD_SUPPLY_TIMING_Last)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  msec = MIN_VAL(msec, 60u * 1000u);
  
  cfg.timingMsec[no - 1] = msec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(cold_supply)
  CLI_ENTRY0( "show",   "Show cold supply controller", CliColdSupplyShowAll)
  CLI_ENTRY0( "scfg",   "Show cold supply configuration", CliShowConfig)

  CLI_ENTRY2( "tim",    "Set cold supply timing [no:1..max, msec:0-60000]", CliSetTiming, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(cold_supply)

/******************************************************************************/
#endif
