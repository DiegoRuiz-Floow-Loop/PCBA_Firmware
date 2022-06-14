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
#include <shc/pump.h>

#include "prime_pump.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define POWER_START_DELAY_MSEC  (100u) //(100u) is standard value
#define POWER_STOP_DELAY_MSEC   (  0u)

#define POWER_CHANGE_PERMIL     (30u)  //(160u) is standard value
#define POWER_PERMIL_MAX        (600u)

#define PUMP_FSM_TL     TRC_TL_3

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  PUMP_INACTIVE,
  PUMP_TANK_LEVEL_HIGH,
  PUMP_TANK_LEVEL_MEDIUM,
  PUMP_TANK_LEVEL_LOW,
  STATE_Last
} State_t;

typedef enum {
  ON_ENTRY,
  ON_EXIT,
  ON_TIMEOUT,
  ON_LOOP_ACTIVATE,
  ON_LOOP_DEACTIVATE,
  ON_LEVEL_CHANGE,
  ON_EVENT_Last
} Event_t;

typedef void (*StateFunc_t)(Event_t event);

/*******************************************************************************
 * Function prototypes, state machine
 ******************************************************************************/

static void StateInactive(Event_t event);
static void StateLevelHigh(Event_t event);
static void StateLevelMedium(Event_t event);
static void StateLevelLow(Event_t event);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const StateFunc_t STATE_FUNCS[] = {
  StateInactive,      // PUMP_INACTIVE
  StateLevelHigh,     // PUMP_TANK_LEVEL_HIGH
  StateLevelMedium,   // PUMP_TANK_LEVEL_MEDIUM
  StateLevelLow,      // PUMP_TANK_LEVEL_LOW
};
static_assert(SIZEOF_ARRAY(STATE_FUNCS) == STATE_Last, "Wrong table row count!");

static const char * const STATE_TEXTS[] = {
  "prime pump inactive",            // PUMP_INACTIVE
  "prime pump tank level high",     // PUMP_TANK_LEVEL_HIGH
  "prime pump tank level medium",   // PUMP_TANK_LEVEL_MEDIUM
  "prime pump tank level low",      // PUMP_TANK_LEVEL_LOW
};
static_assert(SIZEOF_ARRAY(STATE_TEXTS) == STATE_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static PrimePump_t cfg;

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static State_t currentState;
static State_t nextState;
static bool nextIsLocked;

static uint32_t powerAlgoPermil;
static uint32_t powerAlgoChangePermil;
static uint32_t powerOutputPermil;
static bool loopTankLevelDontUseDirectly;
static bool coldTankLevelDontUseDirectly;

static bool wasTankLevelLow;
static bool wasTankLevelHigh;

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
static void SetPrimePump(uint16_t permil);
static bool TankColdLevelIsReached(void);
static bool TankLoopLevelIsReached(void);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void PrimePumpInit(void)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "PrimePumpInit()");
  
  ConfigLoad();
  powerAlgoPermil = POWER_PERMIL_MAX;
  powerAlgoChangePermil = cfg.powerChangePermil;
  
  timer = EvosEventRegister(EvosEvent, "prime pump timer");
}

void PrimePumpSetShowerStateChange(const ShcState_t state)
{
  static ShcState_t lastShowerState;
  
  if (state == lastShowerState) return;
  
  const bool lastInLoop = (lastShowerState == SHC_SHOWER_LOOP);
  const bool nowInLoop  = (state == SHC_SHOWER_LOOP);
  if (BOOL_NOT_EQUAL(lastInLoop, nowInLoop)) {
    EventProcess((nowInLoop ? ON_LOOP_ACTIVATE : ON_LOOP_DEACTIVATE));  
  }
  
  lastShowerState = state;
}

void PrimePumpSetTankLoopLevelChanged(const bool level)
{
  if (level == loopTankLevelDontUseDirectly) return;

  // Wrapped function levels are dependent on both cold and loop levels so they 
  // are both checked to see if any level status changed. This is done for more
  // robust design because sometimes cold level switch status was wrong which 
  // caused an overflow in the tank. 
  
  const bool oldWrappedColdLevel = TankColdLevelIsReached();
  const bool oldWrappedLoopLevel = TankLoopLevelIsReached();
  
  loopTankLevelDontUseDirectly = level;
  
  if (BOOL_NOT_EQUAL(oldWrappedColdLevel, TankColdLevelIsReached()) ||
      BOOL_NOT_EQUAL(oldWrappedLoopLevel, TankLoopLevelIsReached())) {
    EventProcess(ON_LEVEL_CHANGE);
  }
}

void PrimePumpSetTankColdLevelChanged(const bool level)
{
  if (level == coldTankLevelDontUseDirectly) return;
  
  // Wrapped function levels are dependent on both cold and loop levels so they 
  // are both checked to see if any level status changed. This is done for more
  // robust design because sometimes cold level switch status was wrong which 
  // caused an overflow in the tank. 

  const bool oldWrappedColdLevel = TankColdLevelIsReached();
  const bool oldWrappedLoopLevel = TankLoopLevelIsReached();
  
  coldTankLevelDontUseDirectly = level;
  
  if (BOOL_NOT_EQUAL(oldWrappedColdLevel, TankColdLevelIsReached()) ||
      BOOL_NOT_EQUAL(oldWrappedLoopLevel, TankLoopLevelIsReached())) {
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
    TrcTrace(TRC_TA_SHC, PUMP_FSM_TL, STATE_TEXTS[currentState]);
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
      SetPrimePump(0);
      break;
    
    case ON_EXIT:
      if (TankLoopLevelIsReached()) break;
      SetPrimePump((TankColdLevelIsReached() ? powerAlgoPermil : POWER_PERMIL_MAX));
      break;
    
    case ON_LOOP_ACTIVATE:
      if (TankLoopLevelIsReached()) {
        Transistion(PUMP_TANK_LEVEL_HIGH);
      } else if (TankColdLevelIsReached()) {
        Transistion(PUMP_TANK_LEVEL_MEDIUM);
      } else {
        Transistion(PUMP_TANK_LEVEL_LOW);
      }
      break;

    case ON_TIMEOUT:
    case ON_LOOP_DEACTIVATE:
    case ON_LEVEL_CHANGE:
    case ON_EVENT_Last:
      break;
  }  
}

static void StateLevelHigh(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      wasTankLevelHigh = true;
      wasTankLevelLow  = false;
    
      if (powerOutputPermil > 0) {
        TimerSet(cfg.timingMsec[PRIME_PUMP_TIMING_DELAYED_POWER_STOP]);
      }
      break;
    
    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetPrimePump(0);
      break;
    
    case ON_LOOP_DEACTIVATE:
      Transistion(PUMP_INACTIVE);
      break;

    case ON_LEVEL_CHANGE:
      if (TankLoopLevelIsReached()) {
        // Do nothing
      } else if (TankColdLevelIsReached()) {
        Transistion(PUMP_TANK_LEVEL_MEDIUM);
      } else {
        Transistion(PUMP_TANK_LEVEL_LOW);
      }      
      break;

    case ON_LOOP_ACTIVATE:    
    case ON_EVENT_Last:
      break;  
  }
}

static void StateLevelMedium(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      if (wasTankLevelHigh) {
        TimerSet(cfg.timingMsec[PRIME_PUMP_TIMING_DELAYED_POWER_START]);
      } else {
        SetPrimePump(powerAlgoPermil);
      }
      break;
    
    case ON_EXIT:
      TimerClear();
      break;
    
    case ON_TIMEOUT:
      SetPrimePump(powerAlgoPermil);
      break;
    
    case ON_LOOP_DEACTIVATE:
      Transistion(PUMP_INACTIVE);    
      break;
    
    case ON_LEVEL_CHANGE:
      if (TankLoopLevelIsReached()) {
        // Update pump power algorithm
        if (wasTankLevelHigh) {
          powerAlgoChangePermil = cfg.powerChangePermil;
        } else if (wasTankLevelLow) {
          powerAlgoChangePermil /= 2;
        }
        if (powerAlgoPermil < powerAlgoChangePermil) {
          powerAlgoPermil = 0;
        } else {
          powerAlgoPermil -= powerAlgoChangePermil;
        }
        
        Transistion(PUMP_TANK_LEVEL_HIGH);
      } else if (TankColdLevelIsReached()) {
        // Do nothing
      } else {
        // Update pump power algorithm
        if (wasTankLevelHigh) {
          powerAlgoChangePermil /= 2;
        } else if (wasTankLevelLow) {
          powerAlgoChangePermil = cfg.powerChangePermil;
        }
        powerAlgoPermil += powerAlgoChangePermil;
        powerAlgoPermil = MIN_VAL(powerAlgoPermil, POWER_PERMIL_MAX);
      
        Transistion(PUMP_TANK_LEVEL_LOW);        
      }       
      break;
    
    case ON_LOOP_ACTIVATE:      
    case ON_EVENT_Last:
      break;  
  }  
}

static void StateLevelLow(const Event_t event)
{
  switch (event) {
    case ON_ENTRY:
      wasTankLevelHigh = false;
      wasTankLevelLow  = true;
    
      SetPrimePump(POWER_PERMIL_MAX);
      break;
    
    case ON_LOOP_DEACTIVATE:
      Transistion(PUMP_INACTIVE);    
      break;
    
    case ON_LEVEL_CHANGE:
      if (TankLoopLevelIsReached()) {
        Transistion(PUMP_TANK_LEVEL_HIGH);
      } else if (TankColdLevelIsReached()) {
        Transistion(PUMP_TANK_LEVEL_MEDIUM);
      } else {
        // Do nothing
      }      
      break;

    case ON_EXIT:
    case ON_LOOP_ACTIVATE:
    case ON_TIMEOUT:    
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

static void SetPrimePump(const uint16_t permil)
{
  powerOutputPermil = permil;

  const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(LOOP_PUMP);
  PumpSetPower(hwComp, permil);
}

static bool TankColdLevelIsReached(void)
{
  if (loopTankLevelDontUseDirectly) {
    return true;
  }
  
  return coldTankLevelDontUseDirectly;
}

static bool TankLoopLevelIsReached(void)
{
  return loopTankLevelDontUseDirectly;
}

/*******************************************************************************
 * Local functions, configuration
 ******************************************************************************/

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.primePump), (uint8_t *)&cfg, sizeof(PrimePump_t));
    if (cfg.crc != CalcCrc16(offsetof(PrimePump_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(PrimePump_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.primePump), (uint8_t *)&cfg, sizeof(PrimePump_t));
}

static void ConfigSetDefault(void)
{
  cfg.timingMsec[PRIME_PUMP_TIMING_DELAYED_POWER_START] = POWER_START_DELAY_MSEC;
  cfg.timingMsec[PRIME_PUMP_TIMING_DELAYED_POWER_STOP] = POWER_STOP_DELAY_MSEC;
  
  cfg.powerChangePermil = POWER_CHANGE_PERMIL;
  
  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

static const char * const TIMING_TEXTS[] = {
  "prime pump start delay",         // PRIME_PUMP_TIMING_DELAYED_POWER_START
  "prime pump stop delay",          // PRIME_PUMP_TIMING_DELAYED_POWER_STOP
};
static_assert(SIZEOF_ARRAY(TIMING_TEXTS) == PRIME_PUMP_TIMING_Last, "Wrong table row size!");

int_fast16_t CliPrimePumpShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Prime Pump Controller..." CLI_NL);
  CliPrintf("  State  :\t%s" CLI_NL, STATE_TEXTS[currentState]);
  
  CliPrintf("  Pump   :\tpower %u permil" CLI_NL, powerOutputPermil);
  CliPrintf("  Tank   :\tloop level %u\tcold level %u" CLI_NL, 
            TankLoopLevelIsReached(), 
            TankColdLevelIsReached());
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Prime Pump Timings Config..." CLI_NL);
  for (uint_fast8_t idx = 0; idx < PRIME_PUMP_TIMING_Last; idx++) {
    CliPrintf("  Timing %2u:\t%5u msec\t%s" CLI_NL, idx + 1, cfg.timingMsec[idx], TIMING_TEXTS[idx]);
  }
  
  CliWrite("Prime Pump Power Config..." CLI_NL);
  CliPrintf("  Power    :\t%5u permil\tprime pump power change" CLI_NL, cfg.powerChangePermil);
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetTiming(const CliParam_t no, CliParam_t msec, const CliParam_t param3)
{
  if ((no == 0) || (no > PRIME_PUMP_TIMING_Last)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  msec = MIN_VAL(msec, 60u * 1000u);
  
  cfg.timingMsec[no - 1] = msec;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetPowerChange(CliParam_t permil, const CliParam_t param2, const CliParam_t param3)
{
  permil = MIN_VAL(permil, POWER_PERMIL_MAX);
  
  cfg.powerChangePermil = permil;
  ConfigSave();

  return CLI_RESULT_OK;
}

CLI_START_TABLE(prime_pump)
  CLI_ENTRY0( "show",   "Show prime pump controller", CliPrimePumpShowAll)
  CLI_ENTRY0( "scfg",   "Show prime pump configuration", CliShowConfig)

  CLI_ENTRY2( "tim",    "Set prime pump timing [no:1..max, msec:0-60000]", CliSetTiming, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY1( "pwrc",   "Set prime pump power change [permil:0-700]", CliSetPowerChange, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(prime_pump)

/******************************************************************************/
#endif
