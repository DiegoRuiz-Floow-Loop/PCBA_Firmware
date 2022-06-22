/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

// SHC - Shower Controller

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/evos/evos.h"
#include "plf/vtim/vtim.h"
#include "plf/nvm/nvm.h"
#include "plf/elog/elog.h"
#include "plf/led/led.h"

#include "app/flc_cfg.h"

#include "sup/pos_sensor.h"
#include "sup/water_sensor.h"
#include "sup/user_button.h"

#include "shc/flow_knob.h"
#include "shc/pump.h"
#include "shc/uv_lamp.h"
#include "shc/valve.h"

#include "shc.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define TICK_MSEC                   (100u)

#define IDLE_STARTUP_DELAY_MSEC     (             2u * 1000u)
static_assert(IDLE_STARTUP_DELAY_MSEC >= 1000, "Startup should wait 1 sec for ready drivers.");

#define IDLE_TO_LOW_POWER_MSEC      (       5u * 60u * 1000u)
#define PRE_BACKWASH_MSEC           (            15u * 1000u)
#define PRE_BACKWASH_REQUIRED_MSEC  (                     0u)
#define FLOW_STOP_MSEC              (       2u * 60u * 1000u)
#define EMPTY_AIRGAP_MSEC           (            10u * 1000u)
#define POST_BACKWASH_MSEC          (             8u * 1000u)
#define EMPTY_FINAL_MSEC            (            12u * 1000u)
#define LOOP_TO_SHOWER_MSEC         (             5u * 1000u)
#define SHOWER_TO_LOOP_MSEC         (             5u * 1000u) 
#define SHOWER_TO_LOOP_STARTUP_MSEC (            30u * 1000u)
#define FLOW_STOPPED_TO_LOOP_MSEC   (            10u * 1000u)


#define MAX_HYSTERESIS_SEC          (60)

#define PUMP_START                  (PUMP_FORWARD)
#define PUMP_MAX_PERMIL             (1000u)

#define UNKNOWN_STATE_SZ            "UNKNOWN STATE!"

#define IGNORE      (false)
#define UPDATE      (true)
#define OFF         (false)
#define ON          (true)
#define CLOSE       (false)
#define OPEN        (true)
  
#define SHC_FSM_TL  TRC_TL_2

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  ON_ENTRY,
  ON_EXIT,
  ON_TICK,
  ON_EVENT_Last
} ShcEvent_t;

typedef void (*StateFunc_t)(ShcEvent_t event);

typedef struct {
  bool update;
  bool on;
} OnEntryPower_t;

typedef struct {
  bool update;
  bool open;
} OnEntryValve_t;

/*******************************************************************************
 * Function prototypes, state machine
 ******************************************************************************/

static void StateIdle(ShcEvent_t event);
static void StatePreBackwash(ShcEvent_t event);
static void StateShower(ShcEvent_t event);
static void StateShowerLoop(ShcEvent_t event);
static void StateFlowStop(ShcEvent_t event);
static void StateEmptyAirgap(ShcEvent_t event);
static void StatePostBackwash(ShcEvent_t event);
static void StateEmptyFinal(ShcEvent_t event);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const StateFunc_t STATE_FUNCS[] = {
  StateIdle,              // SHC_IDLE
  StatePreBackwash,       // SHC_PRE_BACKWASH
  StateShower,            // SHC_SHOWER
  StateShowerLoop,        // SHC_SHOWER_LOOP
  StateFlowStop,          // SHC_FLOW_STOP
  StateEmptyAirgap,       // SHC_EMPTY_AIRGAP
  StatePostBackwash,      // SHC_POST_BACKWASH
  StateEmptyFinal,        // SHC_EMPTY_FINAL
};
static_assert(SIZEOF_ARRAY(STATE_FUNCS) == SHC_STATE_Last, "Wrong table row count!");

static const char * const STATE_TEXTS[] = {
  "shc idle",             // SHC_IDLE
  "shc pre backwash",     // SHC_PRE_BACKWASH
  "shc shower",           // SHC_SHOWER
  "shc shower loop",      // SHC_SHOWER_LOOP
  "shc flow stop",        // SHC_FLOW_STOP
  "shc empty airgap",     // SHC_EMPTY_AIRGAP
  "shc post backwash",    // SHC_POST_BACKWASH
  "shc empty final",      // SHC_EMPTY_FINAL
};
static_assert(SIZEOF_ARRAY(STATE_TEXTS) == SHC_STATE_Last, "Wrong table row count!");

static const FlcAppComponent_t POWER_COMPONENTS[] = {
  UV_LAMP,
  LOOP_PUMP,
  DELIVERY_PUMP,
};

static const OnEntryPower_t POWER_ON_ENTRY[][SIZEOF_ARRAY(POWER_COMPONENTS)] = {
  // UV Lamp      Loop pump      Delivery pump
  {{IGNORE, OFF}, {UPDATE, OFF}, {UPDATE, OFF}},  // SHC_IDLE
  {{UPDATE, ON }, {UPDATE, OFF}, {UPDATE, ON }},  // SHC_PRE_BACKWASH 
  {{UPDATE, ON }, {UPDATE, OFF}, {UPDATE, ON }},  // SHC_SHOWER
  {{IGNORE, OFF}, {UPDATE, ON }, {UPDATE, ON }},  // SHC_SHOWER_LOOP
  {{IGNORE, OFF}, {UPDATE, OFF}, {UPDATE, OFF}},  // SHC_FLOW_STOP
  {{IGNORE, OFF}, {UPDATE, OFF}, {UPDATE, ON }},  // SHC_EMPTY_AIRGAP
  {{IGNORE, OFF}, {UPDATE, OFF}, {UPDATE, ON }},  // SHC_POST_BACKWASH
  {{IGNORE, OFF}, {UPDATE, OFF}, {UPDATE, OFF}},  // SHC_EMPTY_FINAL
};
static_assert(SIZEOF_ARRAY(POWER_ON_ENTRY) == SHC_STATE_Last, "Wrong table row count!");

// Some valves are handled by other state machines so they are omitted here:
// - hot_supply.c  handles HOT_TANK_VALVE
// - cold_supply.c handles MIXED_TANK_VALVE
static const FlcAppComponent_t VALVE_COMPONENTS[] = {
  HAND_HEAD_DIVERTER_VALVE,
  DRAIN_HEAD_VALVE,
  DRAIN_DELIVERY_VALVE,
  DRAIN_LOOP_VALVE,
  EXTERNAL_RELAY_VALVE,
};

static const OnEntryValve_t VALVE_ON_ENTRY[][SIZEOF_ARRAY(VALVE_COMPONENTS)] = {
  //HAND_HEAD_DIVERTER_VALVE  DRAIN_HEAD_VALVE   DRAIN_DELIVERY_VALVE   DRAIN_LOOP_VALVE   EXTERNAL_RELAY_VALVE
  {{UPDATE, OPEN },           {UPDATE, OPEN },   {UPDATE, OPEN },       {UPDATE, OPEN },   {UPDATE, OPEN}},  // SHC_IDLE
  {{UPDATE, OPEN },           {UPDATE, CLOSE},   {UPDATE, OPEN },       {UPDATE, OPEN },   {UPDATE, CLOSE}}, // SHC_PRE_BACKWASH 
  {{IGNORE, CLOSE},           {UPDATE, OPEN },   {UPDATE, CLOSE},       {UPDATE, CLOSE},   {UPDATE, CLOSE}}, // SHC_SHOWER
  {{IGNORE, CLOSE},           {UPDATE, OPEN },   {UPDATE, CLOSE},       {UPDATE, CLOSE},   {UPDATE, CLOSE}}, // SHC_SHOWER_LOOP
  {{IGNORE, CLOSE},           {UPDATE, OPEN },   {UPDATE, CLOSE},       {UPDATE, CLOSE},   {IGNORE, CLOSE}}, // SHC_FLOW_STOP
  {{UPDATE, OPEN },           {UPDATE, CLOSE},   {UPDATE, OPEN },       {UPDATE, OPEN },   {IGNORE, CLOSE}}, // SHC_EMPTY_AIRGAP
  {{UPDATE, OPEN },           {UPDATE, CLOSE},   {UPDATE, OPEN },       {UPDATE, OPEN },   {IGNORE, CLOSE}}, // SHC_POST_BACKWASH
  {{UPDATE, OPEN },           {UPDATE, OPEN },   {UPDATE, OPEN },       {UPDATE, OPEN },   {IGNORE, CLOSE}}, // SHC_EMPTY_FINAL
};
static_assert(SIZEOF_ARRAY(VALVE_ON_ENTRY) == SHC_STATE_Last, "Wrong table row count!");

static const char * const STATE_NAMES[] = {
  "SHC_IDLE",                 // SHC_IDLE
  "SHC_PRE_BACKWASH",         // SHC_PRE_BACKWASH 
  "SHC_SHOWER",               // SHC_SHOWER
  "SHC_SHOWER_LOOP",          // SHC_SHOWER_LOOP
  "SHC_FLOW_STOP",            // SHC_FLOW_STOP
  "SHC_EMPTY_AIRGAP",         // SHC_EMPTY_AIRGAP
  "SHC_POST_BACKWASH",        // SHC_POST_BACKWASH
  "SHC_EMPTY_FINAL"           // SHC_EMPTY_FINAL  
};
static_assert(SIZEOF_ARRAY(STATE_NAMES) == SHC_STATE_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static Shc_t cfg;

static EvosEventHandle_t tick = EVOS_UNINITIALIZED_HANDLE;

static bool systemInEmergencyStop;

static ShcState_t currentState;
static ShcState_t nextState;
static bool nextIsLocked;

static uint32_t showerLoopMsec;

static bool showerLoopButtonPressed = false;

static bool nextStateIsLoop = true;
static bool firstTimeInShower = true;
static bool toShowerFromCleaning = false;


/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static void EventProcess(ShcEvent_t event);
static void Transistion(ShcState_t state);

// State machine helpers
static void EvosEvent(EvosEventParam_t param);
static void SetPowersOnEntry(void);
static void SetValvesOnEntry(void);
static void SetPump(FlcAppComponent_t comp, uint16_t permil);
static void SetValve(FlcAppComponent_t comp, bool open);
static bool KnobIsAnyShowerOn(void);
static bool WaterIsOnFloor(void);
static bool GetShowerLoopButton(void);
static void ConsumeShowerLoopButtonEvent(void);
static void SetShowerStateLedOnEntry(void);

// Configuration
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool SHC_Init(void)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "SHC_Init()");

  ConfigLoad();  
  
  // HW component output drivers  
  if (!UvLampInit(flcCfg)) return false;
  if (!ValveInit(flcCfg)) return false;
  if (!PumpInit(flcCfg)) return false;

  //Turn off ShowerStateLed
  LedOn(LED_BTN_1);

  tick = EvosEventRegister(EvosEvent, "shc tick");
  EvosEventSetAndReload(tick, cfg.timingMsec[SHC_TIMING_IDLE_STARTUP_DELAY], TICK_MSEC, ON_TICK);  
  
  return true;
}

void ShcSetEmergencyStop(void)
{
  systemInEmergencyStop = true;
}

ShcState_t ShcGetState(void)
{
  return currentState;
}

const char * ShcGetStateName(const ShcState_t state)
{ 
  if (state >= SHC_STATE_Last) {
    return UNKNOWN_STATE_SZ;
  }
  return STATE_NAMES[state];
}

void XPumpCurrentFault(const FlcHwComponent_t pump)
{
  static char sz[32];
  snprintf(sz, sizeof(sz), "pump %u current fault", pump - PUMP_First + 1);
  
  TRACE_VA(TRC_TA_SHC, TRC_TL_FATAL, "%s", sz);  
  EVENT_LOG_ADD_S(sz);
}

__WEAK void XShcStateChanged(const ShcState_t state)
{
}

__WEAK void XShcShowerIsDone(void)
{
}

__WEAK void XShcLowPowerChanged(const bool isLowPower)
{
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void EventProcess(const ShcEvent_t event)
{
  STATE_FUNCS[currentState](event);

  // In emergency stop always overwrite any state machine transistions to idle.
  if (systemInEmergencyStop) {
    Transistion(SHC_IDLE);
  }
  
  while (currentState != nextState) {
    // State transistions are not allowed on exit events to avoid transistioning
    // to a wrong state on programmer error/forgetfulness. 
    nextIsLocked = true;
    STATE_FUNCS[currentState](ON_EXIT);
    nextIsLocked = false;

    currentState = nextState;
    TrcTrace(TRC_TA_SHC, SHC_FSM_TL, STATE_TEXTS[currentState]);
    SetPowersOnEntry();
    SetValvesOnEntry();
    SetShowerStateLedOnEntry();
    STATE_FUNCS[currentState](ON_ENTRY);
    XShcStateChanged(currentState);
  }
}

static void Transistion(const ShcState_t state)
{
  if (!nextIsLocked) {
    nextState = state;
  }
}

static void StateIdle(const ShcEvent_t event)
{
  static bool inLowPower = true;
  static VTim_t lowPowerTimer;
  
  static bool lastShowerIsRecorded = false;
  static VTim_t lastShowerTimer; 
  
  switch (event) {
    case ON_ENTRY:
      // On entry is not executed on init because the state machine already is 
      // in idle state. This means that low power and last shower variables are
      // not cleared at init.
    
      inLowPower = false;
      VTimSetMsec(&lowPowerTimer, cfg.timingMsec[SHC_TIMING_IDLE_TO_LOW_POWER]);
    
      lastShowerIsRecorded = true;
      VTimSetMsec(&lastShowerTimer, cfg.timingMsec[SHC_TIMING_PRE_BACKWASH_REQUIRED]);
    
      XShcShowerIsDone();
      break;

    case ON_EXIT:
      showerLoopMsec = 0;
      if(inLowPower) {
        XShcLowPowerChanged(false);
      }
      break;
    
    case ON_TICK:
      if (!inLowPower) {
        if (VTimIsExpired(&lowPowerTimer) || systemInEmergencyStop) {
          inLowPower = true;
          XShcLowPowerChanged(true);
          
          // The UV lamp output is ignored on entry because it should only be 
          // turned off after a low power delay. This done to improve the UV lamp
          // life time with less on/off changes.
          UvLampSet(OFF);         
        }
      }
      
      if (systemInEmergencyStop) {
        break;
      }
      
      if (KnobIsAnyShowerOn()) {
        if (!lastShowerIsRecorded || VTimIsExpired(&lastShowerTimer)) {
          Transistion(SHC_PRE_BACKWASH);
        } else {
          firstTimeInShower = true;
          nextStateIsLoop = true;
          Transistion(SHC_SHOWER);
        }
      }
      break;
    
    default:
      break;
  }
}  

static void StatePreBackwash(const ShcEvent_t event)
{
  static VTim_t stateTimer;
  
  switch (event) {
    case ON_ENTRY:
      SetPump(DELIVERY_PUMP, PUMP_MAX_PERMIL);
    
      VTimSetMsec(&stateTimer, cfg.timingMsec[SHC_TIMING_PRE_BACKWASH]);
      break;    
    
    case ON_TICK:
      if (VTimIsExpired(&stateTimer)) {
        firstTimeInShower = true;
        nextStateIsLoop = true;
        Transistion(SHC_SHOWER);
      }
      break;

    case ON_EXIT:    
    default:
      break;
  }
}

static void StateShower(const ShcEvent_t event)
{
  static VTim_t showerToLoopTimer;

  switch (event) {
    case ON_ENTRY:
      //Making sure it's not expired when entering the state
      if (toShowerFromCleaning) {
        VTimSetMsec(&showerToLoopTimer, cfg.timingMsec[SHC_TIMING_FLOW_STOPPED_TO_LOOP_TIMEOUT]);
      } else {
        VTimSetMsec(&showerToLoopTimer, cfg.timingMsec[SHC_TIMING_SHOWER_TO_LOOP_TIMEOUT]);
      }

      if (nextStateIsLoop && (firstTimeInShower || toShowerFromCleaning)) {
        LedFlash(LED_BTN_1, 1000, 100);
      } 

      // FALLTHROUGH TO ON_TICK

    case ON_TICK:
      // Shower valves are ignored on entry because they are set according to
      // the flow knob position. This is also the reason for fall-through from
      // ON_ENTRY to ON_TICK.
#ifndef PCB_TEST
      if (FlowKnobIsHeadOn()) {
        SetValve(HAND_HEAD_DIVERTER_VALVE, CLOSE);
        SetPump(DELIVERY_PUMP, FlowKnobGetHeadPowerPermil());
      } else if (FlowKnobIsHandOn()) {
        SetValve(HAND_HEAD_DIVERTER_VALVE, OPEN);
        SetPump(DELIVERY_PUMP, FlowKnobGetHandPowerPermil());
      } else {
        Transistion(SHC_FLOW_STOP);
        break;
      }
#endif
      
      if (UvLampIsOn()) {
        if (GetShowerLoopButton()) {

          if (firstTimeInShower || toShowerFromCleaning) {
            nextStateIsLoop = nextStateIsLoop ? false : true;
            if (nextStateIsLoop) {
              LedFlash(LED_BTN_1, 1000, 100);
            } else {
              LedOn(LED_BTN_1);
            }
          } else {
            nextStateIsLoop = true;
            Transistion(SHC_SHOWER_LOOP);
          }

          ConsumeShowerLoopButtonEvent();
        }

        if (firstTimeInShower || toShowerFromCleaning) {
          if (nextStateIsLoop && VTimIsExpired(&showerToLoopTimer)) {
            nextStateIsLoop = true;
            firstTimeInShower = false;
            Transistion(SHC_SHOWER_LOOP);
          }
        }
      }
      break;
    
    case ON_EXIT:
      // Making sure shower state indicator LED is turned off
      toShowerFromCleaning = false; 
      LedOn(LED_BTN_1);
    default:
      break;
  }
}

static void StateShowerLoop(const ShcEvent_t event)
{
  static VTim_t loopToShowerHysteresisTimer;
  switch (event) {
    case ON_ENTRY:
      LedRGB(LRGB_GREEN);
      //Making sure it's not expired when entering the state
      VTimSetMsec(&loopToShowerHysteresisTimer, cfg.timingMsec[SHC_TIMING_LOOP_TO_SHOWER_HYSTERESIS]);

      // FALLTHROUGH TO ON_TICK
    
    case ON_TICK:
      showerLoopMsec += ((event == ON_TICK) ? TICK_MSEC : 0);

      // Shower valves are ignored on entry because they are set according to
      // the flow knob position. This is also the reason for fall-through from
      // ON_ENTRY to ON_TICK.
#ifndef PCB_TEST    
      if (FlowKnobIsHeadOn()) {
        SetValve(HAND_HEAD_DIVERTER_VALVE, CLOSE);
        SetPump(DELIVERY_PUMP, FlowKnobGetHeadPowerPermil());
      } else if (FlowKnobIsHandOn()) {
        SetValve(HAND_HEAD_DIVERTER_VALVE, OPEN);
        SetPump(DELIVERY_PUMP, FlowKnobGetHandPowerPermil());     
      } else {
        Transistion(SHC_FLOW_STOP);
        break;
      }
#endif
      if (!(UvLampIsOn()) || GetShowerLoopButton()) {
        ConsumeShowerLoopButtonEvent();
        nextStateIsLoop = false;
        Transistion(SHC_SHOWER);
      }

      // if (!WaterIsOnFloor()) {
      //   if (VTimIsExpired(&loopToShowerHysteresisTimer)) {
      //     Transistion(SHC_SHOWER);
      //   }
      // } else {
      //   VTimSetMsec(&loopToShowerHysteresisTimer, cfg.timingMsec[SHC_TIMING_LOOP_TO_SHOWER_HYSTERESIS]);
      // }


      break;

    case ON_EXIT:

      LedRGB(LRGB_BLACK);
      break;	
		
    default:
      break;
  }
}

static void StateFlowStop(const ShcEvent_t event)
{
  static VTim_t stateTimer;
  
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&stateTimer, cfg.timingMsec[SHC_TIMING_FLOW_STOP]);
      break;    
    
    case ON_TICK:
    
      if (KnobIsAnyShowerOn()) {
        toShowerFromCleaning = true;
        Transistion(SHC_SHOWER);
      } else if (VTimIsExpired(&stateTimer)) {
        Transistion(SHC_EMPTY_AIRGAP);
      }
      break;

    case ON_EXIT:    
    default:
      break;
  }
}

static void StateEmptyAirgap(const ShcEvent_t event)
{
  static VTim_t stateTimer;
  
  switch (event) {
    case ON_ENTRY:
      SetPump(DELIVERY_PUMP, PUMP_MAX_PERMIL);
    
      VTimSetMsec(&stateTimer, cfg.timingMsec[SHC_TIMING_EMPTY_AIRGAP]);
      break;    
    
    case ON_TICK:
      if (KnobIsAnyShowerOn()) {
        toShowerFromCleaning = true;
        Transistion(SHC_SHOWER);
      } else if (VTimIsExpired(&stateTimer)) {
        Transistion(SHC_POST_BACKWASH);
      }
      break;

    case ON_EXIT:    
    default:
      break;
  }
}

static void StatePostBackwash(const ShcEvent_t event)
{
  static VTim_t stateTimer;
  
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&stateTimer, cfg.timingMsec[SHC_TIMING_POST_BACKWASH]);
      break;    
    
    case ON_TICK:
      if (KnobIsAnyShowerOn()) {
        toShowerFromCleaning = true;
        Transistion(SHC_SHOWER);
      } if (VTimIsExpired(&stateTimer)) {
        Transistion(SHC_EMPTY_FINAL);
      }
      break;

    case ON_EXIT:    
    default:
      break;
  }
}

static void StateEmptyFinal(const ShcEvent_t event)
{
  static VTim_t stateTimer;
  
  switch (event) {
    case ON_ENTRY:
      VTimSetMsec(&stateTimer, cfg.timingMsec[SHC_TIMING_EMPTY_FINAL]);
      break;    
    
    case ON_TICK:
      if (KnobIsAnyShowerOn()) {
        toShowerFromCleaning = true;
        Transistion(SHC_SHOWER);
      } else if (VTimIsExpired(&stateTimer)) {
        Transistion(SHC_IDLE);
      }
      break;

    case ON_EXIT:    
    default:
      break;
  }
}

/*******************************************************************************
 * Local functions, state machine helpers
 ******************************************************************************/

static void EvosEvent(const EvosEventParam_t param)
{
  EventProcess(param);
}

static void SetPowersOnEntry(void)
{
  for (uint_fast8_t idx = 0; idx < SIZEOF_ARRAY(POWER_COMPONENTS); idx++) {
    if (POWER_ON_ENTRY[currentState][idx].update) {
      
      const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(POWER_COMPONENTS[idx]);
      const bool shouldTurnOn = POWER_ON_ENTRY[currentState][idx].on;

      if (flcCfg[hwComp].type == UV_LAMP_BALLAST) {
        UvLampSet(shouldTurnOn);
      } else if (flcCfg[hwComp].type == PUMP) {
        PumpSetState(hwComp, (shouldTurnOn ? PUMP_START : PUMP_STOP));
      }
    }
  }
}

static void SetValvesOnEntry(void)
{
  for (uint_fast8_t idx = 0; idx < SIZEOF_ARRAY(VALVE_COMPONENTS); idx++) {
    if (VALVE_ON_ENTRY[currentState][idx].update) {
      SetValve(VALVE_COMPONENTS[idx], VALVE_ON_ENTRY[currentState][idx].open);
    }
  }  
}

static void SetPump(const FlcAppComponent_t comp, const uint16_t permil)
{
  const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(comp);
  PumpSetPower(hwComp, permil);
}

static void SetValve(const FlcAppComponent_t comp, const bool open)
{
  const FlcHwComponent_t hwComp = FlcCfgAppToHwComponent(comp);
  if (open) {
    ValveOpen(hwComp);
  } else {
    ValveClose(hwComp);
  }
}

static bool KnobIsAnyShowerOn(void)
{
#ifndef PCB_TEST  
  return (FlowKnobIsHeadOn() || FlowKnobIsHandOn());
#else
  bool returnVal = UserButtonRead(USER_BUTTON_2);
  // ConsumeShowerLoopButtonEvent();
  return returnVal;
#endif
}

static bool WaterIsOnFloor(void)
{
  const FlcHwComponent_t hwIdx = FlcCfgAppToHwComponent(FLOOR_WATER_LEVEL_SENSOR);
  return WaterSensorRead(hwIdx);
}

static void ConsumeShowerLoopButtonEvent(void)
{
  SetShowerLoopButton(false);
}

static bool GetShowerLoopButton(void)
{
  return showerLoopButtonPressed;
}

void SetShowerLoopButton(bool level)
{
  showerLoopButtonPressed = level;
}

static void SetShowerStateLedOnEntry(void)
{
  if (currentState == SHC_SHOWER_LOOP) {
    LedOff(LED_BTN_1);
  } else {
    LedOn(LED_BTN_1);
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
    NvmRead(offsetof(NvmStruct_t, appl.app.shc), (uint8_t *)&cfg, sizeof(Shc_t));
    if (cfg.crc != CalcCrc16(offsetof(Shc_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(Shc_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.shc), (uint8_t *)&cfg, sizeof(Shc_t));
}

static void ConfigSetDefault(void)
{
  cfg.timingMsec[SHC_TIMING_IDLE_STARTUP_DELAY]             = IDLE_STARTUP_DELAY_MSEC;
  cfg.timingMsec[SHC_TIMING_IDLE_TO_LOW_POWER]              = IDLE_TO_LOW_POWER_MSEC;
  cfg.timingMsec[SHC_TIMING_PRE_BACKWASH]                   = PRE_BACKWASH_MSEC;
  cfg.timingMsec[SHC_TIMING_PRE_BACKWASH_REQUIRED]          = PRE_BACKWASH_REQUIRED_MSEC;
  cfg.timingMsec[SHC_TIMING_FLOW_STOP]                      = FLOW_STOP_MSEC;
  cfg.timingMsec[SHC_TIMING_EMPTY_AIRGAP]                   = EMPTY_AIRGAP_MSEC;
  cfg.timingMsec[SHC_TIMING_POST_BACKWASH]                  = POST_BACKWASH_MSEC;
  cfg.timingMsec[SHC_TIMING_EMPTY_FINAL]                    = EMPTY_FINAL_MSEC;
  cfg.timingMsec[SHC_TIMING_SHOWER_TO_LOOP_HYSTERESIS]      = SHOWER_TO_LOOP_MSEC;
  cfg.timingMsec[SHC_TIMING_LOOP_TO_SHOWER_HYSTERESIS]      = LOOP_TO_SHOWER_MSEC;
  cfg.timingMsec[SHC_TIMING_SHOWER_TO_LOOP_TIMEOUT]         = SHOWER_TO_LOOP_STARTUP_MSEC;
  cfg.timingMsec[SHC_TIMING_FLOW_STOPPED_TO_LOOP_TIMEOUT]   = FLOW_STOPPED_TO_LOOP_MSEC;


  ConfigSave();
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

#include <string.h>

static const char * const TIMING_TEXTS[] = {
  "shc idle startup delay",             // SHC_TIMING_IDLE_STARTUP_DELAY
  "shc idle to low power",              // SHC_TIMING_IDLE_TO_LOW_POWER
  "shc pre backwash",                   // SHC_TIMING_PRE_BACKWASH
  "shc pre backwash required",          // SHC_TIMING_PRE_BACKWASH_REQUIRED
  "shc flow stop",                      // SHC_TIMING_FLOW_STOP
  "shc empty airgap",                   // SHC_TIMING_EMPTY_AIRGAP
  "shc post backwash",                  // SHC_TIMING_POST_BACKWASH
  "shc empty final",                    // SHC_TIMING_EMPTY_FINAL
  "shc shower to loop hysteresis",      // SHC_TIMING_SHOWER_TO_LOOP_HYSTERESIS
  "shc loop to shower hysteresis",      // SHC_TIMING_LOOP_TO_SHOWER_HYSTERESIS
  "shc shower to loop timeout",         // SHC_TIMING_SHOWER_TO_LOOP_TIMEOUT
  "shc flow stopped to loop timeout",   // SHC_TIMING_FLOW_STOPPED_TO_LOOP_TIMEOUT
};
static_assert(SIZEOF_ARRAY(TIMING_TEXTS) == SHC_TIMING_Last, "Wrong table row size!");

int_fast16_t CliShcShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Shower Controller..." CLI_NL);
  CliPrintf("  State  :\t%s %s" CLI_NL, STATE_TEXTS[currentState],
                                        (systemInEmergencyStop ? "(emergency stop)" : ""));
  CliPrintf("  In loop:\t%u sec" CLI_NL, showerLoopMsec / 1000u);
  
  CliWrite( "  Knob   :");
  if (!KnobIsAnyShowerOn()) {
    CliWrite("\toff" CLI_NL);
  } else {
    CliPrintf("\t%s\t%u%%" CLI_NL,
              (FlowKnobIsHeadOn() ? "head" : "hand"),
              (FlowKnobIsHeadOn() ? FlowKnobGetHeadPowerPermil() : FlowKnobGetHandPowerPermil()) / 10u);    
  }
  
  CliPrintf("  Floor  :\t%s" CLI_NL, (WaterIsOnFloor() ? "wet" : "dry"));
  CliPrintf("  UV lamp:\tfeedback %s" CLI_NL, (UvLampIsOn() ? "on" : "off"));
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Shower Controller Timings Config..." CLI_NL);
  for (uint_fast8_t idx = 0; idx < SHC_TIMING_Last; idx++) {
    CliPrintf("  Timing %2u:\t%5u sec\t%s" CLI_NL, idx + 1, 
                                                  cfg.timingMsec[idx] / 1000u, 
                                                  TIMING_TEXTS[idx]);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetTiming(const CliParam_t no, CliParam_t sec, const CliParam_t param3)
{
  if ((no == 0) || (no > SHC_TIMING_Last)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  } else if ((no > SHC_TIMING_EMPTY_FINAL) && (sec > MAX_HYSTERESIS_SEC)) {
    // Hysteresis timing set to more than 60 seconds
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  sec = MIN_VAL(sec, 24u * 60u * 60u);
  
  cfg.timingMsec[no - 1] = sec * 1000u;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetState(const CliParam_t no, CliParam_t sec, const CliParam_t param3)
{
  if (no > SHC_EMPTY_FINAL) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  Transistion((ShcState_t) no);

  return CLI_RESULT_OK;
}




CLI_START_TABLE(shc)
  CLI_ENTRY0( "show",   "Show shower controller", CliShcShowAll)
  CLI_ENTRY0( "scfg",   "Show shower controller configuration", CliShowConfig)

  CLI_ENTRY2( "tim",    "Set shower controller timing [no:1..max, sec:0-86400]", CliSetTiming, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY1( "setstate", "Set SHC state", CliSetState, CLI_PARAM_UINT32)
  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(shc)

/******************************************************************************/
#endif
