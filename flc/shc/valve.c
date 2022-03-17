/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/elog/elog.h"

#include "hal/dio/dio.h"
#include "hal/pwm/pwm.h"

#include "pow/pow.h"

#include "valve.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT         (VALVE_Last - VALVE_First)

// Normally open, so 100% duty cycle closes the valve
#define NO_OPEN_PERMIL   (   0u)
#define NO_CLOSE_PERMIL  (1000u)

// Normally closed, so 100% duty cycle opens the valve
#define NC_OPEN_PERMIL   (1000u)
#define NC_CLOSE_PERMIL  (   0u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  HalPwmCh_t channel;
} HwCfg_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const HwCfg_t HW_CFG[HW_COUNT] = {
  { PWM_VALVE1  },
#if !defined(EVK)
  { PWM_VALVE2  },
  { PWM_VALVE3  },
  { PWM_VALVE4  },
  { PWM_VALVE5  },
  { PWM_VALVE6  },
  { PWM_VALVE7  },
  { PWM_VALVE8  },
  { PWM_VALVE9  },
  { PWM_VALVE10 },
#endif  
};

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static const FlcComponentMapping_t * appCfg;

static uint16_t pwmOutputPermil[HW_COUNT];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static bool ComponentIndexIsValid(FlcHwComponent_t valve);
static bool TypeIsNormallyOpen(FlcHwComponent_t valve);
static void SetPwmOutput(uint8_t index, uint16_t permil);
static void ZeroPwmOutput(uint8_t index);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool ValveInit(const FlcComponentMapping_t * const cfg)
{
  TRACE(TRC_TA_SHC, TRC_TL_COMPONENT, "ValveInit()");
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = VALVE_First; idx < VALVE_Last; idx++) {
    if ((cfg[idx].type != VALVE) || (cfg[idx].subtype >= VALVE_TYPE_Last)) {
      TRACE(TRC_TA_SHC, TRC_TL_FATAL, "Valve config invalid");
      EVENT_LOG_ADD_S("Valve config invalid");
      return false;
    }
  }
  appCfg = cfg;
  
  // Default valve states at init is equal to the valve states when power is off
  // so all valves are set to zero duty cycle.
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    ZeroPwmOutput(idx);
  }
  
#if !defined(EVK)  
  PowerSet(POWER_VALVES, true);
#endif
  
  return true;
}

void ValveOpen(const FlcHwComponent_t valve)
{
  if (!ComponentIndexIsValid(valve)) return;

   const bool isNO = TypeIsNormallyOpen(valve);
   SetPwmOutput(valve - VALVE_First, (isNO ? NO_OPEN_PERMIL : NC_OPEN_PERMIL));
}

void ValveClose(const FlcHwComponent_t valve)
{
  if (!ComponentIndexIsValid(valve)) return;
  
   const bool isNO = TypeIsNormallyOpen(valve);
   SetPwmOutput(valve - VALVE_First, (isNO ? NO_CLOSE_PERMIL : NC_CLOSE_PERMIL));
}

FlcValveType_t ValveGetType(const FlcHwComponent_t valve)
{
  if (!ComponentIndexIsValid(valve)) return VALVE_NORMALLY_OPEN;
  
  return appCfg[valve].subtype;
}

uint16_t ValveGetDutyCycle(const FlcHwComponent_t valve)
{
  if (!ComponentIndexIsValid(valve)) return 0u;
  
  return pwmOutputPermil[valve - VALVE_First];
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool ComponentIndexIsValid(const FlcHwComponent_t valve)
{
  return ((valve >= VALVE_First) && (valve < VALVE_Last));
}

static bool TypeIsNormallyOpen(const FlcHwComponent_t valve)
{
  return (appCfg[valve].subtype == VALVE_NORMALLY_OPEN);
}

static void SetPwmOutput(const uint8_t index, const uint16_t permil)
{
  if (permil == pwmOutputPermil[index]) return;
  
  pwmOutputPermil[index] = permil;
  HalPwmStartPermil(HW_CFG[index].channel, permil);
}

static void ZeroPwmOutput(const uint8_t index)
{
  pwmOutputPermil[index] = 0;
  HalPwmStartPermil(HW_CFG[index].channel, 0);
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

static const char * const TYPE_TEXT[VALVE_TYPE_Last] = {
  "NO",     // VALVE_NORMALLY_OPEN,
  "NC",     // VALVE_NORMALLY_CLOSED,  
};

int_fast16_t CliValveShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Valve State..." CLI_NL);
  for (uint_fast8_t index = 0; index < HW_COUNT; index++) {
    CliPrintf("  Valve %2u:\t%s\t%u%%" CLI_NL, 
              index + 1,
              TYPE_TEXT[appCfg[index + VALVE_First].subtype],
              pwmOutputPermil[index] / 10);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliOpen(const CliParam_t no, const CliParam_t param2, const CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  ValveOpen(no - 1 + VALVE_First);
  return CLI_RESULT_OK;
}

static int_fast16_t CliClose(const CliParam_t no, const CliParam_t param2, const CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }  
  
  ValveClose(no - 1 + VALVE_First);
  return CLI_RESULT_OK;
}

CLI_START_TABLE(valve)
  CLI_ENTRY0( "show",   "Show all valves", CliValveShowAll)
  CLI_ENTRY1( "open",   "Open a valve [no:1..max]", CliOpen, CLI_PARAM_UINT32)
  CLI_ENTRY1( "close",  "Close a valve [no:1..max]", CliClose, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(valve)

/******************************************************************************/
#endif
