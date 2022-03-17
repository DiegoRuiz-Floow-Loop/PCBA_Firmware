/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>
#include <plf/elog/elog.h>
#include <plf/util/linear_interpolation.h>

#include <sup/adc_mux.h>

#include "pos_sensor.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT        (POS_SENSOR_Last - POS_SENSOR_First)

// Information from schematic
#if !defined(EVK)
#define SUPPLY_VOLT     (5.0f)
#define GAIN_FACTOR     (0.6667f)
#else
#define SUPPLY_VOLT     (3.3f)
#define GAIN_FACTOR     (1.0f)
#endif

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  AdcMuxChannels_t channel;
} HwCfg_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const HwCfg_t HW_CFG[] = {
#if defined(EVK)
  { ADC_MUX_ITEM_4_1 },
#else
  { ADC_MUX_SENSOR15 },
#endif  
};
static_assert(SIZEOF_ARRAY(HW_CFG) == HW_COUNT, "Wrong table row size!");

// Vishay - Model 981 HE - 0-1000 permil equal to 10-90% Vsupply
static const LookupTablePoints_t VISHAY_981HE_PERMIL_ARRAY[]= {
  // Volt                Permil
  { 0.0f * SUPPLY_VOLT,    0.0f },
  { 0.1f * SUPPLY_VOLT,    0.0f },
  { 0.9f * SUPPLY_VOLT, 1000.0f },
  { 1.0f * SUPPLY_VOLT, 1000.0f },
};

static const LookupTable_t VISHAY_981HE_PERMIL_TABLE = {
  SIZEOF_ARRAY(VISHAY_981HE_PERMIL_ARRAY),
  VISHAY_981HE_PERMIL_ARRAY
};

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static const FlcComponentMapping_t * appCfg;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static bool ComponentIndexIsValid(FlcHwComponent_t sensor);
static float GetVolt(uint8_t index);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool PosSensorInit(const FlcComponentMapping_t * const cfg)
{
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "PosSensorInit()");
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = POS_SENSOR_First; idx < POS_SENSOR_Last; idx++) {
    if ((cfg[idx].type != POS_SENSOR)) {
      TRACE(TRC_TA_SUP, TRC_TL_FATAL, "Pos. sensor config invalid");
      EVENT_LOG_ADD_S("Pos. sensor config invalid");
      return false;
    }
  }
  appCfg = cfg;

  return true;
}

uint16_t PosSensorGetPermil(const FlcHwComponent_t sensor)
{
  if (!ComponentIndexIsValid(sensor)) return 0u;

  
  float permil;
  const float volt = GetVolt(sensor - POS_SENSOR_First);
  if (!LinearInterpolation(&VISHAY_981HE_PERMIL_TABLE, volt, &permil)) {
    return 0u;
  }
  return (uint16_t)permil;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool ComponentIndexIsValid(const FlcHwComponent_t sensor)
{
  return ((sensor >= POS_SENSOR_First) && (sensor < POS_SENSOR_Last));
}

static float GetVolt(const uint8_t index)
{
  return AdcMuxGetVolt(HW_CFG[index].channel) / GAIN_FACTOR;
}

/******************************************************************************/
#if defined(CLI_ENABLE)
/******************************************************************************/

int_fast16_t CliPosSensorShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Pos Sensor State..." CLI_NL);
  for (uint_fast8_t index = 0; index < HW_COUNT; index++) {
    CliPrintf("  Pos. sensor %u: \t%u permil\t%0.2f V" CLI_NL, 
              index + 1,
              PosSensorGetPermil(index + POS_SENSOR_First),
              (double)GetVolt(index));
  }
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(pos_sensor)
  CLI_ENTRY0( "show",   "Show all position sensors", CliPosSensorShowAll)
 
  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(pos_sensor)

/******************************************************************************/
#endif
