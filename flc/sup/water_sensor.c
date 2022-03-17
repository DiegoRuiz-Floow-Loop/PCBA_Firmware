/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <plf/nvm/nvm.h>
#include <plf/elog/elog.h>

#include <hal/dio/dio.h>

#include "adc_mux.h"
#include "water_sensor.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT      (WATER_SENSOR_Last - WATER_SENSOR_First)

#define ADC_REF_VOLT            (3.3f)
#define FLOOR_WATER_TRIGGER_PCT (0.5f)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const AdcMuxChannels_t WATER_SENSORS[] = {
#if defined(EVK)
  ADC_MUX_ITEM_5_1
#else
  ADC_MUX_SENSOR16A,
  ADC_MUX_SENSOR17A,
  ADC_MUX_SENSOR18A
#endif
};
static_assert(SIZEOF_ARRAY(WATER_SENSORS) == HW_COUNT, "WATER_SENSORS unexpected size");


/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static WaterSensor_t cfg;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static bool ComponentIndexIsValid(FlcHwComponent_t waterSensor);
static void ConfigLoad(void);
static void ConfigSave(void);
static void ConfigSetDefault(void);

/*******************************************************************************
 * Public functions
 ******************************************************************************/
 
bool WaterSensorInit(const FlcComponentMapping_t * const config)
{
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "WaterSensorInit()");  
  
  // Validate the configuration table
  for (FlcHwComponent_t idx = WATER_SENSOR_First; idx < WATER_SENSOR_Last; idx++) {
    if ((config[idx].type != WATER_SENSOR)) {
      TRACE(TRC_TA_SUP, TRC_TL_FATAL, "Water sensor config invalid");
      EVENT_LOG_ADD_S("Water sensor config invalid");
      return false;
    }
  }
  
  ConfigLoad();
  return true;  
}

bool WaterSensorRead(const FlcHwComponent_t waterSensor)
{
  if (!ComponentIndexIsValid(waterSensor)) {
    return false;
  }
  
  const uint_fast8_t idx = waterSensor - WATER_SENSOR_First;
  return (AdcMuxGetVolt(WATER_SENSORS[idx]) >= (cfg.levelTriggerPct[idx] * ADC_REF_VOLT));
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static bool ComponentIndexIsValid(const FlcHwComponent_t waterSensor)
{
  return ((waterSensor >= WATER_SENSOR_First) && (waterSensor < WATER_SENSOR_Last));
}

static void ConfigLoad(void)
{
  if (nvmState != NVM_OK) {
    ConfigSetDefault();
  } else {
    NvmRead(offsetof(NvmStruct_t, appl.app.waterSensor), (uint8_t *)&cfg, sizeof(WaterSensor_t));
    if (cfg.crc != CalcCrc16(offsetof(WaterSensor_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED)) {
      ConfigSetDefault();
    }
  }
}

static void ConfigSave(void)
{
  cfg.crc = CalcCrc16(offsetof(WaterSensor_t, crc), (uint8_t *)&cfg, CRC16_INITIAL_SEED);
  NvmWrite(offsetof(NvmStruct_t, appl.app.waterSensor), (uint8_t *)&cfg, sizeof(WaterSensor_t));
}

static void ConfigSetDefault(void)
{
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    cfg.levelTriggerPct[idx] = FLOOR_WATER_TRIGGER_PCT;
  }

  ConfigSave();
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t CliWaterSensorShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Water Sensor State..." CLI_NL);  
  for(uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    CliPrintf("  Water sensor %u:\t%s" CLI_NL, 
    idx + 1,
    (WaterSensorRead(idx + WATER_SENSOR_First) ? "HIGH" : "LOW"));
  }
  
  return CLI_RESULT_OK;
}

int_fast16_t CliShowConfig(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Water Sensor Config..." CLI_NL);  
  for(uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    CliPrintf("  Water sensor %u:\t%.0f%% full scale" CLI_NL, 
    idx + 1,
    (double)(cfg.levelTriggerPct[idx] * 100.0f));
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliSetLevelTrigger(const CliParam_t no, CliParam_t pct, const CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  pct = MIN_VAL(pct, 100u);
  
  cfg.levelTriggerPct[no - 1] = ((float)pct) / 100.0f;
  ConfigSave();
  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(water_sensors)
  CLI_ENTRY0( "show",   "Show all water sensors", CliWaterSensorShowAll)
  CLI_ENTRY0( "scfg",   "Show water sensor configuration", CliShowConfig)

  CLI_ENTRY2( "lvl",    "Set water sensor level trigger [no:1..max, pct:0-100]", CliSetLevelTrigger, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(water_sensors)

/******************************************************************************/
#endif
