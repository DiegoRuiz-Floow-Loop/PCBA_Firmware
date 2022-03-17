/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <plf/elog/elog.h>
#include <plf/util/linear_interpolation.h>
#include <hal/dio/dio.h>
#include <app/flc_def.h>
#include <sup/adc_mux.h>

#include "log_sensors.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define HW_COUNT   (LOG_SENSOR_Last - LOG_SENSOR_First)
static const float NTC_SRC_VOLT = 3.3f;

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  const AdcMuxChannels_t channel;
  const float r1;           // For voltage and temperature
  const float r2;           // For voltage
} Sensor_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

// Grundfos VFS 1-18 l/min
static const LookupTablePoints_t GRUNDFOS_VFS_1_18_LPM_ARRAY[]= {
  //Volt     LPM (liter per minute)
  { 0.00f,    0.0f },
  { 0.28f,    0.0f },
  { 0.50f,    1.0f },
  { 3.50f,   15.0f },
  { 4.10f,   18.0f },
  { 5.00f,   18.0f },
};
static const LookupTable_t GRUNDFOS_VFS_1_18_LPM_TABLE = {
  SIZEOF_ARRAY(GRUNDFOS_VFS_1_18_LPM_ARRAY),
  GRUNDFOS_VFS_1_18_LPM_ARRAY
};

// Grundfos VFS 1-20 l/min
static const LookupTablePoints_t GRUNDFOS_VFS_1_20_LPM_ARRAY[]= {
  //Volt     LPM (liter per minute)
  { 0.00f,    0.0f },
  { 0.35f,    0.0f },
  { 0.50f,    1.3f },
  { 3.50f,   20.0f },
  { 5.00f,   20.0f },
};
static const LookupTable_t GRUNDFOS_VFS_1_20_LPM_TABLE = {
  SIZEOF_ARRAY(GRUNDFOS_VFS_1_20_LPM_ARRAY),
  GRUNDFOS_VFS_1_20_LPM_ARRAY
};

// Grundfos VFS 1-18 l/min and 1-20 l/min (temp. output is the same)
static const LookupTablePoints_t GRUNDFOS_VFS_DEGC_ARRAY[]= {
  //Volt     degC (degrees celsius)
  { 0.00f,     0.0f },
  { 0.50f,     0.0f },
  { 3.50f,   100.0f },
  { 4.10f,   120.0f },  
  { 5.00f,   120.0f },
};
static const LookupTable_t GRUNDFOS_VFS_DEGC_TABLE = {
  SIZEOF_ARRAY(GRUNDFOS_VFS_DEGC_ARRAY),
  GRUNDFOS_VFS_DEGC_ARRAY
};

static const McuPin_t SELECTOR_PINS[] =
{
#if defined(EVK)
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA
#else
  MP_SENSOR1_SEL,
  MP_SENSOR2_SEL,
  MP_SENSOR3_SEL,
  MP_SENSOR4_SEL,
  MP_SENSOR5_SEL,
  MP_SENSOR6_SEL,
  MP_SENSOR7_SEL,
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA,
  MCUPIN_NA
#endif
};
static_assert(SIZEOF_ARRAY(SELECTOR_PINS) == HW_COUNT, "LOG_SENSOR_SELECTOR_PINS unexpected size");

static const Sensor_t SENSORS[] = {
#if defined(EVK)
  { ADC_MUX_ITEM_1_1, 0.0f, 1.0f },
  { ADC_MUX_ITEM_2_1, 0.0f, 1.0f },
  { ADC_MUX_ITEM_3_1, 0.0f, 1.0f }
#else                     
  { ADC_MUX_SENSOR1,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR2,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR3,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR4,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR5,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR6,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR7,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR8,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR9,  10000.0f, 20000.0f },
  { ADC_MUX_SENSOR10, 10000.0f, 20000.0f },
  { ADC_MUX_SENSOR11, 10000.0f, 20000.0f },
  { ADC_MUX_SENSOR12, 10000.0f, 20000.0f },
  { ADC_MUX_SENSOR13, 10000.0f, 20000.0f },
  { ADC_MUX_SENSOR14, 10000.0f, 20000.0f }
#endif
};
static_assert(SIZEOF_ARRAY(SENSORS) == HW_COUNT, "SENSORS unexpected size");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static const FlcComponentMapping_t * configTable;

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// Convert voltage from the ADC reading to the source voltage of voltage divider.
static bool GetActualVoltage(const Sensor_t * sensor, float * const voltage);
static bool GetSensorResistance(const Sensor_t * sensor, float * const resistance);
static bool ComponentIndexIsValid(const FlcHwComponent_t sensor);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool LogSensorsInit(const FlcComponentMapping_t * const cfg)
{
  configTable = cfg;
  
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "LogSensorsInit()");
  
  for (FlcHwComponent_t logSensorIdx = LOG_SENSOR_First; logSensorIdx < LOG_SENSOR_Last; logSensorIdx++) {
    
    // Config is invalid
    if (configTable[logSensorIdx].type != LOG_SENSOR ||
        configTable[logSensorIdx].subtype >= LOG_SENSOR_TYPE_Last ||
       (configTable[logSensorIdx].subtype == LOG_SENSOR_NTC_OHM &&
        SELECTOR_PINS[logSensorIdx - LOG_SENSOR_First] == MCUPIN_NA)) {
          
      TRACE_VA(TRC_TA_SUP, TRC_TL_FATAL, "Log sensor config invalid at component index: %i", logSensorIdx);
      EVENT_LOG_ADD_S("Log sensor config invalid");
      return false;
    }
    
    if(configTable[logSensorIdx].subtype == LOG_SENSOR_NTC_OHM) {
      HAL_DIO_PIN_SET(SELECTOR_PINS[logSensorIdx - LOG_SENSOR_First]);
    } else {
      HAL_DIO_PIN_CLR(SELECTOR_PINS[logSensorIdx - LOG_SENSOR_First]);
    }
  }
  
  return true;
}

bool LogSensorGetValue(const FlcHwComponent_t sensor, float * const value)
{
  
  if (!ComponentIndexIsValid(sensor)) {
    return false;
  }
  
  float sensorVoltage = 0.0f;
  
  switch((FlcLogSensorType_t)configTable[sensor].subtype) {
    case LOG_SENSOR_UNUSED_VOLT:
      return GetActualVoltage(&SENSORS[sensor - LOG_SENSOR_First], value);
    
    case LOG_SENSOR_NTC_OHM:
      return GetSensorResistance(&SENSORS[sensor - LOG_SENSOR_First], value);
    
    case LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_18_LPM:      
      if (!GetActualVoltage(&SENSORS[sensor - LOG_SENSOR_First], &sensorVoltage)) {
        return false;
      }
      return LinearInterpolation(&GRUNDFOS_VFS_1_18_LPM_TABLE, sensorVoltage, value);

    case LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM:      
      if (!GetActualVoltage(&SENSORS[sensor - LOG_SENSOR_First], &sensorVoltage)) {
        return false;
      }
      return LinearInterpolation(&GRUNDFOS_VFS_1_20_LPM_TABLE, sensorVoltage, value);
      
    case LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC:
      if (!GetActualVoltage(&SENSORS[sensor - LOG_SENSOR_First], &sensorVoltage)) {
        return false;
      }
      return LinearInterpolation(&GRUNDFOS_VFS_DEGC_TABLE, sensorVoltage, value);

    case LOG_SENSOR_TYPE_Last:
      break;
  }
  
  return false;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

// Convert voltage from the ADC reading to the source voltage of voltage divider.
static bool GetActualVoltage(const Sensor_t * sensor, float * const voltage)
{
  const float adcVoltage = AdcMuxGetVolt(sensor->channel);
  if (adcVoltage == 0.0f || sensor->r2 == 0.0f) {
    *voltage = 0.0f;
    return true;
  }
  
  *voltage = adcVoltage / (sensor->r2 / (sensor->r1 + sensor->r2));
  return true;
}

static bool GetSensorResistance(const Sensor_t * sensor, float * const resistance)
{
  const float adcVoltage = AdcMuxGetVolt(sensor->channel);
  if (adcVoltage == 0.0f) {
    return false;
  }
  
  *resistance = (adcVoltage * sensor->r1) / (NTC_SRC_VOLT - adcVoltage);
  return true;
}

static bool ComponentIndexIsValid(const FlcHwComponent_t sensor)
{
  return ((sensor >= LOG_SENSOR_First) && (sensor < LOG_SENSOR_Last));
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

#ifdef CLI_ENABLE

static const char * const TYPE_NAMES[] = {
  "Log sensor voltage      ",       // LOG_SENSOR_UNUSED_VOLT
  "Log sensor NTC ohm      ",       // LOG_SENSOR_NTC_OHM
  "Grundfos VFS 1-18 l/min flow",   // LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_18_LPM
  "Grundfos VFS 1-20 l/min flow",   // LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM
  "Grundfos VFS temperature",       // LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC
};
static_assert(SIZEOF_ARRAY(TYPE_NAMES) == LOG_SENSOR_TYPE_Last, "Wrong table row size!");
  
static const char * const TYPE_UNITS[] = {
  "volt",     // LOG_SENSOR_UNUSED_VOLT
  "ohm",      // LOG_SENSOR_NTC_OHM
  "l/min",    // LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_18_LPM
  "l/min",    // LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM
  "degC",     // LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC
};
static_assert(SIZEOF_ARRAY(TYPE_UNITS) == LOG_SENSOR_TYPE_Last, "Wrong table row size!");

static void CliSensorOutput(const uint_fast8_t idx)
{
  const FlcHwComponent_t hwComp = idx + LOG_SENSOR_First;
  
  float value;
  if (!LogSensorGetValue(hwComp, &value)) {
    value = 0.0f;
  }
  
  CliPrintf("  Log sensor %2u:\t%s\t%.2f %s" CLI_NL,
            idx + 1, 
            TYPE_NAMES[configTable[hwComp].subtype],
            (double)value,
            TYPE_UNITS[configTable[hwComp].subtype]);  
}

int_fast16_t CliLogSensorShowAll(const CliParam_t param1, const CliParam_t param2, const CliParam_t param3)
{
  CliWrite("Log Sensor State..." CLI_NL);
  for (uint_fast8_t idx = 0; idx < HW_COUNT; idx++) {
    CliSensorOutput(idx);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowValue(CliParam_t no, CliParam_t param2, CliParam_t param3)
{
  if ((no == 0) || (no > HW_COUNT)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }

  CliSensorOutput(no - 1);
  return CLI_RESULT_OK;
}

CLI_START_TABLE(log_sensors)
  CLI_ENTRY0( "show",   "Show all log sensors", CliLogSensorShowAll)
  CLI_ENTRY1( "value",  "Show log sensor value [no:1..max]", CliShowValue, CLI_PARAM_INT)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(log_sensors)

#endif

/******************************************************************************/
