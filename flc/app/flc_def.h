/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef FLC_DEF_H
#define FLC_DEF_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  HOT_TANK_FLOAT_SWITCH,
  MIXED_TANK_LOOP_FLOAT_SWITCH,
  MIXED_TANK_COLD_FLOAT_SWITCH,
  UNUSED_FLOAT_SWITCH,
  
  HOT_TANK_VALVE,
  MIXED_TANK_VALVE,
  HAND_HEAD_DIVERTER_VALVE,
  DRAIN_HEAD_VALVE,
  DRAIN_DELIVERY_VALVE,
  DRAIN_LOOP_VALVE,
  EXTERNAL_RELAY_VALVE,
  UNUSED_VALVE,
  
  UV_LAMP,
  UNUSED_UV_LAMP,
  
  DELIVERY_PUMP,
  LOOP_PUMP,
  UNUSED_PUMP,
  
  DELIVERY_FLOW_SENSOR,
  DELIVERY_TEMP_SENSOR,
  LOOP_FLOW_SENSOR,
  LOOP_TEMP_SENSOR,
  MIXED_TANK_TEMP_SENSOR,
  HOT_TANK_TEMP_SENSOR,
  UNUSED_LOG_SENSOR,
  
  FLOW_KNOB_POSITION_SENSOR,
  UNUSED_POS_SENSOR,
  
  FLOOR_WATER_LEVEL_SENSOR,
  UNUSED_WATER_SENSOR,
  
  APP_COMPONENT_Last
} FlcAppComponent_t;

typedef enum {
  FLOAT_SWITCH,
  VALVE,
  UV_LAMP_BALLAST,
  PUMP,
  LOG_SENSOR,
  POS_SENSOR,
  WATER_SENSOR,
  COMPONENT_TYPE_Last
} FlcComponentType_t;

typedef enum {
  FLOAT_SWITCH_First,
  FLOAT_SWITCH_1 = FLOAT_SWITCH_First,
#if !defined(EVK)
  FLOAT_SWITCH_2,
  FLOAT_SWITCH_3,
  FLOAT_SWITCH_4,
  FLOAT_SWITCH_5,
#endif  
  FLOAT_SWITCH_Last,
  
  VALVE_First = FLOAT_SWITCH_Last,
    
  VALVE_1 = VALVE_First,
#if !defined(EVK)  
  VALVE_2,
  VALVE_3,
  VALVE_4,
  VALVE_5,
  VALVE_6,
  VALVE_7,
  VALVE_8,
  VALVE_9,
  VALVE_10,
#endif  
  VALVE_Last,
  
  UV_LAMP_First = VALVE_Last,
  
  UV_LAMP_1 = UV_LAMP_First,
  UV_LAMP_Last,
  
  PUMP_First = UV_LAMP_Last,
  
  PUMP_1 = PUMP_First,
#if !defined(EVK)  
  PUMP_2,
  PUMP_3,
#endif  
  PUMP_Last,
  
  LOG_SENSOR_First = PUMP_Last,
  
  LOG_SENSOR_1 = LOG_SENSOR_First,
  LOG_SENSOR_2,
  LOG_SENSOR_3,
#if !defined(EVK)  
  LOG_SENSOR_4,
  LOG_SENSOR_5,
  LOG_SENSOR_6,
  LOG_SENSOR_7,
  LOG_SENSOR_8,
  LOG_SENSOR_9,
  LOG_SENSOR_10,
  LOG_SENSOR_11,
  LOG_SENSOR_12,
  LOG_SENSOR_13,
  LOG_SENSOR_14,
#endif  
  LOG_SENSOR_Last,
  
  POS_SENSOR_First = LOG_SENSOR_Last,
  
  POS_SENSOR_1 = POS_SENSOR_First,
  POS_SENSOR_Last,
  
  WATER_SENSOR_First = POS_SENSOR_Last,
  
  WATER_SENSOR_1 = WATER_SENSOR_First,
#if !defined(EVK)  
  WATER_SENSOR_2,
  WATER_SENSOR_3,
#endif  
  WATER_SENSOR_Last,
  
  HW_COMPONENT_Last =  WATER_SENSOR_Last
} FlcHwComponent_t;

/******************************************************************************/

// Common units
// --------------------------------------------------
// | Voltage      | VOLT
// | Current      | AMP
// | Resistance   | OHM
// | Flow,        | LPM (liter per minute)
// | Temperature  | DEGC (degrees celsius)

typedef enum {
  VALVE_NORMALLY_OPEN,
  VALVE_NORMALLY_CLOSED,
  VALVE_TYPE_Last
} FlcValveType_t;

typedef enum {
  LOG_SENSOR_UNUSED_VOLT,                // Log sensor setup as voltage input
  LOG_SENSOR_NTC_OHM,                    // Log sensor setup as NTC input 
  LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_18_LPM, // Grundfos VFS (Vortex Flow Sensor) 1-18 l/min
  LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM, // Grundfos VFS (Vortex Flow Sensor) 1-20 l/min
  LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC,     // Grundfos VFS (Vortex Flow Sensor) temperature
  LOG_SENSOR_TYPE_Last
} FlcLogSensorType_t;

typedef struct {
  FlcAppComponent_t  component;
  FlcComponentType_t type;
  uint8_t            subtype;     // Use HW type enum (valve, log sensor, etc.)
} FlcComponentMapping_t;

/******************************************************************************/

typedef enum {
  SHC_TIMING_IDLE_STARTUP_DELAY,
  SHC_TIMING_IDLE_TO_LOW_POWER,
  SHC_TIMING_PRE_BACKWASH,
  SHC_TIMING_PRE_BACKWASH_REQUIRED,
  SHC_TIMING_FLOW_STOP,
  SHC_TIMING_EMPTY_AIRGAP,
  SHC_TIMING_POST_BACKWASH,
  SHC_TIMING_EMPTY_FINAL,
  SHC_TIMING_Last
} FlcShowerCtrlTiming_t;

typedef enum {
  HOT_SUPPLY_TIMING_DELAYED_CLOSE,
  HOT_SUPPLY_TIMING_DELAYED_OPEN,
  HOT_SUPPLY_TIMING_Last
} FlcHotSupplyCtrlTiming_t;

typedef enum {
  COLD_SUPPLY_TIMING_DELAYED_CLOSE,
  COLD_SUPPLY_TIMING_DELAYED_NORMAL_OPEN,
  COLD_SUPPLY_TIMING_DELAYED_IN_LOOP_OPEN,
  COLD_SUPPLY_TIMING_Last
} FlcColdSupplyCtrlTiming_t;

typedef enum {
  PRIME_PUMP_TIMING_DELAYED_POWER_START,
  PRIME_PUMP_TIMING_DELAYED_POWER_STOP,
  PRIME_PUMP_TIMING_Last
} FlcPrimePumpCtrlTiming_t;

/*******************************************************************************
 * Functions
 ******************************************************************************/

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
