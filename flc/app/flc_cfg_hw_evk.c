/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "flc_cfg.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const FlcComponentMapping_t CFG_TABLE[] = {
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE
  { MIXED_TANK_LOOP_FLOAT_SWITCH, FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_1
                                                                                     
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                     
  { HAND_HEAD_DIVERTER_VALVE,     VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_1
                                                                                     
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                     
  { UV_LAMP,                      UV_LAMP_BALLAST, NO_SUBTYPE                            }, // UV_LAMP_1
                                                                                     
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                     
  { DELIVERY_PUMP,                PUMP,            NO_SUBTYPE                            }, // PUMP_1
                                                   
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE
  { DELIVERY_FLOW_SENSOR,         LOG_SENSOR,      LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_18_LPM }, // LOG_SENSOR_1
  { DELIVERY_TEMP_SENSOR,         LOG_SENSOR,      LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC     }, // LOG_SENSOR_2
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_3
                                                   
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE
  { FLOW_KNOB_POSITION_SENSOR,    POS_SENSOR,      NO_SUBTYPE                            }, // POS_SENSOR_1
                                                                                     
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                     
  { FLOOR_WATER_LEVEL_SENSOR,     WATER_SENSOR,    NO_SUBTYPE                            }, // WATER_SENSOR_1
};
static_assert(SIZEOF_ARRAY(CFG_TABLE) == HW_COMPONENT_Last, "Wrong table size!");

/*******************************************************************************
 * Public data variables
 ******************************************************************************/

const FlcComponentMapping_t * flcCfg = CFG_TABLE;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

/*******************************************************************************
 * Public functions
 ******************************************************************************/

/*******************************************************************************
 * Local functions
 ******************************************************************************/

/******************************************************************************/
