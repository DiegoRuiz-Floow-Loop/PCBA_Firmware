/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>

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
  { HOT_TANK_FLOAT_SWITCH,        FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_1
  { MIXED_TANK_LOOP_FLOAT_SWITCH, FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_2
  { MIXED_TANK_COLD_FLOAT_SWITCH, FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_3
  { UNUSED_FLOAT_SWITCH,          FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_4
  { UNUSED_FLOAT_SWITCH,          FLOAT_SWITCH,    NO_SUBTYPE                            }, // FLOAT_SWITCH_5
                                                                                       
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                         
  { HOT_TANK_VALVE,               VALVE,           VALVE_NORMALLY_CLOSED                 }, // VALVE_1
  { MIXED_TANK_VALVE,             VALVE,           VALVE_NORMALLY_CLOSED                 }, // VALVE_2
  { HAND_HEAD_DIVERTER_VALVE,     VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_3
  { DRAIN_HEAD_VALVE,             VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_4
  { DRAIN_DELIVERY_VALVE,         VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_5
  { DRAIN_LOOP_VALVE,             VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_6
  { UNUSED_VALVE,                 VALVE,           VALVE_NORMALLY_CLOSED                 }, // VALVE_7
  { UNUSED_VALVE,                 VALVE,           VALVE_NORMALLY_CLOSED                 }, // VALVE_8
  { UNUSED_VALVE,                 VALVE,           VALVE_NORMALLY_CLOSED                 }, // VALVE_9
  { EXTERNAL_RELAY_VALVE,         VALVE,           VALVE_NORMALLY_OPEN                   }, // VALVE_10
                                                                                         
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                         
  { UV_LAMP,                      UV_LAMP_BALLAST, NO_SUBTYPE                            }, // UV_LAMP_1
       
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                         
  { UNUSED_PUMP,                  PUMP,            NO_SUBTYPE                            }, // PUMP_1
  { LOOP_PUMP,                    PUMP,            NO_SUBTYPE                            }, // PUMP_2
  { DELIVERY_PUMP,                PUMP,            NO_SUBTYPE                            }, // PUMP_3
  
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE
  { HOT_TANK_TEMP_SENSOR,         LOG_SENSOR,      LOG_SENSOR_NTC_OHM                    }, // LOG_SENSOR_1
  { MIXED_TANK_TEMP_SENSOR,       LOG_SENSOR,      LOG_SENSOR_NTC_OHM                    }, // LOG_SENSOR_2
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_3
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_4
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_5
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_6
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_7
  { DELIVERY_TEMP_SENSOR,         LOG_SENSOR,      LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC     }, // LOG_SENSOR_8
  { DELIVERY_FLOW_SENSOR,         LOG_SENSOR,      LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM }, // LOG_SENSOR_9
  { LOOP_TEMP_SENSOR,             LOG_SENSOR,      LOG_SENSOR_TEMP_GRUNDFOS_VFS_DEGC     }, // LOG_SENSOR_10
  { LOOP_FLOW_SENSOR,             LOG_SENSOR,      LOG_SENSOR_FLOW_GRUNDFOS_VFS_1_20_LPM }, // LOG_SENSOR_11
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_12
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_13
  { UNUSED_LOG_SENSOR,            LOG_SENSOR,      LOG_SENSOR_UNUSED_VOLT                }, // LOG_SENSOR_14
                                                                             
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE             
  { FLOW_KNOB_POSITION_SENSOR,    POS_SENSOR,      NO_SUBTYPE                            }, // POS_SENSOR_1 (SENSOR 15)
  
  // APP COMPONENT                COMP. TYPE       COMP. SUBTYPE                     
  { UNUSED_WATER_SENSOR,          WATER_SENSOR,    NO_SUBTYPE                            }, // WATER_SENSOR_1 (SENSOR 16)
  { FLOOR_WATER_LEVEL_SENSOR,     WATER_SENSOR,    NO_SUBTYPE                            }, // WATER_SENSOR_2 (SENSOR 17)
  { UNUSED_WATER_SENSOR,          WATER_SENSOR,    NO_SUBTYPE                            }, // WATER_SENSOR_3 (SENSOR 18)

  { NO_USER_BUTTON1,              BUTTON,          NO_SUBTYPE                            },
  { NO_USER_BUTTON2,              BUTTON,          NO_SUBTYPE                            },    
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
