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

static const FlcAppComponent_t UNUSED_APP_COMPONENTS[] = {
  UNUSED_FLOAT_SWITCH,
  UNUSED_VALVE,
  UNUSED_UV_LAMP,
  UNUSED_PUMP,
  UNUSED_LOG_SENSOR,
  UNUSED_POS_SENSOR,
  UNUSED_WATER_SENSOR,
};
static_assert(SIZEOF_ARRAY(UNUSED_APP_COMPONENTS) == COMPONENT_TYPE_Last-1, "UNUSED_APP_COMPONENTS unexpected size");

/*******************************************************************************
 * Public data variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static FlcHwComponent_t hwComponents[APP_COMPONENT_Last];

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static FlcHwComponent_t FlcCfgFindHwComponent(const FlcAppComponent_t component);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void FlcCfgInit(void)
{
  TRACE(TRC_TA_APP, TRC_TL_COMPONENT, "FlcCfgInit()");
  
  // Generate bindings for app component to hw component
  for (FlcAppComponent_t idx = 0; idx < APP_COMPONENT_Last; idx++) {
    hwComponents[idx] = FlcCfgFindHwComponent(idx);
  }
}

FlcHwComponent_t FlcCfgAppToHwComponent(const FlcAppComponent_t component)
{
  if (component >= APP_COMPONENT_Last) {
    return HW_COMPONENT_Last;
  }
    
  return hwComponents[component];
}

static FlcHwComponent_t FlcCfgFindHwComponent(const FlcAppComponent_t component)
{
  if (FlcCfgAppComponentIsUnused(component)) {
    return HW_COMPONENT_Last;
  }
  
  for (FlcHwComponent_t idx = 0; idx < HW_COMPONENT_Last; idx++) {
    if (component == flcCfg[idx].component) {
      return idx;
    }
  }
  return HW_COMPONENT_Last;
}

bool FlcCfgAppComponentIsUnused(const FlcAppComponent_t component)
{
  for (uint_fast8_t cmpIdx = 0; cmpIdx < COMPONENT_TYPE_Last; cmpIdx++) {
    if (component == UNUSED_APP_COMPONENTS[cmpIdx]) {
      return true;
    }
  }
  
  return false;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

/******************************************************************************/
