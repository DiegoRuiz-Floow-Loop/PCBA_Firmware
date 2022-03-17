/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef FLOAT_SWITCH_H
#define FLOAT_SWITCH_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <app/flc_def.h>

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Functions
 ******************************************************************************/

bool FloatSwitchInit(const FlcComponentMapping_t * cfg);

bool FloatSwitchRead(FlcHwComponent_t floatSwitch);

// eXternal required features - implement elsewhere
void XFloatSwitchChanged(const FlcHwComponent_t comp, const bool level);

#ifdef CLI_ENABLE
int_fast16_t CliFloatSwitchShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
