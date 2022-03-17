/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef VALVE_H
#define VALVE_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>

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

// Config pointer is set to app component configuration.
bool ValveInit(const FlcComponentMapping_t * cfg);

void ValveOpen(FlcHwComponent_t valve);
void ValveClose(FlcHwComponent_t valve);

FlcValveType_t ValveGetType(FlcHwComponent_t valve);
uint16_t ValveGetDutyCycle(FlcHwComponent_t valve);

#ifdef CLI_ENABLE
int_fast16_t CliValveShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
