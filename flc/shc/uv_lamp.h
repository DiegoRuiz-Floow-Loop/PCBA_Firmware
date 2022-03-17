/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef UV_LAMP_H
#define UV_LAMP_H

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

// Config pointer is set to app component configuration.
bool UvLampInit(const FlcComponentMapping_t * cfg);

void UvLampSet(bool on);
bool UvLampIsOn(void);

#ifdef CLI_ENABLE
int_fast16_t CliUvLampShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
