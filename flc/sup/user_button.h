/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef USER_BUTTON_H
#define USER_BUTTON_H

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

bool UserButtonInit(const FlcComponentMapping_t * cfg);

bool UserButtonRead(FlcHwComponent_t button);

// eXternal required features - implement elsewhere
void XUserButtonChanged(const FlcHwComponent_t comp, const bool level);

#ifdef CLI_ENABLE
int_fast16_t CliUserButtonsShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
