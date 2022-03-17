/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef PRIME_PUMP_H
#define PRIME_PUMP_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>

#include <shc/shc.h>

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Functions
 ******************************************************************************/

void PrimePumpInit(void);

void PrimePumpSetShowerStateChange(ShcState_t state);
void PrimePumpSetTankLoopLevelChanged(bool level);
void PrimePumpSetTankColdLevelChanged(bool level);

#ifdef CLI_ENABLE
int_fast16_t CliPrimePumpShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
