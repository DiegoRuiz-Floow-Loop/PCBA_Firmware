/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef COLD_SUPPLY_H
#define COLD_SUPPLY_H

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

void ColdSupplyInit(void);

void ColdSupplySetShowerStateChange(ShcState_t state);
void ColdSupplySetTankLoopLevelChanged(bool level);
void ColdSupplySetTankColdLevelChanged(bool level);

#ifdef CLI_ENABLE
int_fast16_t CliColdSupplyShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
