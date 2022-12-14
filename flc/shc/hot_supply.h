/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef HOT_SUPPLY_H
#define HOT_SUPPLY_H

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

void HotSupplyInit(void);

void HotSupplySetShowerStateChange(ShcState_t state);
void HotSupplySetTankLevelChanged(bool level);

#ifdef CLI_ENABLE
int_fast16_t CliHotSupplyShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
