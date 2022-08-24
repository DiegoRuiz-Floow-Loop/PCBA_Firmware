/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef SHC_H
#define SHC_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  SHC_IDLE,
  SHC_PRE_BACKWASH,    // Backwash = return flush
  SHC_SHOWER,
  SHC_SHOWER_LOOP,
  SHC_FLOW_STOP,
  SHC_EMPTY_AIRGAP,
  SHC_POST_BACKWASH,
  SHC_EMPTY_FINAL,
  SHC_EMERGENCY_STATE,
  SHC_STATE_Last
} ShcState_t;

/*******************************************************************************
 * Functions
 ******************************************************************************/

bool SHC_Init(void);

void ShcSetEmergencyStop(void);

ShcState_t ShcGetState(void);
const char * ShcGetStateName(ShcState_t state);

// eXternal required features - implement elsewhere
void XShcStateChanged(ShcState_t state);
void XShcShowerIsDone(void);
void XShcLowPowerChanged(bool isLowPower);
void SetShowerLoopButton(bool level);
void ShcSetEmergencyRebootEvent(void);
#ifdef CLI_ENABLE
int_fast16_t CliShcShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
