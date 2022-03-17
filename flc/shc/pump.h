/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef PUMP_H
#define PUMP_H

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

typedef enum {
  PUMP_STOP,
  PUMP_FORWARD,
  PUMP_REVERSE,
  PUMP_STATE_Last
} PumpState_t;

/*******************************************************************************
 * Functions
 ******************************************************************************/

// Config pointer is set to app component configuration.
bool PumpInit(const FlcComponentMapping_t * cfg);

// Pump power is set to zero every time a state is changed
void PumpSetState(FlcHwComponent_t pump, PumpState_t state);
void PumpSetPower(FlcHwComponent_t pump, uint16_t permil);

PumpState_t PumpGetState(FlcHwComponent_t pump);
uint16_t PumpGetPower(FlcHwComponent_t pump);
bool PumpGetCurrentFault(FlcHwComponent_t pump);
float PumpGetCurrentAmp(FlcHwComponent_t pump);

// Call from external GPIO interrupt functions on rising edges
void PumpXiCurrentFaultCb(FlcHwComponent_t pump);

// eXternal required features - implement elsewhere
void XPumpCurrentFault(FlcHwComponent_t pump);

#ifdef CLI_ENABLE
int_fast16_t CliPumpShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
