/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef FLOW_KNOB_H
#define FLOW_KNOB_H

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

/*******************************************************************************
 * Functions
 ******************************************************************************/

void FlowKnobInit(void);

bool FlowKnobIsHeadOn(void);
uint16_t FlowKnobGetHeadPowerPermil(void);

bool FlowKnobIsHandOn(void);
uint16_t FlowKnobGetHandPowerPermil(void);

// eXternal required features - implement elsewhere
void XFlowKnobOutOfBoundsChanged(bool isOutOfBounds);

#ifdef CLI_ENABLE
int_fast16_t CliFlowKnobShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
