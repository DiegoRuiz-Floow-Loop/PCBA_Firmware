/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef VTIM_H
#define VTIM_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#if (PLF_OS == PLF_OS_RTOS)
#include <plf/rtos/rtos.h>
#endif

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

#if (PLF_OS == PLF_OS_RTOS)
typedef RtosTicks_t VTimTicks_t;
#else
typedef uint32_t VTimTicks_t;
#endif

typedef struct {
  VTimTicks_t start;
  VTimTicks_t interval;
} VTim_t;

/*******************************************************************************
 * Functions
 ******************************************************************************/

// Virtual timer functions based on ticks

// Setup start tick and interval tick
void VTimSetTick(VTim_t * vtim, VTimTicks_t ticks);
void VTimSetMsec(VTim_t * vtim, uint32_t msec);

// Reset by incrementing start tick with interval tick
void VTimReset(VTim_t * vtim);

// Restart by setting start tick to current tick
void VTimRestart(VTim_t * vtim);

// Get status
bool VTimIsExpired(const VTim_t * vtim);
uint32_t VTimGetMsecLeft(const VTim_t * vtim);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
