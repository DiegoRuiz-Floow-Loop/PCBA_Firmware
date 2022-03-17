/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

// Virtual timer functions based on ticks

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>

#if (PLF_OS == PLF_OS_RTOS)
#include <plf/rtos/rtos.h>
#else
#include <hal/hal.h>
#endif

#include "vtim.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static inline VTimTicks_t GetTickCount(void);
static inline VTimTicks_t GetTicksFromMsec(const uint32_t msec);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void VTimSetTick(VTim_t * const vtim, const VTimTicks_t ticks)
{
  vtim->interval = ticks;
  vtim->start = GetTickCount();
}

void VTimSetMsec(VTim_t * const vtim, const uint32_t msec)
{
  VTimSetTick(vtim, GetTicksFromMsec(msec));
}

void VTimReset(VTim_t * const vtim)
{
  vtim->start += vtim->interval;
}

void VTimRestart(VTim_t * const vtim)
{
  vtim->start = GetTickCount();
}

bool VTimIsExpired(const VTim_t * const vtim)
{
  return ((GetTickCount() - vtim->start) >= vtim->interval);
}

uint32_t VTimGetMsecLeft(const VTim_t * const vtim)
{
  if (VTimIsExpired(vtim)) return 0u;
  
  const VTimTicks_t ticksLeft = vtim->interval - (GetTickCount() - vtim->start);
#if (PLF_OS == PLF_OS_RTOS)
  return (ticksLeft * 1000 / RTOS_TICKS_PER_SEC);
#else  
  // Presumed that 1 msec is equal to 1 tick with STM32 HAL
	return ticksLeft;
#endif  
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static inline VTimTicks_t GetTickCount(void)
{
#if (PLF_OS == PLF_OS_RTOS)
  return RTOS_GET_TICK_COUNT();
#else
  // STM32 HAL
  return HAL_GetTick();
#endif  
}

static inline VTimTicks_t GetTicksFromMsec(const uint32_t msec)
{
#if (PLF_OS == PLF_OS_RTOS)
  return RTOS_TICKS_FROM_MSEC(msec);
#else
  // Presumed that 1 msec is equal to 1 tick with STM32 HAL
  return msec;
#endif  
}

/******************************************************************************/
