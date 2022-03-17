/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "hal/hal.h"
#include "plf/plf.h"
#include "hal/wdt/wdt.h"

/****************************************************************************/

extern IWDG_HandleTypeDef hiwdg;

#ifndef BOOTLOADER
#ifdef HAL_WDT_ENABLE

static void EnableDebugging(void)
{
	// This function makes it possible to debug (i.e. break the CPU) with watchdog enabled
	
#ifdef __HAL_RCC_DBGMCU_CLK_ENABLE
// It seems that on M7, the DBGMCU unit is always enabled, and no clock enable macro exists
// On M0, the DBGMCU unit must have its clock enabled before the FREEZE_IWDG setting is set.
		__HAL_RCC_DBGMCU_CLK_ENABLE();
#endif
	
		// Pause the watchdog during debug breaks
		__HAL_DBGMCU_FREEZE_IWDG();		
}
#endif

#endif

extern void MX_IWDG_Init(void);

void HalWdtActivate(void)
{
#ifdef HAL_WDT_ENABLE
  MX_IWDG_Init();
  HAL_IWDG_Refresh(&hiwdg);
#endif
}


uint32_t HalWdtReActivate(uint32_t prescaler)
{

#ifdef HAL_WDT_ENABLE
  uint32_t  op = hiwdg.Init.Prescaler;
  hiwdg.Init.Prescaler = prescaler;
  if (HAL_IWDG_Init(&hiwdg) != HAL_OK)  {
    //Error_Handler();
  }
  HalWdtFeed();
  return op;
#else
  return 0;
#endif
}


void HalWdtFeed(void)
{
#ifdef HAL_WDT_ENABLE
#ifndef BOOTLOADER
	static bool wdtInit = false;
  if (!wdtInit) {
    HalWdtActivate();
    EnableDebugging();
    wdtInit = true;
    HalWdtActivate();
  }
  HAL_IWDG_Refresh(&hiwdg);
#endif
#endif
}



/****************************************************************************/
