/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef MCUPIN_H
#define MCUPIN_H

#if (PLF_OS != PLF_OS_WINDOWS)

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if !defined(USE_FULL_LL_DRIVER)
  
  #if defined(STM32L073xx)  || defined(STM32L071xx)
    #include "stm32l0xx_hal.h"
    #include "stm32l0xx_hal_gpio.h"
  #elif defined(STM32L476xx) || defined(STM32L471xx) || defined(STM32L4A6xx)
    #include "stm32l4xx_hal.h"
    #include "stm32l4xx_hal_gpio.h"
  #elif defined(STM32F030xC) || defined(STM32F030x8)
    #include "stm32f0xx_hal.h"
    #include "stm32f0xx_hal_gpio.h"
  #endif
  
#endif
  
#include "plf/plf.h"

#include "main.h"


/******************************************************************************/
  
// pin_t:   uint16_t
// port_t:  GPIO_TypeDef * 
  
typedef uint32_t                    McuPin_t;  

#define MCUPIN(port, pin)      		  (McuPin_t)((((uint32_t)(0xFFFF & port)) << 16) | (uint32_t)(0xFFFF & pin)) 

#if defined(STM32L073xx)  || defined(STM32L071xx)
  #define MCUPIN_PORT_GET(mcuPin)	  (GPIO_TypeDef*)(IOPPERIPH_BASE + (mcuPin >> 16))
  
#elif defined(STM32L476xx) || defined(STM32L471xx) || defined(STM32L4A6xx)
  #define MCUPIN_PORT_GET(mcuPin)	  (GPIO_TypeDef*)(AHB2PERIPH_BASE + (mcuPin >> 16))
  
#elif defined(STM32F030xC) || defined(STM32F030x8)
  #define MCUPIN_PORT_GET(mcuPin)	  (GPIO_TypeDef*)(AHB2PERIPH_BASE + (mcuPin >> 16))
  
#elif defined(STM32G031xx)
  #define MCUPIN_PORT_GET(mcuPin)	  (GPIO_TypeDef*)(IOPORT_BASE + (mcuPin >> 16))
  
#endif

#define MCUPIN_PIN_GET(mcuPin)   	  ((uint16_t)mcuPin)

#define MCUPIN_NA                   (McuPin_t)(0xFFFFFFFFul)

/******************************************************************************/


//#include "hal/mcupin/mcupin.inc"

#define MP_LED_GREEN              MCUPIN(LED2_GPIO_PORT, LED2_Pin)

/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif

#endif /* MCUPIN_H */
