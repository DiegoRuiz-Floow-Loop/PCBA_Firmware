/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_DIO_H
#define HAL_DIO_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "plf/plf.h"

#if (PLF_OS != PLF_OS_WINDOWS)

/******************************************************************************/


#if defined(STM32F091xC)	|| defined(STM32F042x6) 
  #include "stm32f0xx_hal.h"
  #include "stm32f0xx_hal_gpio.h"
#elif defined(STM32F767xx) || defined(STM32F765xx) || defined(STM32F769xx)
  #include "stm32f7xx_hal.h"
  #include "stm32f7xx_hal_gpio.h"

#elif defined(STM32G070xx)
  #include "stm32g0xx_hal.h"
  #include "stm32g0xx_hal_gpio.h"

#elif defined(STM32L010xB)
  #include "stm32l0xx_hal.h"
  #include "stm32l0xx_hal_gpio.h"
#elif defined(STM32L431xx) || defined(STM32L433xx) || defined(STM32L471xx) || defined(STM32L475xx)
  #include "stm32l4xx_hal.h"
  #include "stm32l4xx_hal_gpio.h"

#elif defined(STM32WL55xx)
  #include "stm32wlxx_hal.h"
  #include "stm32wlxx_hal_gpio.h"

#else

#endif

#include "hal/mcupin/mcupin.h"
#include "plf/trc/trc.h"

/******************************************************************************/

typedef enum {
  GPIO_A,
  GPIO_B,
  GPIO_C,
#if defined(GPIOD)	
  GPIO_D,
#endif	
#if defined(GPIOE)	
  GPIO_E,
#endif	
#if defined(GPIOF)	
  GPIO_F,
#endif	
#if defined(GPIOG)	
  GPIO_G,
#endif	
#if defined(GPIOH)	
  GPIO_H,
#endif	
#if defined(GPIOI)	
  GPIO_I,
#endif	
#if defined(GPIOJ)	
  GPIO_J,
#endif	
#if defined(GPIOK)	
  GPIO_K,
#endif	
  GPIO_Last 
}                       Gpio_t;

extern const GPIO_TypeDef * const gpio[GPIO_Last];

/******************************************************************************/


#define HAL_DIO_PIN_SET(mcuPin) 			  if (MCUPIN_NA != mcuPin) HAL_GPIO_WritePin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin), GPIO_PIN_SET)
#define HAL_DIO_PIN_CLR(mcuPin) 			  if (MCUPIN_NA != mcuPin) HAL_GPIO_WritePin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin), GPIO_PIN_RESET)
#define HAL_DIO_PIN_TOGGLE(mcuPin) 		  if (MCUPIN_NA != mcuPin) HAL_GPIO_TogglePin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin))

#define HAL_DIO_PIN_WRITE(mcuPin, val)  if (MCUPIN_NA != mcuPin) HAL_GPIO_WritePin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin), (GPIO_PinState)val)

#define HAL_DIO_PIN_GET(mcuPin) 		    (MCUPIN_NA != mcuPin ? HAL_GPIO_ReadPin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin)) : GPIO_PIN_RESET)

	
extern void HalDioInit(void);

#ifdef CLI_ENABLE

  extern int_fast16_t TstHalDioShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);

#endif

/******************************************************************************/


#endif

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* HAL_DIO_H */
