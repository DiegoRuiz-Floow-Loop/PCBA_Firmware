/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef LED_CFG_H
#define LED_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*******************************************************************************/

#include "plf/plf.h" 
#include "hal/mcupin/mcupin.h"

/*******************************************************************************/

  
// 0,5 Hz
#define LED_FLASH_SLOW_ON     (800)
#define LED_FLASH_SLOW_OFF    (1200)
// 1 Hz
#define LED_FLASH_ON          (400)
#define LED_FLASH_OFF         (600)
// 2 Hz
#define LED_FLASH_FAST_ON     (200)
#define LED_FLASH_FAST_OFF    (300)
 
typedef enum { 
  LED_First,
  LED_DEBUG_GREEN = LED_First,
// #if !defined(BOOTLOADER) && !defined(EVK)
  LED_PWM_First,
  LED_BTN_1 = LED_PWM_First,
  LED_BTN_2,
  LED_SHOWER,
// #endif  
#if !defined(BOOTLOADER)  
  LED_RGB_R,
  LED_RGB_G,
  LED_RGB_B,
#endif
  LED_Last
}                       Led_t;

#define LED_HEARTBEAT   LED_DEBUG_GREEN

// LED mcu pin table (digital inputs)
#if defined(EVK) && defined(BOOTLOADER)
#define LED_PIN_TABLE { MP_LED_GREEN }
#elif defined(EVK)
#define LED_PIN_TABLE { MP_LED_GREEN, MP_LED_RGB_R, MP_LED_RGB_G, MP_LED_RGB_B }
#elif !defined(EVK)
#define LED_PIN_TABLE { MP_DBG_LED_HB }
#endif

// PWM table
#if !defined(EVK) && !defined(BOOTLOADER)
#define LED_PWM_SUPPORTED
#define LED_PWM_TABLE {   \
  PWM_BTN_1_LIGHT,        \
  PWM_BTN_2_LIGHT,        \
  PWM_SHOWER_LIGHT,       \
  PWM_RGB_STRIP_RED,      \
  PWM_RGB_STRIP_GREEN,    \
  PWM_RGB_STRIP_BLUE      \
}
#endif

/*******************************************************************************/
  
#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /*  */
