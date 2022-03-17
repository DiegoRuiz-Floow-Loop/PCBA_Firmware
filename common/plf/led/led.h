/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

/** @addtogroup xxx_group
 * @{
 */

#ifndef LED_H
#define LED_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*******************************************************************************/

#include "plf/plf.h" 
#include "plf/cfg/led_cfg.h"
  
/*******************************************************************************/


typedef enum {
  LR_OFF    = '-',
  LR_ON     = 'X',
  LR_FLASH  = 'f',
}                       LedRun_t;

typedef LedRun_t        LedRunState_t[LED_Last+1];
extern LedRunState_t    ledRunState;

/*******************************************************************************/

extern bool LedStateGet(
  Led_t           led);

extern void LedStateSet(
  Led_t           led, 
  bool            state);


extern void LedFlash(
  Led_t           led,
  uint_fast16_t   timeOn, 
  uint_fast16_t   timeOff);

extern void LedFlashOnce(
  Led_t           led,
  uint_fast16_t   timeOn);

extern void LedOn(
  Led_t led);

extern void LedOff(
  Led_t led);

extern void LedToggle
  (Led_t led);

typedef enum {    //  R G B
  LRGB_BLACK,     //  0 0 0
  LRGB_BLUE,      //  0 0 1
  LRGB_GREEN,     //  0 1 0
  LRGB_CYAN,      //  0 1 1
  LRGB_RED,       //  1 0 0
  LRGB_MAGENTA,   //  1 0 1
  LRGB_YELLOW,    //  1 1 0
  LRGB_WHITE,     //  1 1 1
  LRGB_Last  
}                   LedRgb_t;

extern void LedRGB(LedRgb_t ledRGB);

// May be called after LedInit() !!
extern void LedOnlevelSet(Led_t led, bool onLevel);

extern void LedInit(void);


/*******************************************************************************/
  
#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /*  */
