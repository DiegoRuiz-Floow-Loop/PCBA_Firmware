/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdio.h>
#include <string.h>

#include "hal/dio/dio.h"

#include "plf/led/led.h"
#include "plf/evos/evos.h"
#include "plf/trc/trc.h"

#if defined(LED_PWM_SUPPORTED)
#include "hal/pwm/pwm.h"
#endif

/*******************************************************************************/
typedef char LedName_t[4];

typedef struct {
  EvosEventHandle_t     event;
  LedName_t             name;
  bool                  flashIsOn;      /* current level */
  uint16_t              flashTimeOn;    /* time for ON periode */
  uint16_t              flashTimeOff;   /* time for OFF periode */
  bool                  onLevel;
}                       LedState_t;

static LedState_t       leds[LED_Last];
LedRunState_t           ledRunState;

static const McuPin_t   ledMcuPin[] = LED_PIN_TABLE;
#if defined(LED_PWM_SUPPORTED)
static const HalPwmCh_t ledPwmCh[] = LED_PWM_TABLE;
#endif

/*******************************************************************************/

static void LedHwSet(const Led_t led, const bool on)
{
#if !defined(LED_PWM_SUPPORTED)
  HAL_DIO_PIN_WRITE(ledMcuPin[led], (on ? GPIO_PIN_SET : GPIO_PIN_RESET));
#else
  if (led >= LED_PWM_First) {
    HalPwmStartPct(ledPwmCh[led - LED_PWM_First], (on ? 100 : 0));
  } else {
    HAL_DIO_PIN_WRITE(ledMcuPin[led], (on ? GPIO_PIN_SET : GPIO_PIN_RESET));
  }
#endif  
}

/*******************************************************************************/

bool LedStateGet(const Led_t led)
{
  if (led >= LED_Last) return false;
  
  return ledRunState[led] != LR_OFF;
}

void LedStateSet(const Led_t led, const bool state)
{
  if (led >= LED_Last) return;
  
  const LedState_t * const pLed = &leds[led];
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "LedStateSet(%d, %d)", led, state);
  ledRunState[led]      = state ? LR_ON : LR_OFF;
  EvosEventClear(pLed->event);
  LedHwSet(led, BOOL_EQUAL(state, leds[led].onLevel));
}

/*******************************************************************************/

static void LedEventFunction(const EvosEventParam_t led)
{
  LedState_t * const pLed = &leds[(int)led];
  uint_fast16_t time;
  pLed->flashIsOn = !pLed->flashIsOn;
  if (pLed->flashIsOn) {
    time = pLed->flashTimeOn;
    LedHwSet(led, BOOL_EQUAL(1, leds[led].onLevel));
    EvosEventSetDelta(pLed->event, time, led);
  } else {
    time = pLed->flashTimeOff;
    LedHwSet(led, BOOL_EQUAL(0, leds[led].onLevel));
    if (pLed->flashTimeOff != 0)
      EvosEventSetDelta(pLed->event, time, led);
  }
}

/*******************************************************************************/

void LedFlashOnce(const Led_t led, const uint_fast16_t timeOn)
{
  if (led >= LED_Last) return;  
  
  LedState_t * const pLed = &leds[led];
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "LedFlashOnce(%d,%d)", led, timeOn);
  ledRunState[led]      = LR_FLASH;
  pLed->flashIsOn       = false;
  pLed->flashTimeOn     = timeOn;
  pLed->flashTimeOff    = 0;
  EvosEventSetNow(pLed->event, (EvosEventParam_t)led);  
}


void LedFlash(const Led_t led, const uint_fast16_t timeOn, const uint_fast16_t timeOff)
{
  if (led >= LED_Last) return;  
  
  LedState_t * const pLed = &leds[led];
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "LedFlash(%d,%d,%d)", led, timeOn, timeOff);
  ledRunState[led]      = LR_FLASH;
  pLed->flashIsOn       = false;
  pLed->flashTimeOn     = timeOn;
  pLed->flashTimeOff    = timeOff;
  EvosEventSetNow(pLed->event, (EvosEventParam_t)led);  
}

/*******************************************************************************/

void LedOn(const Led_t led)
{
  if (led >= LED_Last) return;
  
  const LedState_t * const pLed = &leds[led];
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "LedOn(%d)", led);
  ledRunState[led]      = LR_ON;
  EvosEventClear(pLed->event);
  LedHwSet(led, BOOL_EQUAL(1, leds[led].onLevel));
}

/*******************************************************************************/

void LedOff(const Led_t led)
{
  if (led >= LED_Last) return;
  
  const LedState_t * const pLed = &leds[led];
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "LedOff(%d)", led);
  ledRunState[led]      = LR_OFF;
  EvosEventClear(pLed->event);
  LedHwSet(led, BOOL_EQUAL(0, leds[led].onLevel));
}

/*******************************************************************************/

#if !defined(BOOTLOADER)

void LedRGB(const LedRgb_t ledRGB)
{
  switch ((int)ledRGB) {
    case LRGB_BLACK:    LedOff(LED_RGB_R);  LedOff(LED_RGB_G);  LedOff(LED_RGB_B); break;
    case LRGB_BLUE:     LedOff(LED_RGB_R);  LedOff(LED_RGB_G);  LedOn(LED_RGB_B);  break;
    case LRGB_GREEN:    LedOff(LED_RGB_R);  LedOn(LED_RGB_G);   LedOff(LED_RGB_B); break;
    case LRGB_CYAN:     LedOff(LED_RGB_R);  LedOn(LED_RGB_G);   LedOn(LED_RGB_B);  break;
    case LRGB_RED:      LedOn(LED_RGB_R);   LedOff(LED_RGB_G);  LedOff(LED_RGB_B); break;
    case LRGB_MAGENTA:  LedOn(LED_RGB_R);   LedOff(LED_RGB_G);  LedOn(LED_RGB_B);  break;
    case LRGB_YELLOW:   LedOn(LED_RGB_R);   LedOn(LED_RGB_G);   LedOff(LED_RGB_B); break;
    case LRGB_WHITE:    LedOn(LED_RGB_R);   LedOn(LED_RGB_G);   LedOn(LED_RGB_B);  break;
    default: break;
  }
}

#endif

/*******************************************************************************/

void LedOnlevelSet(const Led_t led, const bool onLevel)
{
  if (led >= LED_Last) return;
  
  leds[led].onLevel = onLevel;
}

void LedInit(void)
{
#if !defined(LED_PWM_SUPPORTED)  
  if (SIZEOF_ARRAY(ledMcuPin) != LED_Last) {
    for (;;);
  }
#else
  if (SIZEOF_ARRAY(ledMcuPin) != LED_PWM_First) {
    for (;;);
  }
  if (SIZEOF_ARRAY(ledPwmCh) != (LED_Last - LED_PWM_First)) {
    for (;;);
  }  
#endif  

  //memset(&leds, 0, sizeof(leds));
  memset(ledRunState, 0, sizeof(ledRunState));  // undefined and zero terminated

  for (Led_t ledIdx = LED_First; ledIdx < LED_Last; ledIdx++) {
#if !defined(BOOTLOADER)
    sprintf(leds[ledIdx].name, "L%u", ledIdx);
#else
    strcpy(leds[ledIdx].name, "led");
#endif    
    leds[ledIdx].event = EvosEventRegister(LedEventFunction, leds[ledIdx].name);
    LedOff(ledIdx);
  }
}
