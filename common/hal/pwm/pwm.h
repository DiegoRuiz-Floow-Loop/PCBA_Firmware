/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_PWM_H
#define HAL_PWM_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "plf/plf.h"


/******************************************************************************/

#include "hal/cfg/pwm_cfg.h"

/******************************************************************************/

extern void HalPwmStart(HalPwmCh_t ch, uint16_t pulse); 

extern void HalPwmStartPct(HalPwmCh_t ch, uint8_t pct);

extern void HalPwmStartPermil(HalPwmCh_t ch, uint16_t permil);

// note, all PWM on the ch-TIM will be adjusted
extern void HalPwmBaseFreqSet(HalPwmCh_t ch, uint32_t frequency);
  
extern void HalPwmStop(HalPwmCh_t ch);

extern void HalPwmInit(void);
 
#ifdef CLI_ENABLE
  extern int_fast16_t TstHalPwmShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* HAL_PWM_H */
