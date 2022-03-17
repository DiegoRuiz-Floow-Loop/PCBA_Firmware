/*******************************************************************************
 * COPYRIGHT (C) 2016 - All Rights Reserved
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_PWM_CFG_H
#define HAL_PWM_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "main.h"
#include "hal/hal.h"
#include "hal/pwm/pwm.h"


/******************************************************************************/

#define PWM_CLK_FREQ    (80UL*1000UL*1000UL)   // 80 MHz

#if defined(EVK)
#define PWM_PUMP1             HAL_PWM_2_1      // PWM_1
#define PWM_VALVE1            HAL_PWM_2_2      // PWM_2
#else
#define PWM_PUMP1             HAL_PWM_1_1
#define PWM_PUMP2             HAL_PWM_1_2
#define PWM_PUMP3             HAL_PWM_1_3
#define PWM_SHOWER_LIGHT      HAL_PWM_2_1
#define PWM_RGB_STRIP_RED     HAL_PWM_2_2
#define PWM_RGB_STRIP_GREEN   HAL_PWM_2_3
#define PWM_RGB_STRIP_BLUE    HAL_PWM_2_4
#define PWM_VALVE1            HAL_PWM_3_1
#define PWM_VALVE2            HAL_PWM_3_2
#define PWM_VALVE3            HAL_PWM_3_3
#define PWM_VALVE4            HAL_PWM_3_4
#define PWM_VALVE5            HAL_PWM_4_1
#define PWM_VALVE6            HAL_PWM_4_2
#define PWM_VALVE7            HAL_PWM_4_3
#define PWM_VALVE8            HAL_PWM_4_4
#define PWM_VALVE9            HAL_PWM_5_1
#define PWM_VALVE10           HAL_PWM_5_2
#define PWM_BTN_1_LIGHT       HAL_PWM_8_1
#define PWM_BTN_2_LIGHT       HAL_PWM_8_2
#endif

// Define existing PWM channels and channel data...
typedef enum {
#if defined(EVK)  
	HAL_PWM_2_1,  
	HAL_PWM_2_2, 
#else
  HAL_PWM_1_1,
  HAL_PWM_1_2,
  HAL_PWM_1_3,
  HAL_PWM_2_1,
  HAL_PWM_2_2,
  HAL_PWM_2_3,
  HAL_PWM_2_4,
  HAL_PWM_3_1,
  HAL_PWM_3_2,
  HAL_PWM_3_3,
  HAL_PWM_3_4,
  HAL_PWM_4_1,
  HAL_PWM_4_2,
  HAL_PWM_4_3,
  HAL_PWM_4_4,
  HAL_PWM_5_1,
  HAL_PWM_5_2,
  HAL_PWM_8_1,
  HAL_PWM_8_2,    
#endif  
  HAL_PWM_Last
}               HalPwmCh_t;

#if defined(PWM_MAKE_TABLE)

typedef struct {
  TIM_TypeDef *       htim;
  uint32_t            channel;
  char *              name;
} PwmCfg_t;

const PwmCfg_t pwmArr[HAL_PWM_Last] = {
#if defined(EVK)
  {TIM2, LL_TIM_CHANNEL_CH1, "2.1"},
  {TIM2, LL_TIM_CHANNEL_CH2, "2.2"},
#else
  {TIM1, LL_TIM_CHANNEL_CH1, "1.1"},
  {TIM1, LL_TIM_CHANNEL_CH2, "1.2"},
  {TIM1, LL_TIM_CHANNEL_CH3, "1.3"},
  {TIM2, LL_TIM_CHANNEL_CH1, "2.1"},
  {TIM2, LL_TIM_CHANNEL_CH2, "2.2"},
  {TIM2, LL_TIM_CHANNEL_CH3, "2.3"},
  {TIM2, LL_TIM_CHANNEL_CH4, "2.4"},
  {TIM3, LL_TIM_CHANNEL_CH1, "3.1"},
  {TIM3, LL_TIM_CHANNEL_CH2, "3.2"},
  {TIM3, LL_TIM_CHANNEL_CH3, "3.3"},
  {TIM3, LL_TIM_CHANNEL_CH4, "3.4"},  
  {TIM4, LL_TIM_CHANNEL_CH1, "4.1"},
  {TIM4, LL_TIM_CHANNEL_CH2, "4.2"},
  {TIM4, LL_TIM_CHANNEL_CH3, "4.3"},
  {TIM4, LL_TIM_CHANNEL_CH4, "4.4"},
  {TIM5, LL_TIM_CHANNEL_CH1, "5.1"},
  {TIM5, LL_TIM_CHANNEL_CH2, "5.2"},
  {TIM8, LL_TIM_CHANNEL_CH1, "8.1"},
  {TIM8, LL_TIM_CHANNEL_CH2, "8.2"},    
#endif
};

#endif


/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* HAL_PWM_CFG_H */
