/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef ADC_CFG_H
#define ADC_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************/
 
#define ADC_RESOLUTION  (4096)  // range: 0..4095

#define hadc          hadc1
  
typedef enum {
  HAL_ADC_1,
  HAL_ADC_2,
  HAL_ADC_3,
  HAL_ADC_TEMP,
  HAL_ADC_VREF,
 
  HAL_ADC_Last 		// to use in "for (.<last.)" and in "arrayes[last]"
}                       HalAdcCh_t;

#define ADC_CHANNEL_ARRAY {\
  ADC_CHANNEL_0, \
  ADC_CHANNEL_3, \
  ADC_CHANNEL_6, \
  ADC_CHANNEL_TEMPSENSOR, \
  ADC_CHANNEL_VREFINT, \
}

/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* ADC_H */
