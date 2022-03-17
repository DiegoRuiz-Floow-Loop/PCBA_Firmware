/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef ADC_H
#define ADC_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "hal/cfg/adc_cfg.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"
#include "plf/trc/trc.h"


/******************************************************************************/
  
extern int32_t vdda_mv;

extern const uint32_t adcChannelArray[HAL_ADC_Last];
  
   
// lock/unlog usage of ADDs (SUP do this...)
extern void AdcLock(bool lock);

// if ADCs locked, INT16_MIN is returned
// if HW error, "-1 * HAL_StatusTypeDef" is returned
extern int16_t AdcGet(HalAdcCh_t adcCh);
  
/******************************************************************************/

extern void HalAdcInit(void);

#ifdef CLI_ENABLE

  extern int_fast16_t TstHalAdcShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
  
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* ADC_H */
