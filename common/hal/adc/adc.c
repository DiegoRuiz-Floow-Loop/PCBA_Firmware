/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/hal.h"
#include "hal/adc/adc.h"

#include "plf/evos/evos.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

#include "sif/sif.h"

/******************************************************************************/

/* enum to channel - defined in adc_cfg.h */

#if defined(STM32L431xx)  
  extern ADC_HandleTypeDef hadc1;
  #define hadc hadc1
#else
  extern ADC_HandleTypeDef hadc;
#endif

extern ADC_HandleTypeDef hadc1;


const uint32_t adcChannelArray[HAL_ADC_Last] = ADC_CHANNEL_ARRAY;

int32_t vdda_mv = 3300;

extern ADC_HandleTypeDef hadc1;

/******************************************************************************/

static ADC_ChannelConfTypeDef sConfig = {0};

int16_t AdcGet(HalAdcCh_t ch)
{
  int16_t v;
  HAL_StatusTypeDef st;

  
  if (ch >= HAL_ADC_Last) 
    return 0;
  
//  if (adcLocked)
//    return INT16_MIN;  


  sConfig.Channel = adcChannelArray[ch];
#if defined(STM32G070xx)
  sConfig.Rank = ADC_REGULAR_RANK_1;
  sConfig.SamplingTime = ADC_SAMPLINGTIME_COMMON_1;
#elif defined(STM32L071xx  )
  sConfig.Rank = ADC_RANK_CHANNEL_NUMBER;
#elif defined(STM32L475xx) || defined(STM32L431xx) || defined(STM32L471xx)
  sConfig.Rank = ADC_REGULAR_RANK_1;
  sConfig.SamplingTime = ADC_SAMPLETIME_2CYCLES_5;
  sConfig.SingleDiff = ADC_SINGLE_ENDED;
  sConfig.OffsetNumber = ADC_OFFSET_NONE;
  sConfig.Offset = 0;
#elif defined(STM32F767xx)
  sConfig.Rank = ADC_REGULAR_RANK_1;
  sConfig.SamplingTime = ADC_SAMPLETIME_3CYCLES;
#elif defined(STM32WL55xx)
  sConfig.Rank = ADC_REGULAR_RANK_1;
  sConfig.SamplingTime = ADC_SAMPLETIME_3CYCLES_5;

#else  
  #error "fix"
#endif
  st = HAL_ADC_ConfigChannel(&hadc, &sConfig);
	if (st != HAL_OK) 
    return 0;
  
  st = HAL_ADC_Start(&hadc);
	if (st != HAL_OK) 
    return 0;

  st = HAL_ADC_PollForConversion(&hadc, 1000);
	if (st != HAL_OK) 
    return 0;

  v = HAL_ADC_GetValue(&hadc);

  HAL_ADC_Stop(&hadc);
  
	if (st != HAL_OK) 
    return 0;

  return v;
}

/******************************************************************************/


void HalAdcInit(void)
{

  #if defined(STM32F767xx)
    sConfig.Rank = ADC_REGULAR_RANK_1;
    static const uint16_t * const vrefint_cal = (uint16_t*)0x1FF0F44A;  // 0x1FF0F44A - 0x1FF0 F44B
    uint16_t vrefint_data = AdcGet(HAL_ADC_VREF);
    vdda_mv = (3300 * vrefint_data) / (*vrefint_cal);
  #else
    vdda_mv = 3300;  
  #endif
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalAdcShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  HalAdcCh_t a;
  if (cliShowHeader)
    CliWriteLn("HAL ADC State...");
  for (a=(HalAdcCh_t)0; a<HAL_ADC_Last; a++) {
    CliPrintf("  ADC %u:\t%d" CLI_NL, 1+a, AdcGet(a)); 	
  }
  return CLI_RESULT_OK;
}

CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(hal_adc)
  CLI_ENTRY0( "show", "Show current ADC values", TstHalAdcShow)  
#ifdef TRC_ENABLE
  CLI_SUBTEST("trc", "Trace System", trc)
#endif
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_adc)

/******************************************************************************/
#endif
/******************************************************************************/
