/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/dio/dio.h"
#define PWM_MAKE_TABLE
#include "hal/pwm/pwm.h"
#undef PWM_MAKE_TABLE

#include "plf/cli/cli.h"
#include "plf/trc/trc.h"

#include "sif/sif.h"


/******************************************************************************/

static void EnableOutput(const HalPwmCh_t ch)
{
  pwmArr[ch].htim->CCER |= pwmArr[ch].channel;
}

static void DisableOutput(const HalPwmCh_t ch)
{
  pwmArr[ch].htim->CCER &= ~pwmArr[ch].channel;
}


/******************************************************************************/

static void SetCompareRegister(const HalPwmCh_t ch, const uint32_t value)
{
  switch (pwmArr[ch].channel) {
    case LL_TIM_CHANNEL_CH1:
      LL_TIM_OC_SetCompareCH1(pwmArr[ch].htim, value);
      break;
    case LL_TIM_CHANNEL_CH2:
      LL_TIM_OC_SetCompareCH2(pwmArr[ch].htim, value);
      break;
    case LL_TIM_CHANNEL_CH3:
      LL_TIM_OC_SetCompareCH3(pwmArr[ch].htim, value);
      break;
    case LL_TIM_CHANNEL_CH4:
      LL_TIM_OC_SetCompareCH4(pwmArr[ch].htim, value);
      break;
    case LL_TIM_CHANNEL_CH5:
      LL_TIM_OC_SetCompareCH5(pwmArr[ch].htim, value);
      break;
    case LL_TIM_CHANNEL_CH6:
      LL_TIM_OC_SetCompareCH6(pwmArr[ch].htim, value);
      break;
    
    default:
      break;
  }  
}

static uint32_t GetCompareRegister(const HalPwmCh_t ch)
{
  switch (pwmArr[ch].channel) {
    case LL_TIM_CHANNEL_CH1:
      return LL_TIM_OC_GetCompareCH1(pwmArr[ch].htim);
    case LL_TIM_CHANNEL_CH2:
      return LL_TIM_OC_GetCompareCH2(pwmArr[ch].htim);
    case LL_TIM_CHANNEL_CH3:
      return LL_TIM_OC_GetCompareCH3(pwmArr[ch].htim);
    case LL_TIM_CHANNEL_CH4:
      return LL_TIM_OC_GetCompareCH4(pwmArr[ch].htim);
    case LL_TIM_CHANNEL_CH5:
      return LL_TIM_OC_GetCompareCH5(pwmArr[ch].htim);
    case LL_TIM_CHANNEL_CH6:
      return LL_TIM_OC_GetCompareCH6(pwmArr[ch].htim);
    
    default:
      return 0u;
  }
}


/******************************************************************************/

static uint16_t PermilToPulse(const HalPwmCh_t ch, const uint16_t permil)
{
  const uint32_t period = LL_TIM_GetAutoReload(pwmArr[ch].htim);
  const uint32_t pulse = ((period + 1u) * permil) / 1000u;
  
  return MIN_VAL(period, pulse);
}

/******************************************************************************/

void HalPwmStart(const HalPwmCh_t ch, uint16_t pulse)
{
#if !defined(EVK)  
  // According to schematic maximum allowed PWM duty cycle is 99% for the HW pump
  // driver. Very important or the pump HW driver output will produce magic smoke!
  // Changed to 97% after agreement with HW/KVI 24/06-2021
  #define PUMP_MAX_DUTY_CYCLE   (970u)
  if ((ch == PWM_PUMP1) || (ch == PWM_PUMP2) || (ch == PWM_PUMP3)) {
    const uint16_t maxPulse = PermilToPulse(ch, PUMP_MAX_DUTY_CYCLE);
    pulse = MIN_VAL(pulse, maxPulse);
  }
#endif  
  
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalPwmStart(%u,%u)", ch, pulse);
  
  if (ch >=HAL_PWM_Last) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "PWM ch >= last");
    return;
  }
  
  SetCompareRegister(ch, pulse);
  EnableOutput(ch);
  LL_TIM_GenerateEvent_UPDATE(pwmArr[ch].htim);
}

void HalPwmStartPct(const HalPwmCh_t ch, const uint8_t pct)
{
  HalPwmStartPermil(ch, pct * 10u);
}

void HalPwmStartPermil(const HalPwmCh_t ch, uint16_t permil)
{
  if (ch >= HAL_PWM_Last) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "PWM ch >= last");
    return;
  }

  permil = MIN_VAL(permil, 1000u);
  HalPwmStart(ch, PermilToPulse(ch, permil));
}


/******************************************************************************/

void HalPwmStop(const HalPwmCh_t ch)
{
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalPwmStop(%u)", ch);
  if (ch >=HAL_PWM_Last) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "PWM ch >= last");
    return;
  }

  SetCompareRegister(ch, 0);  
  DisableOutput(ch);
  LL_TIM_GenerateEvent_UPDATE(pwmArr[ch].htim);
}


/******************************************************************************/

void HalPwmInit(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_COMPONENT, "HalPwmInit()");
  
}


/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalPwmShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{

  if (cliShowHeader) {
    CliWriteLn("PWM State...");
  }
  for (uint_fast8_t idx = 0; idx < HAL_PWM_Last; idx++) {
    const uint32_t period = LL_TIM_GetAutoReload(pwmArr[idx].htim);
    const uint32_t prescaler = 1 + LL_TIM_GetPrescaler(pwmArr[idx].htim);
    const uint32_t freq = PWM_CLK_FREQ / (period * prescaler);
    
    const uint32_t pulse = GetCompareRegister(idx);
    const uint32_t percent = (pulse * 100u) / period;
    
    CliPrintf("  PWM %s (#%u): \t%u Hz\t%3u%%" CLI_NL, pwmArr[idx].name, idx, freq, percent); 	
  }

  return CLI_RESULT_OK;
}
static int_fast16_t TstHalPwmStart(CliParam_t ch, CliParam_t pct, CliParam_t param3)
{
  if (ch >= HAL_PWM_Last)
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  if (pct > 100)
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  
  HalPwmStartPct((HalPwmCh_t)ch, pct);
  
  return CLI_RESULT_OK;
}

static int_fast16_t TstHalPwmAbsStart(CliParam_t ch, CliParam_t val, CliParam_t param3)
{
  if (ch >= HAL_PWM_Last)
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  if (val > 0xFFFF)
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  
  HalPwmStart((HalPwmCh_t)ch, val);
  
  return CLI_RESULT_OK;
}

static int_fast16_t TstHalPwmStop(CliParam_t ch, CliParam_t param2, CliParam_t param3)
{
  if (ch >= HAL_PWM_Last)
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
 
  HalPwmStop((HalPwmCh_t)ch);
  
  return CLI_RESULT_OK;
}

CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(hal_pwm)
  CLI_ENTRY0( "show", "Show PWM State", TstHalPwmShow)  

  CLI_ENTRY2( "start",    "Start PWM [no:0.., pct]", TstHalPwmStart, CLI_PARAM_UINT32, CLI_PARAM_UINT32)  
  CLI_ENTRY2( "startabs", "Start PWM [no:0.., val:0..]", TstHalPwmAbsStart, CLI_PARAM_UINT32, CLI_PARAM_UINT32)  
  CLI_ENTRY1( "stop",     "Stop PWM [no:0..]", TstHalPwmStop, CLI_PARAM_UINT32)  

#ifdef TRC_ENABLE
	CLI_SUBTEST("trc",      "Trace system", trc)
#endif
//CLI_ENTRY1( "spw",      "Set Password", CliPasswordSet, CLI_PARAM_STRING)
	CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_pwm)

/******************************************************************************/
#endif
/******************************************************************************/
