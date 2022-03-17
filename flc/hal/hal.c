/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stddef.h>
#include <stdio.h>

#include "stm32l4xx_hal.h"

#include "hal/hal.h"
#include "hal/dio/dio.h"
#include "hal/pwm/pwm.h"
#include "hal/dflash/dflash.h"
#include "hal/fram/fram.h"
#include "hal/adc/adc.h"
#include "hal/rtc/rtc.h"

#include "plf/plf.h"
#include "plf/key/key.h"
#include "plf/trc/trc.h"
#include "plf/blc/blc.h"

#include "sif/sif.h"

/****************************************************************************/
/* Keys/SW */

#if !defined(BOOTLOADER)

#if !defined(EVK)
typedef struct {
  McuPin_t noPin;   // Normally open
  McuPin_t ncPin;   // Normally closed
} XorButton_t;

static const XorButton_t BTN_TABLE[KEY_Last] = {
  { .noPin = MP_BTN_1_NO, .ncPin = MP_BTN_1_NC },   // KEY_UI_BTN_1
  { .noPin = MP_BTN_2_NO, .ncPin = MP_BTN_2_NC },   // KEY_UI_BTN_2
};
#endif

void HalReadButtons(uint8_t * keys)
{
	uint8_t k = 0; 
  
  #if defined(EVK)
  if (!HAL_DIO_PIN_GET(MP_USER_BTN))    k |= (1 << KEY_UI_BTN_1);
  #else
  static uint8_t previousKeys = 0x00;
  for (uint_fast8_t idx = 0; idx < KEY_Last; idx++) {
    const bool ncBtn = HAL_DIO_PIN_GET(BTN_TABLE[idx].ncPin);
    const bool noBtn = HAL_DIO_PIN_GET(BTN_TABLE[idx].noPin);
    
    // If the normally open and normally closed inputs are equal then the button
    // inputs are in an undefined state and the previous button input will be 
    // used instead.
    if (BOOL_EQUAL(ncBtn, noBtn)) {
      k |= previousKeys & (0x1 << idx);
    } else {
      if (noBtn) k |= (0x1 << idx);
    }
  }
  previousKeys = k;
  #endif

  *keys = k;
}

void HalCfgGet(uint8_t * rev)
{
	uint8_t r = 0; 
 
#if defined(EVK)  
  if (HAL_DIO_PIN_GET(MP_CONFIG_0))   r |= (1 << 0);
  if (HAL_DIO_PIN_GET(MP_CONFIG_1))   r |= (1 << 1); 
#else  
  if (HAL_DIO_PIN_GET(MP_HW_CFG0))    r |= (1 << 0);
  if (HAL_DIO_PIN_GET(MP_HW_CFG1))    r |= (1 << 1);
#endif

  *rev = r;
}

int HalHwRevGet(void)
{
	uint8_t r = 0; 

#if defined(EVK)  
  if (HAL_DIO_PIN_GET(MP_HWREV_0))    r |= (1 << 0);
  if (HAL_DIO_PIN_GET(MP_HWREV_1))    r |= (1 << 1);  
#else  
  if (HAL_DIO_PIN_GET(MP_HW_REV0))    r |= (1 << 0);
  if (HAL_DIO_PIN_GET(MP_HW_REV1))    r |= (1 << 1);
#endif
  
  return 1+r;
}

#endif

/******************************************************************************/
/* INTERRUPT */

void HalInterruptDisable(HalIrqStat_t * oldState)
{
  HalIrqStat_t primask_bit;
  primask_bit = __get_PRIMASK();
  __disable_irq();
	if (oldState)
		*oldState = primask_bit;
}

void HalInterruptEnable(HalIrqStat_t * oldState)
{
  HalIrqStat_t primask_bit;
  primask_bit = __get_PRIMASK();
  __enable_irq();
	if (oldState)
    *oldState = primask_bit;
}

void HalInterruptRestore(HalIrqStat_t primask_bit)
{
  __set_PRIMASK(primask_bit);
}


/****************************************************************************/
/* EXT INTERRUPT */

#if !defined(BOOTLOADER)

#include "modem.h"
#include "pow/pow.h"
#include "shc/pump.h"

#if defined(EVK)
void HalExintHandler(uint32_t exint)
{
  switch (exint) {
    case (XI_GSM_STATUS_Pin):
      ModemXiStatusCb();
      break;    

    default:
      break;
  }
}
#endif

#if !defined(EVK)
void HAL_GPIO_EXTI_Callback(uint16_t GPIO_Pin)
{
  switch (GPIO_Pin) {
    case (XI_MDM_STATUS_Pin):
      ModemXiStatusCb();
      break;
    
    case (XI_PUMP_FAULT1_Pin):
      PumpXiCurrentFaultCb(PUMP_1);
      break;      
    case (XI_PUMP_FAULT2_Pin):
      PumpXiCurrentFaultCb(PUMP_2);
      break;      
    case (XI_PUMP_FAULT3_Pin):
      PumpXiCurrentFaultCb(PUMP_3);
      break;
    
    case (XI_PWR_FAULT_24V_Pin):
      PowerXi24VFaultCb();
      break;
    case (XI_PWR_FAULT_SENS_Pin):
      PowerXiSensorFaultCb();
      break;
    
    case (XI_PWR_FAULT_CUR_Pin):
    case (XI_PWR_FAULT_FU_Pin):
    default:
      break;
  }
}
#endif

#endif

/****************************************************************************/
/* SPI INTERRUPT */

void HAL_SPI_TxCpltCallback(SPI_HandleTypeDef *hspi)
{
  if (hspi == &SPI_DFLASH) {
    DFlashSpiCallback();
  }
}

void HAL_SPI_RxCpltCallback(SPI_HandleTypeDef *hspi)
{
  if (hspi == &SPI_DFLASH) {
    DFlashSpiCallback();
  }
}

void HAL_SPI_TxRxCpltCallback(SPI_HandleTypeDef *hspi)
{
  if (hspi == &SPI_DFLASH) {
    DFlashSpiCallback();
  }
}

void HAL_SPI_TxHalfCpltCallback(SPI_HandleTypeDef *hspi)
{
}

void HAL_SPI_RxHalfCpltCallback(SPI_HandleTypeDef *hspi)
{
}

void HAL_SPI_TxRxHalfCpltCallback(SPI_HandleTypeDef *hspi)
{
}

void HAL_SPI_ErrorCallback(SPI_HandleTypeDef *hspi)
{
  TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_SPI_ErrorCallback");
}

void HAL_SPI_AbortCpltCallback(SPI_HandleTypeDef *hspi)
{
  TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_SPI_AbortCpltCallback");
}


/****************************************************************************/

void HalReboot(void)
{
  BlcReboot();
}

/****************************************************************************/

void HalInit(void)
{ 
  HalInterruptEnable(NULL);
	HalDioInit();
  HalDFlashInit();
  
#if !defined(BOOTLOADER)
  HalAdcInit();
  HalPwmInit();
  HalFramInit();
#endif
}

/******************************************************************************/
#ifdef CLI_ENABLE 
/******************************************************************************/

CLI_DECLARE_SUBTEST(trc)        
CLI_DECLARE_SUBTEST(hal_dio)
CLI_DECLARE_SUBTEST(hal_adc)
CLI_DECLARE_SUBTEST(hal_pwm)
CLI_DECLARE_SUBTEST(hal_fram)
CLI_DECLARE_SUBTEST(hal_dflash)

int_fast16_t CliReboot(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  BlcReboot();
  return CLI_RESULT_OK;
}

static int_fast16_t TestHalShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  CliWriteLine();
  
  TstHalDioShow(0,0,0);
  TstHalAdcShow(0,0,0);
  TstHalPwmShow(0,0,0);
  TstHalFramShow(0,0,0);
  TstHalDFlashShow(0,0,0);
  cliShowHeader = true;


  return CLI_RESULT_OK;
}

#if defined(HAL_WDT_ENABLE)
static int_fast16_t TestWdt(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  UNUSED(param1);
  UNUSED(param2);
  UNUSED(param3);

  for (uint_fast32_t i = 1; ; i++) {
    CliPrintf("now: %u" CLI_NL, i);
  }

  return CLI_RESULT_OK;
}
#endif


CLI_START_TABLE(hwt)
  CLI_ENTRY0( "show", "Show HAL Status", TestHalShow)
  CLI_SUBTEST("dio", "DIO test", hal_dio)
  CLI_SUBTEST("adc", "ADC test", hal_adc)
	CLI_SUBTEST("pwm", "PWM test", hal_pwm)
	CLI_SUBTEST("fram", "FRAM test", hal_fram)
	CLI_SUBTEST("dflash", "Data Flash test", hal_dflash)

#if defined(HAL_WDT_ENABLE)
  CLI_ENTRY0( "wdt", "WDT test", TestWdt)
#endif

  CLI_SUBTEST("trc",      "Trace system", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hwt)

/******************************************************************************/
#endif
/******************************************************************************/
