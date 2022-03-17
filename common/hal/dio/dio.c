/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <ctype.h>

#include "hal/hal.h"
#include "hal/dio/dio.h"
#include "plf/cli/cli.h"
#include "plf/trc/trc.h"

#ifdef CLI_ENABLE
	#include "sif/sif.h"
#endif

const GPIO_TypeDef * const gpio[GPIO_Last] = {
  GPIOA,
  GPIOB,
  GPIOC,
#if defined(GPIOD)	
  GPIOD,
#endif	
#if defined(GPIOE)	
  GPIOE,
#endif	
#if defined(GPIOF)	
  GPIOF,
#endif	
#if defined(GPIOG)	
  GPIOG,
#endif	
#if defined(GPIOH)	
  GPIOH,
#endif
#if defined(GPIOI)	
  GPIOI,
#endif
#if defined(GPIOJ)	
  GPIOJ,
#endif
#if defined(GPIOK)	
  GPIOK,
#endif
};

const char gpioCh[] = "ABC"
#if defined(GPIOD)	
  "D"
#endif	
#if defined(GPIOE)	
  "E"
#endif	
#if defined(GPIOF)	
  "F"
#endif	
#if defined(GPIOG)	
  "G"
#endif	
#if defined(GPIOH)	
  "H"
#endif	
#if defined(GPIOI)	
  "I"
#endif	
#if defined(GPIOJ)	
  "J"
#endif	
#if defined(GPIOK)	
  "K"
#endif	
 ;

/******************************************************************************/

//void HalDioPinClr(McuPin_t mcuPin)
//{
//  if (MCUPIN_NA != mcuPin)
//    xHAL_DIO_PIN_CLR(mcuPin);
//}

//void HalDioPinSet(McuPin_t mcuPin)
//{
//  if (MCUPIN_NA != mcuPin)
//    xHAL_DIO_PIN_SET(mcuPin);
//}

//GPIO_PinState HAL_DIO_PIN_GET(McuPin_t mcuPin)
//{
//	
//  if (MCUPIN_NA != mcuPin) {
//    return HAL_GPIO_ReadPin(MCUPIN_PORT_GET(mcuPin), MCUPIN_PIN_GET(mcuPin));
//  } else {
//    return GPIO_PIN_RESET;
//	}
//}


void HalDioInit(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_COMPONENT, "HalDioInit()");
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalDioShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  int g;
  if (cliShowHeader)
    CliWriteLn("HAL DIO State...");
  for (g=0; g<GPIO_Last; g++) {
    CliPrintf("  GPIO%c:\tin:0x%04X, out:0x%04X" CLI_NL, gpioCh[g], (uint16_t)gpio[g]->IDR, (uint16_t)gpio[g]->ODR); 	
  }
  return CLI_RESULT_OK;
}

static int_fast16_t TstHalDioSet(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  int ix;
  char port = toupper((char)param1);
  uint16_t pin  = (uint8_t)param2;
  bool level = (bool)param3;
  
  for (ix=0; ix<GPIO_Last; ix++) {
    if (port == gpioCh[ix]) 
      break;
  }
  if (ix == GPIO_Last) {
    CliPrintf("Ports: %s" CLI_NL, gpioCh);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }

  if (pin >= 16) {
    CliPrintf("Pins: 0..15" CLI_NL, gpioCh);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  } 
  HAL_GPIO_WritePin((GPIO_TypeDef *)gpio[ix], 1 << pin, (GPIO_PinState)level);
  return CLI_RESULT_OK;
}


CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(hal_dio)
  CLI_ENTRY0( "show", "Show DIO State", TstHalDioShow)  
  CLI_ENTRY3( "dout", "Set DOUT [gpio:a.., pin:0..15, onoff]", TstHalDioSet, CLI_PARAM_CHAR, CLI_PARAM_UINT32, CLI_PARAM_ONOFF)  
#ifdef TRC_ENABLE
	CLI_SUBTEST("trc",      "Trace system", trc)
#endif
//CLI_ENTRY1( "spw",      "Set Password", CliPasswordSet, CLI_PARAM_STRING)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_dio)

/******************************************************************************/
#endif
/******************************************************************************/
