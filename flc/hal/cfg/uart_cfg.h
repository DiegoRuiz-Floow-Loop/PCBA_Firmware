/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef UART_CFG_H
#define UART_CFG_H

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************/
	
// according to entry in uartArr - see below
typedef enum {
  HAL_UART_SIF,
#if !defined(BOOTLOADER)  
  HAL_UART_GSM,
#endif  
#if !defined(BOOTLOADER) && !defined(EVK)
  HAL_UART_FUTURE,
#endif  
	HAL_UART_Last
}												HalUart_t;


/*****************************************************************************/
/* only define/undef in uart_xxx.c */
#ifdef HAL_UART_CODE

#include "stm32l4xx_hal.h"

extern UART_HandleTypeDef huart1;
extern UART_HandleTypeDef huart2;
extern UART_HandleTypeDef hlpuart1;

static UART_HandleTypeDef * huartArr[HAL_UART_Last] = {
  // HAL_UART_SIF
#if defined(EVK)
  &huart2,
#else
  &hlpuart1,
#endif

  // HAL_UART_GSM
#if !defined(BOOTLOADER)
  &huart1,
#endif

  // HAL_UART_FUTURE
#if !defined(BOOTLOADER) && !defined(EVK)
  &huart2,
#endif
};

static USART_TypeDef * uartArr[HAL_UART_Last] = {
  // HAL_UART_SIF
#if defined(EVK)
  USART2,
#else
  LPUART1,
#endif

  // HAL_UART_GSM
#if !defined(BOOTLOADER)
  USART1,
#endif

  // HAL_UART_FUTURE
#if !defined(BOOTLOADER) && !defined(EVK)
  USART2,
#endif
};

#endif
/*****************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // UART_CFG_H

