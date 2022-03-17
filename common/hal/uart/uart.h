/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef UART_H
#define UART_H

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************/
	
#include "plf/plf.h"
#include "hal/cfg/uart_cfg.h"
#include "hal/dio/dio.h"

/*****************************************************************************/


extern void UartInterrupt(
	HalUart_t 	uart);


extern bool HalUartTxEmpty(
  HalUart_t  uart);

extern void HalUartTxFirst(
  HalUart_t  uart,
  uint8_t       ch);

typedef  void (*HalUartRxCallbackFunc_t)(uint8_t ch) ;
typedef  bool (*HalUartTxCallbackFunc_t)(uint8_t * ch);

extern void HalUartIrqEnable(HalUart_t uart);

extern void HalUartInit(
  HalUart_t              uart,         
  HalUartRxCallbackFunc_t   uartRxFunc,
  HalUartTxCallbackFunc_t   uartTxFunc,
  McuPin_t                  rs485Pin);    // not used when == MCUPIN_NA

/*****************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // UART_H
