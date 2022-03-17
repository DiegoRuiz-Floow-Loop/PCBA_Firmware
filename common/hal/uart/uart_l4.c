/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdarg.h>
#include <stdio.h>

#include "hal/dio/dio.h"  

#define HAL_UART_CODE
#include "hal/uart/uart.h"  
#undef HAL_UART_CODE


#include "hal/hal.h"
#include "plf/plf.h"
#include "plf/trc/trc.h"

/*****************************************************************************/

typedef struct {
  HalUartTxCallbackFunc_t       txFunc;
  HalUartRxCallbackFunc_t       rxFunc;
  McuPin_t                      rs485Pin;
  bool                        txEmpty;
} 															UartDef_t;

static UartDef_t 		            uart[HAL_UART_Last];

#define RX_ENABLE(mcuPin)   HAL_DIO_PIN_CLR(mcuPin) 
#define TX_ENABLE(mcuPin)   HAL_DIO_PIN_SET(mcuPin) 


bool HalUartTxEmpty(
  HalUart_t       device)
{
  return uart[device].txEmpty;
}

void HalUartTxFirst(
  HalUart_t       device,
  uint8_t       ch)
{
	UART_HandleTypeDef * huart = huartArr[device];
	UartDef_t * pUartDef = &uart[device];

  TX_ENABLE(pUartDef->rs485Pin);
  
  pUartDef->txEmpty = false; /* not empty in the THR until it shifts out */
	// Send data
  huart->Instance->TDR = ch;
  // Enable the UART Transmit Data Register Empty Interrupt 
	//  USART_ITConfig(uartx, USART_IT_TXE, ENABLE);
#if defined(STM32WL55xx)
  SET_BIT(huart->Instance->CR1, USART_CR1_TXEIE_TXFNFIE);
#else
  SET_BIT(huart->Instance->CR1, USART_CR1_TXEIE);
#endif
  SET_BIT(huart->Instance->CR1, USART_CR1_TCIE);

  /* Enable RXNE interrupt */
#if defined(STM32WL55xx)
  SET_BIT(huart->Instance->CR1, USART_CR1_RXNEIE_RXFNEIE);
#else
  SET_BIT(huart->Instance->CR1, USART_CR1_RXNEIE);
#endif
  
}

/*****************************************************************************/

static void UART_EndTransmit_IT(UART_HandleTypeDef *huart)
{
  /* Disable the UART Transmit Complete Interrupt */
  CLEAR_BIT(huart->Instance->CR1, USART_CR1_TCIE);

  /* Tx process is ended, restore huart->gState to Ready */
  huart->gState = HAL_UART_STATE_READY;

  /* Cleat TxISR function pointer */
  huart->TxISR = NULL;

}


void UartInterrupt(HalUart_t device)
{
	UART_HandleTypeDef * huart = huartArr[device];
//	UartDef_t * pUartDef = &uart[device];

  uint32_t isrflags   = READ_REG(huart->Instance->ISR);
  uint32_t cr1its     = READ_REG(huart->Instance->CR1);
  //uint32_t cr3its     = READ_REG(huart->Instance->CR3);

  uint32_t errorflags;
//  uint32_t errorcode;

  /* If no error occurs */
  errorflags = (isrflags & (uint32_t)(USART_ISR_PE | USART_ISR_FE | USART_ISR_ORE | USART_ISR_NE | USART_ISR_RTOF));
  if (errorflags == 0U)
  {
    /* UART in mode Receiver ---------------------------------------------------*/
    


    
#if defined(STM32WL55xx)
    if (((isrflags & USART_ISR_RXNE_RXFNE) != 0U)      && ((cr1its & USART_CR1_RXNEIE_RXFNEIE) != 0U))
#else
    if (((isrflags & USART_ISR_RXNE) != 0U)      && ((cr1its & USART_CR1_RXNEIE) != 0U))
#endif
    {
      uart[device].rxFunc(huart->Instance->RDR);
//      if (huart->RxISR != NULL)
//      {
//        huart->RxISR(huart);
//      }
      return;
    }
    //uint8_t dummy = huart->Instance->RDR;
    //return;
//  if ((errorflags != 0U) && (((cr3its & USART_CR3_EIE) != 0U) || ((cr1its & (USART_CR1_RXNEIE | USART_CR1_PEIE)) != 0U)))
  } else {
    /* UART parity error interrupt occurred -------------------------------------*/
//    if (((isrflags & USART_ISR_PE) != 0U) && ((cr1its & USART_CR1_PEIE) != 0U))
    if ((isrflags & USART_ISR_PE) != 0U)
    {
      __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_PEF);

      huart->ErrorCode |= HAL_UART_ERROR_PE;
    }

    /* UART frame error interrupt occurred --------------------------------------*/
//    if (((isrflags & USART_ISR_FE) != 0U) && ((cr3its & USART_CR3_EIE) != 0U))
    if ((isrflags & USART_ISR_FE) != 0U) 
    {
      __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_FEF);

      huart->ErrorCode |= HAL_UART_ERROR_FE;
    }

    /* UART noise error interrupt occurred --------------------------------------*/
    if ((isrflags & USART_ISR_NE) != 0U)
    {
      __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_NEF);

      huart->ErrorCode |= HAL_UART_ERROR_NE;
    }

    /* UART Over-Run interrupt occurred -----------------------------------------*/
    if ((isrflags & USART_ISR_ORE) != 0U)
    {
      __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_OREF);

      huart->ErrorCode |= HAL_UART_ERROR_ORE;
    }

    /* UART Receiver Timeout interrupt occurred ---------------------------------*/
//    if (((isrflags & USART_ISR_RTOF) != 0U) && ((cr1its & USART_CR1_RTOIE) != 0U))
    if ((isrflags & USART_ISR_RTOF) != 0U)
    {
      __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_RTOF);

      huart->ErrorCode |= HAL_UART_ERROR_RTO;
    }

    /* Call UART Error Call back function if need be ----------------------------*/
    if (huart->ErrorCode != HAL_UART_ERROR_NONE)
    {
      /* UART in mode Receiver --------------------------------------------------*/
#if defined(STM32WL55xx)
      if (((isrflags & USART_ISR_RXNE_RXFNE) != 0U)  && ((cr1its & USART_CR1_RXNEIE_RXFNEIE) != 0U))
#else
      if (((isrflags & USART_ISR_RXNE) != 0U)  && ((cr1its & USART_CR1_RXNEIE) != 0U))
#endif
      {
        if (uart[device].rxFunc)
          uart[device].rxFunc(huart->Instance->RDR);
      }

      /* If Error is to be considered as blocking :
          - Receiver Timeout error in Reception
          - Overrun error in Reception
          - any error occurs in DMA mode reception
      */

    }
    return;

  } /* End if some error occurs */

  /* UART wakeup from Stop mode interrupt occurred ---------------------------*/
  if ((isrflags & USART_ISR_WUF) != 0U)
  {
    __HAL_UART_CLEAR_FLAG(huart, UART_CLEAR_WUF);

    /* UART Rx state is not reset as a reception process might be ongoing.
       If UART handle state fields need to be reset to READY, this could be done in Wakeup callback */

    return;
  }

  /* UART in mode Transmitter ------------------------------------------------*/

#if defined(STM32WL55xx)
  if (((isrflags & USART_ISR_TXE_TXFNF) != 0U) && ((cr1its & USART_CR1_TXEIE_TXFNFIE) != 0U))
#else
  if (((isrflags & USART_ISR_TXE) != 0U) && ((cr1its & USART_CR1_TXEIE) != 0U))
#endif
  {
    uint8_t ch;
    if (uart[device].txFunc(&ch)) {
      huart->Instance->TDR = ch;
    } else {
      uart[device].txEmpty = true;
#if defined(STM32WL55xx)
      CLEAR_BIT(huart->Instance->CR1, USART_CR1_TXEIE_TXFNFIE);
#else
      CLEAR_BIT(huart->Instance->CR1, USART_CR1_TXEIE);
#endif
      /* Enable the UART Transmit Complete Interrupt */
      SET_BIT(huart->Instance->CR1, USART_CR1_TCIE);      
    }
//    if (huart->TxISR != NULL)
//    {
//      huart->TxISR(huart);
//    }
    return;
  }

  /* UART in mode Transmitter (transmission end) -----------------------------*/
  if (((isrflags & USART_ISR_TC) != 0U) && ((cr1its & USART_CR1_TCIE) != 0U))
  {
    UART_EndTransmit_IT(huart);
    return;
  }


}

/*****************************************************************************/

#if 0

#if defined(USE_FULL_LL_DRIVER)
#define __HAL_UART_CLEAR_IT_LL(__UART__, __IT_CLEAR__) \
	((__UART__)->ICR = (uint32_t)(__IT_CLEAR__))

#define __HAL_UART_ENABLE_IT_LL(__UART__, __INTERRUPT__) \
	(((((uint8_t)(__INTERRUPT__)) >> 5U) == 1)? ((__UART__)->CR1 |= (1U << ((__INTERRUPT__) & UART_IT_MASK))): \
  ((((uint8_t)(__INTERRUPT__)) >> 5U) == 2)? ((__UART__)->CR2 |= (1U << ((__INTERRUPT__) & UART_IT_MASK))): \
  ((__UART__)->CR3 |= (1U << ((__INTERRUPT__) & UART_IT_MASK))))

#define __HAL_UART_DISABLE_IT_LL(__UART__, __INTERRUPT__)  \
  (((((uint8_t)(__INTERRUPT__)) >> 5U) == 1)? ((__UART__)->CR1 &= ~ (1U << ((__INTERRUPT__) & UART_IT_MASK))): \
  ((((uint8_t)(__INTERRUPT__)) >> 5U) == 2)? ((__UART__)->CR2 &= ~ (1U << ((__INTERRUPT__) & UART_IT_MASK))): \
  ((__UART__)->CR3 &= ~ (1U << ((__INTERRUPT__) & UART_IT_MASK))))

#define __HAL_UART_ENABLE_LL(__UART__) \
	((__UART__)->CR1 |=  USART_CR1_UE)

#define __HAL_UART_DISABLE_LL(__UART__) \
  ((__UART__)->CR1 &=  ~USART_CR1_UE)

typedef enum
{
  LL_HAL_UART_ERROR_NONE      = 0x00,    /*!< No error            */
  LL_HAL_UART_ERROR_PE        = 0x01,    /*!< Parity error        */
  LL_HAL_UART_ERROR_NE        = 0x02,    /*!< Noise error         */
  LL_HAL_UART_ERROR_FE        = 0x04,    /*!< frame error         */
  LL_HAL_UART_ERROR_ORE       = 0x08,    /*!< Overrun error       */
  LL_HAL_UART_ERROR_DMA       = 0x10,    /*!< DMA transfer error  */
  LL_HAL_UART_ERROR_BUSY      = 0x20     /*!< Busy Error          */
} LL_HAL_UART_ErrorTypeDef;
#endif

/*****************************************************************************/

bool HalUartTxEmpty(
  HalUart_t       device)
{
  return uart[device].txEmpty;
}


void HalUartTxFirst(
  HalUart_t       device,
  uint8_t       ch)
{
  USART_TypeDef * uartDef = (USART_TypeDef *)uartArr[device];

  uart[device].txEmpty = false; /* not empty in the THR until it shifts out */
	// Send data
  uartDef->TDR = ch;
  // Enable the UART Transmit Data Register Empty Interrupt 
	//  USART_ITConfig(uartx, USART_IT_TXE, ENABLE);
  SET_BIT(uartDef->CR1, USART_CR1_TXEIE);
  SET_BIT(uartDef->CR1, USART_CR1_TCIE);

  /* Enable RXNE interrupt */
  SET_BIT(uartDef->CR1, USART_CR1_RXNEIE);
  
}


/*****************************************************************************/

static void UART_Transmit_IT(USART_TypeDef * uartDef, HalUart_t device)
{
	uint8_t ch;

  if (uart[device].txFunc(&ch)) {
    uartDef->TDR = ch;
  } else {
    uart[device].txEmpty = true;
    CLEAR_BIT(uartDef->CR1, USART_CR1_TXEIE);

    /* Enable the UART Transmit Complete Interrupt */
    SET_BIT(uartDef->CR1, USART_CR1_TCIE);
  }
}

void UartInterrupt(HalUart_t device)
{
  USART_TypeDef * uartDef = (USART_TypeDef *)uartArr[device];
	uint32_t ErrorCode = LL_HAL_UART_ERROR_NONE;
  uint32_t isrflags   = READ_REG(uartDef->ISR);
  uint32_t cr1its     = READ_REG(uartDef->CR1);
  uint32_t cr3its;
  uint32_t errorflags;

  /* If no error occurs */
  errorflags = (isrflags & (uint32_t)(USART_ISR_PE | USART_ISR_FE | USART_ISR_ORE | USART_ISR_NE));
  if (errorflags != 0) {
    __HAL_UART_CLEAR_IT_LL(uartDef, USART_ISR_PE | USART_ISR_FE | USART_ISR_ORE | USART_ISR_NE);
  }
  if (errorflags == RESET)
  {
    /* UART in mode Receiver ---------------------------------------------------*/
    if(((isrflags & USART_ISR_RXNE) != RESET) && ((cr1its & USART_CR1_RXNEIE) != RESET))
    {
      uart[device].rxFunc(uartDef->RDR);
      return;
    }
  }  

  /* If some errors occur */
  cr3its = READ_REG(uartDef->CR3);
  if(   (errorflags != RESET)
     && (   ((cr3its & USART_CR3_EIE) != RESET)
         || ((cr1its & (USART_CR1_RXNEIE | USART_CR1_PEIE)) != RESET)) )
  {
    /* UART parity error interrupt occurred -------------------------------------*/
    if(((isrflags & USART_ISR_PE) != RESET) && ((cr1its & USART_CR1_PEIE) != RESET))
    {
      __HAL_UART_CLEAR_IT_LL(uartDef, USART_ICR_PECF);
      ErrorCode |= LL_HAL_UART_ERROR_PE;
    }

    /* UART frame error interrupt occurred --------------------------------------*/
    if(((isrflags & USART_ISR_FE) != RESET) && ((cr3its & USART_CR3_EIE) != RESET))
    {
      __HAL_UART_CLEAR_IT_LL(uartDef, USART_ICR_FECF);
      ErrorCode |= LL_HAL_UART_ERROR_FE;
    }

    /* UART noise error interrupt occurred --------------------------------------*/
    if(((isrflags & USART_ISR_NE) != RESET) && ((cr3its & USART_CR3_EIE) != RESET))
    {
      __HAL_UART_CLEAR_IT_LL(uartDef, USART_ICR_NCF);
      ErrorCode |= LL_HAL_UART_ERROR_NE;
    }
    
    /* UART Over-Run interrupt occurred -----------------------------------------*/
    if(((isrflags & USART_ISR_ORE) != RESET) &&
       (((cr1its & USART_CR1_RXNEIE) != RESET) || ((cr3its & USART_CR3_EIE) != RESET)))
    {
      __HAL_UART_CLEAR_IT_LL(uartDef, USART_ICR_ORECF);
      ErrorCode |= LL_HAL_UART_ERROR_ORE;
    }

    /* Call UART Error Call back function if need be --------------------------*/
    if(ErrorCode != LL_HAL_UART_ERROR_NONE)
    {
      /* UART in mode Receiver ---------------------------------------------------*/
      if(((isrflags & USART_ISR_RXNE) != RESET) && ((cr1its & USART_CR1_RXNEIE) != RESET))
      {
        uart[device].rxFunc(uartDef->RDR);
      }

      /* If Overrun error occurs, or if any error occurs in DMA mode reception,
         consider error as blocking */
      if (((ErrorCode & LL_HAL_UART_ERROR_ORE) != RESET) ||
          (HAL_IS_BIT_SET(uartDef->CR3, USART_CR3_DMAR)))
      {  
        /* Blocking error : transfer is aborted
           Set the UART state ready to be able to start again the process,
           Disable Rx Interrupts, and disable Rx DMA request, if ongoing */
        CLEAR_BIT(uartDef->CR1, USART_CR1_RXNEIE);

        /* Disable the UART DMA Rx request if enabled */
        if (HAL_IS_BIT_SET(uartDef->CR3, USART_CR3_DMAR)) {
          CLEAR_BIT(uartDef->CR3, USART_CR3_DMAR);
        }
      } else {
        /* Non Blocking error : transfer could go on. 
           Error is notified to user through user error callback */
        //HAL_UART_ErrorCallback(huart);
        ErrorCode = LL_HAL_UART_ERROR_NONE;
      }
    }
    return;

  } /* End if some error occurs */

  /* UART wakeup from Stop mode interrupt occurred ---------------------------*/
  if(((isrflags & USART_ISR_WUF) != RESET) && ((cr3its & USART_CR3_WUFIE) != RESET))
  {
    __HAL_UART_CLEAR_IT_LL(uartDef, USART_ICR_WUCF);
    /* Set the UART state ready to be able to start again the process */
//    huart->gState  = HAL_UART_STATE_READY;
//    huart->RxState = HAL_UART_STATE_READY;
    //HAL_UARTEx_WakeupCallback(huart);
    return;
  }

  /* UART in mode Transmitter ------------------------------------------------*/
  if(((isrflags & USART_ISR_TXE) != RESET) && ((cr1its & USART_CR1_TXEIE) != RESET))
  {
    UART_Transmit_IT(uartDef, device);
    return;
  }

  /* UART in mode Transmitter (transmission end) -----------------------------*/
  if(((isrflags & USART_ISR_TC) != RESET) && ((cr1its & USART_CR1_TCIE) != RESET))
  {
    CLEAR_BIT(uartDef->CR1, USART_CR1_TCIE);
    return;
  }
}


//#define UART_IT_TXE                         0x0727U                  /*!< UART transmit data register empty interruption */ 
//#define UART_IT_PE                          0x0028U                  /*!< UART parity error interruption                 */    
//#define UART_IT_TC                          0x0626U                  /*!< UART transmission complete interruption        */    
#define UART_IT_RXNE                        0x0525U                  /*!< UART read data register not empty interruption */    
//#define UART_IT_ERR                         0x0060U                  /*!< UART error interruption         */   

#define UART_IT_MASK                        0x001FU  /*!< UART interruptions flags mask */
////..\..\..\common\hal\uart\uart_l4.c(438): error:  #20: identifier "UART_IT_MASK" is undefined

//#if defined(USART_CR1_FIFOEN)
//#define UART_FLAG_TXE                       USART_ISR_TXE_TXFNF     /*!< UART transmit data register empty         */
//#define UART_FLAG_TXFNF                     USART_ISR_TXE_TXFNF     /*!< UART TXFIFO not full                      */
//#else
#define UART_FLAG_TXE                       USART_ISR_TXE           /*!< UART transmit data register empty         */
//#endif
////..\..\..\common\hal\uart\uart_l4.c(507): error:  #20: identifier "UART_FLAG_TXE" is undefined

#define UART_FLAG_TC                        USART_ISR_TC            /*!< UART transmission complete                */
////..\..\..\common\hal\uart\uart_l4.c(510): error:  #20: identifier "UART_FLAG_TC" is undefined

//#if defined(USART_CR1_FIFOEN)
#define UART_FLAG_RXNE                      LL_USART_ISR_RXNE    /*!< UART read data register not empty         */
//#define UART_FLAG_RXFNE                     USART_ISR_RXNE_RXFNE    /*!< UART RXFIFO not empty                     */
//#else
//#define UART_FLAG_RXNE                      USART_ISR_RXNE          /*!< UART read data register not empty         */
//#endif
////..\..\..\common\hal\uart\uart_l4.c(514): error:  #20: identifier "UART_FLAG_RXNE" is undefined

#endif

void HalUartInit(
  HalUart_t               device,
  HalUartRxCallbackFunc_t uartRxFunc,
  HalUartTxCallbackFunc_t uartTxFunc,
  McuPin_t                  rs485Pin)
{
  const UART_HandleTypeDef * huart = huartArr[device];
	UartDef_t * pUartDef = (UartDef_t *)&uart[device];
	
  pUartDef->rxFunc 			= uartRxFunc;
  pUartDef->txFunc 			= uartTxFunc;
  pUartDef->txEmpty     = true;		
  pUartDef->rs485Pin    = rs485Pin;
 
  /* Enable the UART Transmit Empty Interrupt */
  //__HAL_UART_CLEAR_IT_LL(uartDef, UART_FLAG_TXE);
  __HAL_UART_ENABLE_IT(huart, UART_IT_TXE);

  //__HAL_UART_CLEAR_IT_LL(uartDef, UART_FLAG_TC);
  __HAL_UART_ENABLE_IT(huart, UART_IT_TC);

  /* Enable the UART Data Register not empty Interrupt */
  __HAL_UART_CLEAR_IT(huart, UART_FLAG_RXNE);
  __HAL_UART_ENABLE_IT(huart, UART_IT_RXNE);

  /* Enable the peripheral */
  __HAL_UART_ENABLE(huart);

}

/*****************************************************************************/

