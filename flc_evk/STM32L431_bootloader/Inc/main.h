/* USER CODE BEGIN Header */
/**
  ******************************************************************************
  * @file           : main.h
  * @brief          : Header for main.c file.
  *                   This file contains the common defines of the application.
  ******************************************************************************
  * @attention
  *
  * <h2><center>&copy; Copyright (c) 2021 STMicroelectronics.
  * All rights reserved.</center></h2>
  *
  * This software component is licensed by ST under BSD 3-Clause license,
  * the "License"; You may not use this file except in compliance with the
  * License. You may obtain a copy of the License at:
  *                        opensource.org/licenses/BSD-3-Clause
  *
  ******************************************************************************
  */
/* USER CODE END Header */

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __MAIN_H
#define __MAIN_H

#ifdef __cplusplus
extern "C" {
#endif

/* Includes ------------------------------------------------------------------*/
#include "stm32l4xx_hal.h"
#include "stm32l4xx_ll_system.h"
#include "stm32l4xx_ll_gpio.h"
#include "stm32l4xx_ll_exti.h"
#include "stm32l4xx_ll_bus.h"
#include "stm32l4xx_ll_cortex.h"
#include "stm32l4xx_ll_rcc.h"
#include "stm32l4xx_ll_utils.h"
#include "stm32l4xx_ll_pwr.h"
#include "stm32l4xx_ll_dma.h"

/* Private includes ----------------------------------------------------------*/
/* USER CODE BEGIN Includes */

/* USER CODE END Includes */

/* Exported types ------------------------------------------------------------*/
/* USER CODE BEGIN ET */

/* USER CODE END ET */

/* Exported constants --------------------------------------------------------*/
/* USER CODE BEGIN EC */

/* USER CODE END EC */

/* Exported macro ------------------------------------------------------------*/
/* USER CODE BEGIN EM */

/* USER CODE END EM */

/* Exported functions prototypes ---------------------------------------------*/
void Error_Handler(void);

/* USER CODE BEGIN EFP */

/* USER CODE END EFP */

/* Private defines -----------------------------------------------------------*/
#define DO_DC_DC_EN_Pin LL_GPIO_PIN_0
#define DO_DC_DC_EN_GPIO_Port GPIOB
#define DO_MEM_PWR_EN_Pin LL_GPIO_PIN_1
#define DO_MEM_PWR_EN_GPIO_Port GPIOB
#define DO_LED_GREEN_Pin LL_GPIO_PIN_12
#define DO_LED_GREEN_GPIO_Port GPIOE
#define DO_SPI_DFLASH_CS_Pin LL_GPIO_PIN_9
#define DO_SPI_DFLASH_CS_GPIO_Port GPIOD
#define U2_SIF_RX_Pin LL_GPIO_PIN_15
#define U2_SIF_RX_GPIO_Port GPIOA
#define U2_SIF_TX_Pin LL_GPIO_PIN_5
#define U2_SIF_TX_GPIO_Port GPIOD
#define BOOT0_Pin LL_GPIO_PIN_3
#define BOOT0_GPIO_Port GPIOH
void   MX_IWDG_Init(void);
/* USER CODE BEGIN Private defines */

/* USER CODE END Private defines */

#ifdef __cplusplus
}
#endif

#endif /* __MAIN_H */
