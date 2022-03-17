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
#define DI_HW_CFG1_Pin GPIO_PIN_5
#define DI_HW_CFG1_GPIO_Port GPIOF
#define DI_HW_CFG0_Pin GPIO_PIN_1
#define DI_HW_CFG0_GPIO_Port GPIOB
#define DI_HW_REV0_Pin GPIO_PIN_11
#define DI_HW_REV0_GPIO_Port GPIOF
#define DI_HW_REV1_Pin GPIO_PIN_12
#define DI_HW_REV1_GPIO_Port GPIOF
#define DO_DBG_LED_HB_Pin GPIO_PIN_14
#define DO_DBG_LED_HB_GPIO_Port GPIOE
#define DO_PWR_EN_MEM_Pin GPIO_PIN_10
#define DO_PWR_EN_MEM_GPIO_Port GPIOD
#define DO_MEM_RST_Pin GPIO_PIN_11
#define DO_MEM_RST_GPIO_Port GPIOD
#define SPI_MEM_SCK_Pin GPIO_PIN_2
#define SPI_MEM_SCK_GPIO_Port GPIOG
#define SPI_MEM_MISO_Pin GPIO_PIN_3
#define SPI_MEM_MISO_GPIO_Port GPIOG
#define SPI_MEM_MOSI_Pin GPIO_PIN_4
#define SPI_MEM_MOSI_GPIO_Port GPIOG
#define DO_nCS_FLASH_Pin GPIO_PIN_5
#define DO_nCS_FLASH_GPIO_Port GPIOG
#define U_DBG_TX_Pin GPIO_PIN_7
#define U_DBG_TX_GPIO_Port GPIOG
#define U_DBG_RX_Pin GPIO_PIN_8
#define U_DBG_RX_GPIO_Port GPIOG
#define SWDIO_Pin GPIO_PIN_13
#define SWDIO_GPIO_Port GPIOA
#define SWCLK_Pin GPIO_PIN_14
#define SWCLK_GPIO_Port GPIOA
void   MX_IWDG_Init(void);
/* USER CODE BEGIN Private defines */

/* USER CODE END Private defines */

#ifdef __cplusplus
}
#endif

#endif /* __MAIN_H */
