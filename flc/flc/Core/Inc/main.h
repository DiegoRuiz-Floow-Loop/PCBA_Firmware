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
#include "stm32l4xx_ll_tim.h"
#include "stm32l4xx_ll_bus.h"
#include "stm32l4xx_ll_cortex.h"
#include "stm32l4xx_ll_rcc.h"
#include "stm32l4xx_ll_system.h"
#include "stm32l4xx_ll_utils.h"
#include "stm32l4xx_ll_pwr.h"
#include "stm32l4xx_ll_gpio.h"
#include "stm32l4xx_ll_dma.h"

#include "stm32l4xx_ll_exti.h"

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
#define DO_PWR_EN_VALVE_Pin GPIO_PIN_2
#define DO_PWR_EN_VALVE_GPIO_Port GPIOE
#define PWM_VALVE1_Pin GPIO_PIN_3
#define PWM_VALVE1_GPIO_Port GPIOE
#define PWM_VALVE2_Pin GPIO_PIN_4
#define PWM_VALVE2_GPIO_Port GPIOE
#define PWM_VALVE3_Pin GPIO_PIN_5
#define PWM_VALVE3_GPIO_Port GPIOE
#define PWM_VALVE4_Pin GPIO_PIN_6
#define PWM_VALVE4_GPIO_Port GPIOE
#define DO_PWR_EN_MMI_Pin GPIO_PIN_13
#define DO_PWR_EN_MMI_GPIO_Port GPIOC
#define DI_TANK_SENSOR1_Pin GPIO_PIN_0
#define DI_TANK_SENSOR1_GPIO_Port GPIOF
#define DI_TANK_SENSOR2_Pin GPIO_PIN_1
#define DI_TANK_SENSOR2_GPIO_Port GPIOF
#define DI_TANK_SENSOR3_Pin GPIO_PIN_2
#define DI_TANK_SENSOR3_GPIO_Port GPIOF
#define DI_TANK_SENSOR4_Pin GPIO_PIN_3
#define DI_TANK_SENSOR4_GPIO_Port GPIOF
#define DI_TANK_SENSOR5_Pin GPIO_PIN_4
#define DI_TANK_SENSOR5_GPIO_Port GPIOF
#define DI_HW_CFG1_Pin GPIO_PIN_5
#define DI_HW_CFG1_GPIO_Port GPIOF
#define PWM_VALVE9_Pin GPIO_PIN_6
#define PWM_VALVE9_GPIO_Port GPIOF
#define PWM_VALVE10_Pin GPIO_PIN_7
#define PWM_VALVE10_GPIO_Port GPIOF
#define DO_PWR_EN_MODEM_Pin GPIO_PIN_8
#define DO_PWR_EN_MODEM_GPIO_Port GPIOF
#define DO_PWR_EN_5V_Pin GPIO_PIN_9
#define DO_PWR_EN_5V_GPIO_Port GPIOF
#define DO_MEASURE_EN_Pin GPIO_PIN_10
#define DO_MEASURE_EN_GPIO_Port GPIOF
#define DI_HW_CFG0_Pin GPIO_PIN_1
#define DI_HW_CFG0_GPIO_Port GPIOB
#define DO_PWR_EN_POS_Pin GPIO_PIN_2
#define DO_PWR_EN_POS_GPIO_Port GPIOB
#define DI_HW_REV0_Pin GPIO_PIN_11
#define DI_HW_REV0_GPIO_Port GPIOF
#define DI_HW_REV1_Pin GPIO_PIN_12
#define DI_HW_REV1_GPIO_Port GPIOF
#define XI_PUMP_FAULT1_Pin GPIO_PIN_13
#define XI_PUMP_FAULT1_GPIO_Port GPIOF
#define XI_PUMP_FAULT1_EXTI_IRQn EXTI15_10_IRQn
#define XI_PUMP_FAULT2_Pin GPIO_PIN_14
#define XI_PUMP_FAULT2_GPIO_Port GPIOF
#define XI_PUMP_FAULT2_EXTI_IRQn EXTI15_10_IRQn
#define XI_PUMP_FAULT3_Pin GPIO_PIN_15
#define XI_PUMP_FAULT3_GPIO_Port GPIOF
#define XI_PUMP_FAULT3_EXTI_IRQn EXTI15_10_IRQn
#define DO_PWR_EN_PUMP1_Pin GPIO_PIN_0
#define DO_PWR_EN_PUMP1_GPIO_Port GPIOG
#define DO_PWR_EN_PUMP2_Pin GPIO_PIN_1
#define DO_PWR_EN_PUMP2_GPIO_Port GPIOG
#define DO_PWR_EN_PUMP3_Pin GPIO_PIN_7
#define DO_PWR_EN_PUMP3_GPIO_Port GPIOE
#define DO_PUMP_DIR1_Pin GPIO_PIN_8
#define DO_PUMP_DIR1_GPIO_Port GPIOE
#define PWM_PUMP1_Pin GPIO_PIN_9
#define PWM_PUMP1_GPIO_Port GPIOE
#define DO_PUMP_DIR2_Pin GPIO_PIN_10
#define DO_PUMP_DIR2_GPIO_Port GPIOE
#define PWM_PUMP2_Pin GPIO_PIN_11
#define PWM_PUMP2_GPIO_Port GPIOE
#define DO_PUMP_DIR3_Pin GPIO_PIN_12
#define DO_PUMP_DIR3_GPIO_Port GPIOE
#define PWM_PUMP3_Pin GPIO_PIN_13
#define PWM_PUMP3_GPIO_Port GPIOE
#define DO_DBG_LED_HB_Pin GPIO_PIN_14
#define DO_DBG_LED_HB_GPIO_Port GPIOE
#define DO_PWR_EN_24VSENS_Pin GPIO_PIN_15
#define DO_PWR_EN_24VSENS_GPIO_Port GPIOE
#define PWM_RGB_STRIP_GREEN_Pin GPIO_PIN_10
#define PWM_RGB_STRIP_GREEN_GPIO_Port GPIOB
#define PWM_RGB_STRIP_BLUE_Pin GPIO_PIN_11
#define PWM_RGB_STRIP_BLUE_GPIO_Port GPIOB
#define DO_SENSOR16_SEL_Pin GPIO_PIN_12
#define DO_SENSOR16_SEL_GPIO_Port GPIOB
#define DI_SENSOR16D_Pin GPIO_PIN_13
#define DI_SENSOR16D_GPIO_Port GPIOB
#define DO_SENSOR17_SEL_Pin GPIO_PIN_14
#define DO_SENSOR17_SEL_GPIO_Port GPIOB
#define DI_SENSOR17D_Pin GPIO_PIN_15
#define DI_SENSOR17D_GPIO_Port GPIOB
#define DO_SENSOR18_SEL_Pin GPIO_PIN_8
#define DO_SENSOR18_SEL_GPIO_Port GPIOD
#define DI_SENSOR18D_Pin GPIO_PIN_9
#define DI_SENSOR18D_GPIO_Port GPIOD
#define DO_PWR_EN_MEM_Pin GPIO_PIN_10
#define DO_PWR_EN_MEM_GPIO_Port GPIOD
#define DO_MEM_RST_Pin GPIO_PIN_11
#define DO_MEM_RST_GPIO_Port GPIOD
#define PWM_VALVE5_Pin GPIO_PIN_12
#define PWM_VALVE5_GPIO_Port GPIOD
#define PWM_VALVE6_Pin GPIO_PIN_13
#define PWM_VALVE6_GPIO_Port GPIOD
#define PWM_VALVE7_Pin GPIO_PIN_14
#define PWM_VALVE7_GPIO_Port GPIOD
#define PWM_VALVE8_Pin GPIO_PIN_15
#define PWM_VALVE8_GPIO_Port GPIOD
#define SPI_MEM_SCK_Pin GPIO_PIN_2
#define SPI_MEM_SCK_GPIO_Port GPIOG
#define SPI_MEM_MISO_Pin GPIO_PIN_3
#define SPI_MEM_MISO_GPIO_Port GPIOG
#define SPI_MEM_MOSI_Pin GPIO_PIN_4
#define SPI_MEM_MOSI_GPIO_Port GPIOG
#define DO_nCS_FLASH_Pin GPIO_PIN_5
#define DO_nCS_FLASH_GPIO_Port GPIOG
#define DO_nCS_FRAM_Pin GPIO_PIN_6
#define DO_nCS_FRAM_GPIO_Port GPIOG
#define U_DBG_TX_Pin GPIO_PIN_7
#define U_DBG_TX_GPIO_Port GPIOG
#define U_DBG_RX_Pin GPIO_PIN_8
#define U_DBG_RX_GPIO_Port GPIOG
#define PWM_BTN_1_LIGHT_Pin GPIO_PIN_6
#define PWM_BTN_1_LIGHT_GPIO_Port GPIOC
#define PWM_BTN_2_LIGHT_Pin GPIO_PIN_7
#define PWM_BTN_2_LIGHT_GPIO_Port GPIOC
#define XI_PWR_FAULT_CUR_Pin GPIO_PIN_8
#define XI_PWR_FAULT_CUR_GPIO_Port GPIOC
#define XI_PWR_FAULT_CUR_EXTI_IRQn EXTI9_5_IRQn
#define XI_MDM_STATUS_Pin GPIO_PIN_9
#define XI_MDM_STATUS_GPIO_Port GPIOC
#define XI_MDM_STATUS_EXTI_IRQn EXTI9_5_IRQn
#define DO_MDM_PWR_KEY_Pin GPIO_PIN_8
#define DO_MDM_PWR_KEY_GPIO_Port GPIOA
#define U_MDM_TX_Pin GPIO_PIN_9
#define U_MDM_TX_GPIO_Port GPIOA
#define U_MDM_RX_Pin GPIO_PIN_10
#define U_MDM_RX_GPIO_Port GPIOA
#define DI_UV_ON_Pin GPIO_PIN_11
#define DI_UV_ON_GPIO_Port GPIOA
#define DO_PWR_EN_UV_Pin GPIO_PIN_12
#define DO_PWR_EN_UV_GPIO_Port GPIOA
#define SWDIO_Pin GPIO_PIN_13
#define SWDIO_GPIO_Port GPIOA
#define SWCLK_Pin GPIO_PIN_14
#define SWCLK_GPIO_Port GPIOA
#define PWM_SHOWER_LIGHT_Pin GPIO_PIN_15
#define PWM_SHOWER_LIGHT_GPIO_Port GPIOA
#define DO_SENSOR1_SEL_Pin GPIO_PIN_10
#define DO_SENSOR1_SEL_GPIO_Port GPIOC
#define DO_SENSOR2_SEL_Pin GPIO_PIN_11
#define DO_SENSOR2_SEL_GPIO_Port GPIOC
#define DO_SENSOR3_SEL_Pin GPIO_PIN_12
#define DO_SENSOR3_SEL_GPIO_Port GPIOC
#define DO_SENSOR4_SEL_Pin GPIO_PIN_0
#define DO_SENSOR4_SEL_GPIO_Port GPIOD
#define DO_SENSOR5_SEL_Pin GPIO_PIN_1
#define DO_SENSOR5_SEL_GPIO_Port GPIOD
#define DO_SENSOR6_SEL_Pin GPIO_PIN_2
#define DO_SENSOR6_SEL_GPIO_Port GPIOD
#define DO_SENSOR7_SEL_Pin GPIO_PIN_3
#define DO_SENSOR7_SEL_GPIO_Port GPIOD
#define XI_PWR_FAULT_SENS_Pin GPIO_PIN_4
#define XI_PWR_FAULT_SENS_GPIO_Port GPIOD
#define XI_PWR_FAULT_SENS_EXTI_IRQn EXTI4_IRQn
#define U_FU_TX_Pin GPIO_PIN_5
#define U_FU_TX_GPIO_Port GPIOD
#define U_FU_RX_Pin GPIO_PIN_6
#define U_FU_RX_GPIO_Port GPIOD
#define XI_PWR_FAULT_FU_Pin GPIO_PIN_7
#define XI_PWR_FAULT_FU_GPIO_Port GPIOD
#define XI_PWR_FAULT_FU_EXTI_IRQn EXTI9_5_IRQn
#define DO_PWR_EN_FU_Pin GPIO_PIN_9
#define DO_PWR_EN_FU_GPIO_Port GPIOG
#define DIO_FU1_Pin GPIO_PIN_10
#define DIO_FU1_GPIO_Port GPIOG
#define DIO_FU2_Pin GPIO_PIN_11
#define DIO_FU2_GPIO_Port GPIOG
#define DIO_FU3_Pin GPIO_PIN_12
#define DIO_FU3_GPIO_Port GPIOG
#define DIO_FU4_Pin GPIO_PIN_13
#define DIO_FU4_GPIO_Port GPIOG
#define DIO_FU5_Pin GPIO_PIN_14
#define DIO_FU5_GPIO_Port GPIOG
#define DIO_FU6_Pin GPIO_PIN_15
#define DIO_FU6_GPIO_Port GPIOG
#define PWM_RGB_STRIP_RED_Pin GPIO_PIN_3
#define PWM_RGB_STRIP_RED_GPIO_Port GPIOB
#define DO_PWR_EN_5VSENS_Pin GPIO_PIN_4
#define DO_PWR_EN_5VSENS_GPIO_Port GPIOB
#define DO_LED_EXTRA1_Pin GPIO_PIN_5
#define DO_LED_EXTRA1_GPIO_Port GPIOB
#define DO_A_MUX_SEL_Pin GPIO_PIN_6
#define DO_A_MUX_SEL_GPIO_Port GPIOB
#define DI_BTN_2_NC_Pin GPIO_PIN_7
#define DI_BTN_2_NC_GPIO_Port GPIOB
#define DI_BTN_2_NO_Pin GPIO_PIN_8
#define DI_BTN_2_NO_GPIO_Port GPIOB
#define DI_BTN_1_NC_Pin GPIO_PIN_9
#define DI_BTN_1_NC_GPIO_Port GPIOB
#define DI_BTN_1_NO_Pin GPIO_PIN_0
#define DI_BTN_1_NO_GPIO_Port GPIOE
#define XI_PWR_FAULT_24V_Pin GPIO_PIN_1
#define XI_PWR_FAULT_24V_GPIO_Port GPIOE
#define XI_PWR_FAULT_24V_EXTI_IRQn EXTI1_IRQn
void   MX_IWDG_Init(void);
/* USER CODE BEGIN Private defines */

/* USER CODE END Private defines */

#ifdef __cplusplus
}
#endif

#endif /* __MAIN_H */
