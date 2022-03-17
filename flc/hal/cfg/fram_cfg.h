/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_FRAM_CFG_H
#define HAL_FRAM_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*******************************************************************************/

#if !defined(BOOTLOADER)

#include "stm32l4xx_hal.h"

#if defined(EVK)
extern I2C_HandleTypeDef hi2c1;
#define FRAM_I2C         &hi2c1

#else
extern SPI_HandleTypeDef hspi1;
#define SPI_FRAM         hspi1

#endif



#endif

/*******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* HAL_ADC_CFG_H */
