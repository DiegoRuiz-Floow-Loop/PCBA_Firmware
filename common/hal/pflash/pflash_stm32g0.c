/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "stm32g0xx_hal.h"
#include "stm32g0xx_hal_flash_ex.h"

#include "hal/pflash/pflash.h"

#include "plf/trc/trc.h"

/****************************************************************************/

bool HalPFlashRead(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  memcpy((void *)ramAddr, (void *)flashAddr, len);
  return true;
}

/****************************************************************************/

bool HalPFlashWrite_64b(uint32_t flashAddr,  uint64_t data)
{
  HalIrqStat_t is;
  HalInterruptDisable(&is);
	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
  }

  if (*(uint64_t *)flashAddr == 0xFFFFFFFFFFFFFFFFULL) { // STM32G0 can not re-write (?)
    if (data != 0xFFFFFFFFFFFFFFFFULL) {
      // FLASH_TYPEPROGRAM_DOUBLEWORD == 64 bit
      if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_DOUBLEWORD, flashAddr, data) != HAL_OK) {
        HAL_FLASH_Lock();
        HalInterruptRestore(is);
        TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
        return false;
      }
    }
  }
	HAL_FLASH_Lock();
  HalInterruptRestore(is);
  return true;
}

/****************************************************************************/

bool HalPFlashEraseAddrRange(
  uint32_t    address,
  uint32_t    endAddress)
{
  FLASH_EraseInitTypeDef eraseInit;
  HAL_StatusTypeDef stat;
  uint32_t pageError;

  eraseInit.TypeErase = FLASH_TYPEERASE_PAGES;
  eraseInit.NbPages = ((endAddress+1)-address)/FLASH_PAGE_SIZE; 
  eraseInit.Page    = ((address - FLASH_BASE)/FLASH_PAGE_SIZE);

  TRACE_VA(TRC_TA_HAL, TRC_TL_5, "HalPFlashErase(0x%08X, 0x%08X)", address, endAddress);

	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
    return false;
  }  
  
  stat = HAL_FLASHEx_Erase(&eraseInit, &pageError);

	HAL_FLASH_Lock();
  return (stat == HAL_OK);
}

/****************************************************************************/

void HalPFlashInit(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_2, "HalPFlashInit()");
	HAL_FLASH_Lock();
}

/****************************************************************************/
