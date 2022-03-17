/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "stm32f0xx_hal.h"
//#include "stm32f0xx_hal_flash.h"
#include "stm32f0xx_hal_flash_ex.h"

#include "hal/pflash/pflash.h"

#include "plf/trc/trc.h"

/****************************************************************************/

bool_t HalPFlashRead(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  memcpy((void *)ramAddr, (void *)flashAddr, len);
  return true;
}

/****************************************************************************/

bool_t HalPFlashWrite(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  //uint16_t * data = (uint16_t *)ramAddr;
	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
  }
  
    
#if defined(FLASH_TYPEPROGRAM_DOUBLEWORD)
  while (len >= 8) {
    uint64_t d = *(uint64_t *)ramAddr;
    if (*(uint64_t *)flashAddr == 0xFFFFFFFFFFFFFFFFULL) { 
      if (d != 0xFFFFFFFFFFFFFFFFULL) {
        if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_DOUBLEWORD, flashAddr, d) != HAL_OK) {
          HAL_FLASH_Lock();
          TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
          return FALSE;
        }
      }
    }
    ramAddr += 8;
    flashAddr += 8;
    len -= 8;
   }
#endif

    
#if defined(FLASH_TYPEPROGRAM_WORD)
  while (len >= 4) {
    uint32_t d = *(uint32_t *)ramAddr;
    if (*(uint64_t *)flashAddr == 0xFFFFFFFFUL) { 
      if (d != 0xFFFFFFFFUL) {
        if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_WORD, flashAddr, d) != HAL_OK) {
          HAL_FLASH_Lock();
          TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
          return FALSE;
        }
      }
    }
    ramAddr += 4;
    flashAddr += 4;
    len -= 4;
  }
#endif
    
    
  while (len > 0) {
    uint16_t d = *(uint16_t *)ramAddr;
    if (*(uint16_t *)flashAddr == 0xFFFF) { // STM32F1 can not re-write (?)
      if (d != 0xFFFF) {
        if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_HALFWORD, flashAddr, d) != HAL_OK) {
          HAL_FLASH_Lock();
          TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
          return FALSE;
        }
      }
    }
    ramAddr += 2;
    flashAddr += 2;
    len -= 2;
  }

	HAL_FLASH_Lock();
  return TRUE;
}

/****************************************************************************/

bool_t HalPFlashErase(
  uint32_t    address,
  uint32_t    endAddress)
{
  FLASH_EraseInitTypeDef eraseInit;
  HAL_StatusTypeDef stat;
  uint32_t pageError;

  TRACE_VA(TRC_TA_HAL, TRC_TL_5, "HalPFlashErase(0x%08X, 0x%08X)", address, endAddress );
  eraseInit.TypeErase = FLASH_TYPEERASE_PAGES;
  eraseInit.NbPages = ((endAddress+1)-address)/FLASH_PAGE_SIZE; 
  eraseInit.PageAddress = address;

  TRACE_VA(TRC_TA_HAL, TRC_TL_5, "HalPFlashErase(0x%08X, 0x%08X), pages_%u", address, endAddress, eraseInit.NbPages);

	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
    return FALSE;
  }  
  
  HAL_FLASH_Unlock();
  stat = HAL_FLASHEx_Erase(&eraseInit, &pageError);
  if (stat != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASHEx_Erase() failed");
    return FALSE;
  }

	HAL_FLASH_Lock();
  return TRUE;
}

/****************************************************************************/

void HalPFlashInit(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_2, "HalPFlashInit()");
	HAL_FLASH_Lock();
}

/****************************************************************************/
