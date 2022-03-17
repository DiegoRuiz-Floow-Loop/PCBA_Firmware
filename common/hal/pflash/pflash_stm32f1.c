/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "stm32f1xx_hal.h"
//#include "stm32f1xx_hal_flash.h"
//#include "stm32f1xx_hal_flash_ex.h"

#include "hal/pflash/pflash.h"

#include "plf/trc/trc.h"

/****************************************************************************/

//#define HAL_PFLASH_START		  (0x08000000ul)	
//#define HAL_PFLASH_SIZE		    (128*1024ul)  // 128 KB
//#define HAL_PFLASH_PAGES	  (HAL_PFLASH_SIZE / FLASH_PAGE_SIZE)  // 128

/****************************************************************************/

bool_t HalPFlashRead(uint32_t pflashAddr,  uint32_t ramAddr, int32_t len)
{
  memcpy((void *)ramAddr, (void *)pflashAddr, len);
  return true;
}

/****************************************************************************/

bool_t HalPFlashWrite(uint32_t flsahAddr, uint32_t ramAddr, int32_t len)
{
	uint16_t * data = (uint16_t*)ramAddr;

	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_CAN, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
  }
  
  do {
    uint16_t d = *data++;
    if (*(uint16_t *)flsahAddr == 0xFFFF) { // STM32F1 can not re-write (?)
      if (d != 0xFFFF) {
        // FLASH_TYPEPROGRAM_HALFWORD == 16 bit
        if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_HALFWORD, flsahAddr, d) != HAL_OK) {
          HAL_FLASH_Lock();
          TRACE(TRC_TA_CAN, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
          return FALSE;
        }
      }
    }
    flsahAddr += 2;
    len -= 2;
	} while (len > 0);

	HAL_FLASH_Lock();
  return TRUE;
}

/****************************************************************************/

void HalPFlashErase(
  uint32_t    address,
  uint32_t    endAddress)
{
  FLASH_EraseInitTypeDef eit;
  uint32_t pageError;
	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_CAN, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
  }
  
  eit.TypeErase   = FLASH_TYPEERASE_PAGES;
  eit.Banks       = FLASH_BANK_1;
  eit.PageAddress = address;
  eit.NbPages     = ((endAddress+1)-address)/FLASH_PAGE_SIZE; 
  
  if (HAL_FLASHEx_Erase(&eit, &pageError) != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASHEx_Erase() failed");
  }
  
//  do {
//    FLASH_PageErase(address);
//    if (FLASH_WaitForLastOperation(1000) != HAL_OK) {
//      TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HalPFlashErase() failed");
//    }
//    address += FLASH_PAGE_SIZE;
  
//  } while (address < endAddress);
	HAL_FLASH_Lock();
}

/****************************************************************************/

void HalPFlashInit(void)
{
	HAL_FLASH_Lock();
}

/****************************************************************************/
