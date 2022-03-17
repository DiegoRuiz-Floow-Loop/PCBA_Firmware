/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/pflash/pflash.h"

#if defined(STM32L073xx)  || defined(STM32L071xx)
  #include "stm32l0xx_hal.h"
  #include "stm32l0xx_hal_flash.h"
  #include "stm32l0xx_hal_flash_ex.h"
#else
  #error
#endif


/* FLASH Memory Programming functions *****************************************/   

bool_t HalPFlashRead(uint32_t pflashAddr,  uint32_t ramAddr, uint32_t len)
{
  memcpy((void *)ramAddr, (void *)pflashAddr, len);
  return TRUE;    
}

/****************************************************************************/

bool_t HalPFlashEraseAddrRange(
  uint32_t              flashAddr,
  uint32_t              flashAddrLast)
{
  HAL_StatusTypeDef st;
  FLASH_EraseInitTypeDef fe;
  uint32_t pageError;
  uint32_t range = (flashAddrLast - flashAddr + 1);

  fe.TypeErase    = FLASH_TYPEERASE_PAGES;
  fe.PageAddress  = flashAddr;
  fe.NbPages      = range / FLASH_PAGE_SIZE;  /* 128 byte / page */
  
  CLEAR_BIT(FLASH->PECR, FLASH_PECR_PRGLOCK);
  

  
  __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_EOP | FLASH_FLAG_PGAERR | FLASH_FLAG_SIZERR | FLASH_FLAG_OPTVERR | 
    FLASH_FLAG_RDERR | FLASH_FLAG_WRPERR | FLASH_FLAG_FWWERR | FLASH_FLAG_NOTZEROERR);

  st = HAL_FLASH_Unlock();
	if (st != HAL_OK) {
    return FALSE;
  }

	st = HAL_FLASHEx_Erase(&fe, &pageError);
	HAL_FLASH_Lock();
	
	return st == HAL_OK;
}



/****************************************************************************/


bool_t HalPFlashWrite(uint32_t flsahAddr, uint32_t ramAddr, uint32_t len)
{
  HAL_StatusTypeDef st;
	uint32_t * data = (uint32_t*)ramAddr;
	uint32_t i;

	if (((flsahAddr>>2)<<2) != flsahAddr) return FALSE;
	if (((ramAddr>>1)<<1) != ramAddr) return FALSE;

	HAL_FLASH_Unlock();
	__HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);    

	for (i=0; i<len; i += sizeof(uint32_t)) {
    do {
		  st = HAL_FLASH_Program(FLASH_TYPEPROGRAM_WORD, i+flsahAddr, *data);
    } while (st == HAL_BUSY);
    if (st != HAL_OK) {
			HAL_FLASH_Lock();
  		return FALSE;
		}
		data++;
	}

	HAL_FLASH_Lock();
	return TRUE;
}

/****************************************************************************/

void HalPFlashInit(void)
{
    // nop
}
