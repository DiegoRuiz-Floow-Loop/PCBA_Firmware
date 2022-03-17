/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "stm32f7xx_hal.h"
#include "stm32f7xx_hal_flash_ex.h"

#include "hal/pflash/pflash.h"
#include "plf/trc/trc.h"



/****************************************************************************/
bool HalPFlashRead(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  memcpy((void *)ramAddr, (void *)flashAddr, len);
  return true;
}

/****************************************************************************/

bool HalPFlashWrite(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  uint16_t * data = (uint16_t *)ramAddr;
	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
  }
  
  do {
    uint16_t d = *data++;
    if (*(uint16_t *)flashAddr == 0xFFFF) { // STM32F can not re-write (?)
      if (d != 0xFFFF) {
        // FLASH_TYPEPROGRAM_HALFWORD == 16 bit
        if (HAL_FLASH_Program(FLASH_TYPEPROGRAM_HALFWORD, flashAddr, d) != HAL_OK) {
          HAL_FLASH_Lock();
          TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Program() failed");
          return false;
        }
      }
    }
    flashAddr += 2;
    len -= 2;
	} while (len > 0);

	HAL_FLASH_Lock();
  return true;
}

/****************************************************************************/

#define FSIZE (FLASH_END - FLASH_BASE + 1)

#if (FSIZE == 0x00100000) // 1MB

//static uint32_t const PFlashSectorSize[] = {
//	32*1024,  
//	32*1024,
//	32*1024,
//	32*1024,
//	128*1024,
//	256*1024,
//	256*1024,
//	256*1024,
//};

#elif (FSIZE == 0x00200000)  // 2MB

#if 1 /* SINGLE BANK */
static uint32_t const PFlashSectorSize[] = {
	32*1024,  
	32*1024,
	32*1024,
	32*1024,
	128*1024,
	256*1024,
	256*1024,
	256*1024,

	256*1024,
	256*1024,
	256*1024,
	256*1024,
};

#else /* DUALBANK */

static uint32_t const PFlashSectorSize[] = {
	16*1024,  
	16*1024,
	16*1024,
	16*1024,
	64*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,

	16*1024,  
	16*1024,
	16*1024,
	16*1024,
	64*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
	128*1024,
};
#endif

#else
#error "check"
#endif

#define PFLASH_SECTORS		(sizeof(PFlashSectorSize)/sizeof(PFlashSectorSize[0]))

/****************************************************************************/

//extern FLASH_ProcessTypeDef pFlash;
//#define FLASH_TIMEOUT_VALUE       50000U/* 50 s */

//static HAL_StatusTypeDef MyHAL_FLASHEx_Erase(FLASH_EraseInitTypeDef *pEraseInit, uint32_t *SectorError)
//{
//  HAL_StatusTypeDef status = HAL_ERROR;
//  uint32_t index = 0;
//  
//  /* Process Locked */
//  __HAL_LOCK(&pFlash);

//  /* Check the parameters */
//  assert_param(IS_FLASH_TYPEERASE(pEraseInit->TypeErase));

//  /* Wait for last operation to be completed */
//  status = FLASH_WaitForLastOperation((uint32_t)FLASH_TIMEOUT_VALUE);

//  if(status == HAL_OK)
//  {
//    /*Initialization of SectorError variable*/
//    *SectorError = 0xFFFFFFFFU;
//    
//    if(pEraseInit->TypeErase == FLASH_TYPEERASE_MASSERASE)
//    {
//      status = HAL_ERROR;
//    }
//    else
//    {
//      /* Check the parameters */
//      assert_param(IS_FLASH_NBSECTORS(pEraseInit->NbSectors + pEraseInit->Sector));

//      /* Erase by sector by sector to be done*/
//      for(index = pEraseInit->Sector; index < (pEraseInit->NbSectors + pEraseInit->Sector); index++)
//      {
//        FLASH_Erase_Sector(index, (uint8_t) pEraseInit->VoltageRange);
//        PlfDelay(1);

//        /* Wait for last operation to be completed */
//        status = FLASH_WaitForLastOperation((uint32_t)FLASH_TIMEOUT_VALUE);
//        
//        /* If the erase operation is completed, disable the SER Bit and SNB Bits */
//        CLEAR_BIT(FLASH->CR, (FLASH_CR_SER | FLASH_CR_SNB)); 

//        if(status != HAL_OK) 
//        {
//          /* In case of error, stop erase procedure and return the faulty sector*/
//          *SectorError = index;
//          break;
//        }
//      }
//    }
//  }

//  /* Process Unlocked */
//  __HAL_UNLOCK(&pFlash);

//  return status;
//}

bool HalPFlashEraseAddrRange(uint32_t firstAddr, uint32_t lastAddr)
{
  FLASH_EraseInitTypeDef eraseInit;
  HAL_StatusTypeDef stat;
  uint32_t pageError;
	int i, firstSector, lastSector;
	uint32_t a;
	
	a = FLASH_BASE;
	for (i=0; i<PFLASH_SECTORS; i++) {
		a += PFlashSectorSize[i];
		if (a <= firstAddr)
			firstSector = i;
		if (a <= lastAddr)
			lastSector = i;
	}
	firstSector++;
	lastSector++;
		
	memset(&eraseInit, 0, sizeof(eraseInit));
  eraseInit.TypeErase 	= FLASH_TYPEERASE_SECTORS; //FLASH_TYPEERASE_PAGES;
	eraseInit.Sector 			= firstSector;
	eraseInit.NbSectors 	= lastSector-firstSector+1;
	eraseInit.VoltageRange = FLASH_VOLTAGE_RANGE_3;

	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
    return false;
  }  
  
  stat = HAL_FLASHEx_Erase(&eraseInit, &pageError);
  if (stat != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASHEx_Erase() failed");
    return false;
  }

	HAL_FLASH_Lock();
  return true;
}


bool HalPFlashEraseApplicatioFirmware(void)
{
	return HalPFlashEraseAddrRange(APP_PROGRAM_FLASH_START, _FLASH_END-1);
}

/****************************************************************************/

void HalPFlashInit(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_COMPONENT, "HalPFlashInit()");
	HAL_FLASH_Lock();
}

/****************************************************************************/
