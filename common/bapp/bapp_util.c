/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stddef.h>

#include "bapp/bapp.h"
#include "plf/plf.h"

#include "hal/hal.h"
#include "hal/dio/dio.h"
#include "hal/pflash/pflash.h"
#include "hal/dflash/dflash.h"
#include "hal/fram/fram.h"
#include "hal/cfg/fram_cfg.h"

#include "hal/mcupin/mcupin.h"
#include "hal/uart/uart.h"
#include "hal/wdt/wdt.h"

#include "plf/sio/sio.h"
#include "plf/blc/blc.h"
#include "plf/blc/blc_def.h"
#include "plf/ver/ver.h"
#include "plf/crc/crc16.h"

#include "net/xmodem/xmodem.h"


/******************************************************************************/

int8_t FirmwareCheck(void)
{
	if (strcmp(pFWDescriptor->id, BOOTCTRL_HEADER_ID) != 0)
		return FIRMWARE_STATUS_BAD_ID;
  
  if (pFWDescriptor->crcGuard != RUN_FROM_IDE) {
    if (pFWDescriptor->crcGuard == RUN_FROM_PATCHED_FILE) {
      Crc16_t crc = CalcCrc16(APP_PROGRAM_FLASH_SIZE - BLC_FW_DESCIPTOR_SIZE, (void *)APP_PROGRAM_FLASH_START, CRC16_INITIAL_SEED);
      if (pFWDescriptor->crc != crc) {
        return FIRMWARE_STATUS_BAD_CRC;
      }
    } else {
      return FIRMWARE_STATUS_BAD_CRC;
    }
  }
  return FIRMWARE_STATUS_OK;
}


#if defined(USE_FWD_BC_BOOT_MENU)

/******************************************************************************/

char 				buf[XMODEM_BUF_SIZE];

/******************************************************************************/

extern void boot_jump( uint32_t address );

bool ExecuteFirmware(uint32_t address)
{

	if (FirmwareCheck() == FIRMWARE_STATUS_OK) { // OK Appl available	
		
		pBootControl->app = FWD_LAST_APP_BOOTLOADER;
		pBootControl->cmd	= FWD_BC_REBOOT;
		pBootControl->crc = CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED); 	
#if defined(BOOT_CONTROL_IN_FRAM)
    HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif

		SifWriteLn("Booting Application\r\n");
		while (SioTxBusy()) ;  // wait until sio buffer is written...

		HalInterruptDisable(NULL);
		HAL_DeInit();
		
		if (pFWDescriptor->wdtEnable) { // Start Watch Dog
      HalWdtActivate();
//			MX_IWDG_Init();
//			HAL_IWDG_Refresh(&hiwdg);
		}

		boot_jump(address);
	}
	
  SifWriteLn("Application Wrong!\r\n");
	return false;
}

/******************************************************************************/

#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE

#if defined(STM32F767xx)
#include "stm32f7xx_hal_flash_ex.h"
#else
#error "FIX"
#endif

#pragma pack(push, 4)
// Data on an even address!
static uint32_t xx[FLASH_PAGE_SIZE/4];
#pragma pack(pop)


//extern FLASH_ProcessTypeDef pFlash;
//extern void FLASH_PageErase(uint32_t PageAddress);

//HAL_StatusTypeDef Erase(uint32_t address)
//{
//  HAL_StatusTypeDef status = HAL_ERROR;
//  
//	HAL_FLASH_Unlock();

//  /* Process Locked */
//  __HAL_LOCK(&pFlash);

//  /* Wait for last operation to be completed */
//  status = FLASH_WaitForLastOperation(FLASH_TIMEOUT_VALUE);

//  if (status == HAL_OK) {

//    FLASH_PageErase(address);

//    /* Wait for last operation to be completed */
//    status = FLASH_WaitForLastOperation(FLASH_TIMEOUT_VALUE);

//    /* If the erase operation is completed, disable the ERASE Bit */
//    CLEAR_BIT(FLASH->PECR, FLASH_PECR_PROG);
//    CLEAR_BIT(FLASH->PECR, FLASH_PECR_ERASE);

//  }

//  /* Process Unlocked */
//  __HAL_UNLOCK(&pFlash);

//	HAL_FLASH_Lock();
//  return status;
//}
#endif

/******************************************************************************/

#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
bool loadToPFlash = false;
#endif

#if defined(USE_FWD_BC_SAVE_GOLDEN_IMAGE) || defined(SUPPORT_DFU_VIA_DFLASH) || defined(USE_FWD_BC_BOOT_MENU)

uint32_t xOffset;

#if defined(BLC_USE_DFLASH)

int XmodemCallback(unsigned char *buf, int cnt)
{
#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
  if (loadToPFlash) {
    if (cnt != FLASH_PAGE_SIZE) for (;;) ;
    Erase(xOffset);
    memcpy(xx, buf, cnt);
    if (!HalPFlashWrite((uint8_t *)xOffset, (uint8_t *)xx, cnt)) {
      return -1;
    }
    xOffset += cnt;
  } else {
#endif    
    while (cnt) {
      uint32_t size = MIN_VAL(cnt, DFLASH_PP_SIZE);

      if (BlcWriteToDataFlash(xOffset, buf, size) != 0) {
        return -1;
      }
      buf += size;
      xOffset += size;
      cnt -= size;
    }
#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
  }
#endif
  return 0;
}

void SaveNewImage(void)
{
  SifWrite("Saving New Image (backup): ");
  while (SioTxBusy()) {}
  if (BlcCopyFirmwareToDataFlash(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE) == 0)
    SifWriteLn("ok");
  else
    SifWriteLn("failed");
}

#endif
#endif

/******************************************************************************/
#ifdef USE_FWD_BC_MAKE_DFU_GOLD

void SaveGoldenImage(void)
{
  SifWrite("Saving Golden Image (backup): ");
  while (SioTxBusy()) {}
  if (BlcCopyFirmwareToDataFlash(DFLASH_IMAGE_GOLDEN_START) == 0)
    SifWriteLn("ok");
  else
    SifWriteLn("failed");
}

void RestoreGoldenImage(void)
{
  SifWriteLn("Restore Golden Image: ");
  while (SioTxBusy()) {}
  if (BlcCopyDataFlashToFirmware(DFLASH_IMAGE_GOLDEN_START) == 0)
    SifWriteLn("ok");
  else
    SifWriteLn("failed");
}

#endif
/******************************************************************************/

#if defined(BLC_USE_DFLASH)

bool Download(void)
{
  int32_t cnt;
#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
  loadToPFlash = false;
#endif            
  SifWrite("Erasing Data Flash: ");
  HalDFlashEraseAddrRange(DFLASH_DFU_START, DFLASH_DFU_START+APP_PROGRAM_FLASH_SIZE-1);
  
  SifWrite("\r\nXmodem transfer bin Image: ");
  xOffset = DFLASH_DFU_START;
  if ((cnt = XmodemReceive(XmodemCallback)) > 0) {
    SifWrite(" - done, received: ");
    SifWrite(Dec2Str(cnt, &buf[sizeof(buf)]));
    SifWrite(" byte (");
    SifWrite(Hex2Str(cnt, 2*sizeof(cnt), &buf[sizeof(buf)]));
    SifWriteLn(")");
    return true;
  } else {
    SifWriteLn("failed!");
    return false;
  }
}

// Dont return if download succeeded!
bool DownloadAndActivate(void)
{
  if (Download()) {
    
    BlcCopyDataFlashToFirmware(DFLASH_DFU_START);
    
    switch (FirmwareCheck()) {
      case FIRMWARE_STATUS_OK:
        ExecuteFirmware(APP_PROGRAM_FLASH_START);
        return true;
        //break;
      case FIRMWARE_STATUS_BAD_ID:
        SifWriteLn("failed - bad ID");
        break;
      case FIRMWARE_STATUS_BAD_CRC:
        SifWriteLn("failed - bad crc");
        break;
    }
  } 
  return false;
}

#endif

/******************************************************************************/

#ifdef SUPPORT_DFU_VIA_DFLASH

bool DoPFlashProgram(void)
{
  return BlcCopyDataFlashToFirmware(DFLASH_DFU_START) == 0;
} 

#endif

/******************************************************************************/

#if defined(SUPPORT_DFU_VIA_DFLASH) || defined(USE_FWD_BC_MAKE_DFU_GOLD)
void ExecuteNew(void)
{
  switch (FirmwareCheck()) {
    case FIRMWARE_STATUS_OK:
      SifWriteLn("Starting Application");
      ExecuteFirmware(APP_PROGRAM_FLASH_START);
      break;
    case FIRMWARE_STATUS_BAD_ID:
      SifWriteLn("failed - bad cookie");
      break;
    case FIRMWARE_STATUS_BAD_CRC:
      SifWriteLn("failed - bad crc");
      break;
  }
}
#endif

/******************************************************************************/

#endif
