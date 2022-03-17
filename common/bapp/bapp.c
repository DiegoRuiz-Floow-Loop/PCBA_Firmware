/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stddef.h>
#include <string.h>
#include <ctype.h>

#include "hal/hal.h"
#include "hal/pflash/pflash.h"
#include "hal/wdt/wdt.h"
#include "hal/dio/dio.h"

#include "hal/fram/fram.h"
#include "hal/cfg/fram_cfg.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"
#include "plf/blc/blc.h"
#include "plf/sio/sio.h"
#include "plf/ver/ver.h"
#include "plf/led/led.h"
#include "plf/rle/dfx.h"

#include "bapp/bapp.h"

/******************************************************************************/

static EvosEventHandle_t appTask;
uint32_t dot = 0;

static void AppTask(EvosEventParam_t p)
{
  SifWriteCh('.');
  dot++;
  if(dot >= 15){
    if(FirmwareCheck() == FIRMWARE_STATUS_OK)
      BAppExecuteFirmware();
    else
      dot = 0;
  }
  EvosEventSetDelta(appTask, 1000, 0);
}

/******************************************************************************/

extern void boot_jump(uint32_t address );

void BAppExecuteFirmware(void)
{
  // Init BC record
  pBootControl->crc = CALC_CRC16(sizeof(BootControl_t)-sizeof(Crc16_t), (void *)pBootControl, CRC16_INITIAL_SEED);
#if defined(BOOT_CONTROL_IN_FRAM)
  HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif

  // Empty SIO out-queue
  //while (SioTxBusy()) ;
  HAL_Delay(100);
  
  // Disable hardware
  HalInterruptDisable(NULL);
  HAL_DeInit();  
  
  // Activate WDT ?
  //FWDescriptor_t * xx = (FWDescriptor_t * )0x080FFFC0;
  if (pFWDescriptor->wdtEnable)
    HalWdtActivate();

  // Jump to application
  boot_jump(APP_PROGRAM_FLASH_START);
}

/*******************************************************************************/

void BAppGotSioCommand(char * cmd)
{
  unsigned int i;
  for (i=0; i<strlen(cmd); i++)
    cmd[i] = tolower(cmd[i]);
  
  dot = 0;
  EvosEventSetDelta(appTask, 1000, 0);

  if (strcmp(cmd, "reboot") == 0) {
    if (FirmwareCheck() == FIRMWARE_STATUS_OK)  { // OK Appl available
      SifWriteLn("Booting application");
      BAppExecuteFirmware();
    } else {
      SifWriteLn("No bootable application available");
    }
  } else {
    SifWrite("Got not supported command: ");
    SifWriteLn(cmd);
  }
}

/******************************************************************************/

static const char bootText[] =
  "\r\n\n" PLF_PRODUCT_NAME " Bootloader"
  " (ver:" _STRIZE(PLF_VERSION_MAJOR) "." _STRIZE(PLF_VERSION_MINOR) ")";

void BAPP_Init(void)
{
	LedFlash(LED_HEARTBEAT, 100, 100);
  
  appTask = EvosEventRegister(AppTask, "AppTask");
  EvosEventSet(appTask, 1000, 0);

	
  //SifWrite(bootText);	
  //SifWriteCh(PLF_VERSION_TYPE);	
  SifWriteLn(bootText);	
 // SioPrintLn();	
  
  /* Iwatchdog reset flags */
  if (pBootControl->rccCsr & RCC_CSR_IWDGRSTF) { /* Iwatchdog reset flags */
		static const char wdtTxt[] = "WDT reset...!!";
		SifWriteLn(wdtTxt);
		// ???
	}
  
  Crc16_t crc;
	crc = CALC_CRC16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);
	if (crc == pBootControl->crc) {  // crc ok => reboot from appl/bootloader
		if (pBootControl->app == FWD_LAST_APP_APPLICATION) {				
			static const char txt[] = "WORM start from APPL";
			SifWriteLn(txt);
			if (pBootControl->cmd == FWD_BC_REBOOT) {
				BAppExecuteFirmware();
#if defined(USE_FWD_BC_MAKE_DFX_NEW)        
			} else if (pBootControl->cmd == FWD_BC_MAKE_DFX_NEW) {
        static const char txt[] = "Activating New DFX Firmware...";
        SifWriteLn(txt);
        CpxUnpackToPFlash(APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_SIZE, pBootControl->adr);
#endif        
			} else if (pBootControl->cmd == FWD_BC_MAKE_DFU_NEW) {
        static const char txt[] = "Activating New DFU Firmware...";
        SifWriteLn(txt);
#if defined(BLC_USE_DFLASH)
        if (BlcCopyDataFlashToFirmware(pBootControl->adr) != 0) {
          static const char txt[] = "Activating DFU Firmware - failed !?";
          SifWriteLn(txt);
        }
#else
        BlcCopyProgramFlashToFirmware(pBootControl->adr);
#endif        
#if defined(USE_FWD_BC_BOOT_MENU)        
			} else if (pBootControl->cmd == FWD_BC_BOOT_MENU) {
				BAppMenu();		
#endif        
			} else {  // ??
				BAppExecuteFirmware();
      }
		}  else  {
		static const char txt[] = "WORM start from BTL";
			SifWriteLn(txt);
			if (pBootControl->cmd == FWD_BC_BOOT_TO_APPL) {
				BAppExecuteFirmware();
			}
		}    
	} else {
		static const char txt[] = "COLD start";
		SifWriteLn(txt);
		pBootControl->app        = FWD_LAST_APP_BOOTLOADER;
		pBootControl->cmd        = FWD_BC_NONE;
		pBootControl->crc        = CALC_CRC16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);      
#if defined(BOOT_CONTROL_IN_FRAM)
    HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif
	}
  int8_t fc = FirmwareCheck();
	if (fc == FIRMWARE_STATUS_OK) {
		BAppExecuteFirmware(); // dont return!
  } else {
    LongBuf_t buffer;
    static const char txt[] = "Firmware NOT OK: ";
    SifWrite(txt);
    // strLen == 0 no leading '0', else '0' is added excl. '-'
    //extern char * DecToNum(LongBuf_t buffer, int32_t val, uint8_t strLen);    
    SifWriteLn(DecToNum(buffer, fc, 0));
	}

 
}

/******************************************************************************/
