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
#include "hal/mcupin/mcupin.h"
#include "hal/uart/uart.h"
#if defined(ETA)
#include "hal/fram/fram.h"
#endif

#include "plf/sio/sio.h"
#include "plf/blc/blc.h"
#include "plf/ver/ver.h"
#include "plf/crc/crc16.h"

#if defined(BLC_USE_DFLASH)
  #include "hal/dflash/dflash.h"
#else
  #include "hal/pflash/pflash.h"
#endif        

#include "net/xmodem/xmodem.h"

#if defined(USE_FWD_BC_BOOT_MENU)

/******************************************************************************/

#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
	extern bool loadToPFlash;
#endif

#if defined(USE_FWD_BC_SAVE_GOLDEN_IMAGE) || defined(SUPPORT_DFU_VIA_DFLASH)
  extern uint32_t xOffset;
	
	extern int XmodemCallback(unsigned char *buf, int cnt);

#endif
/******************************************************************************/

static const char helpText[] = 
  "COMMON:\r\n"
  " c - Check current firmware\r\n"
  " v - Show System info\r\n"
  " e - Erase entire PROGRAM flash\r\n"
//  " f - Erase entire DATA flash\r\n"
  " x - Exit menu & run firmware\r\n"
#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
  "DFU direct\r\n"
  " l - Load new firmware via XMODEM\r\n"
#endif
#ifdef SUPPORT_DFU_VIA_DFLASH
"DFU via Data Flash:\r\n"
  " a - Download and activate new firmware\r\n"
  " d - Download new firmware\r\n"
  " p - Program new firmware\r\n"
//  " n - Save current firmware as NEW\r\n"
  " 1 - Verify New Image\r\n"
#endif
#ifdef USE_FWD_BC_MAKE_DFU_GOLD
"\"GOLDEN\" firmware:\r\n" 
  " b - Save current firmware as GOLDEN\r\n"
  " r - Restore GOLDEN Image\r\n"
  " 2 - Verify Golden Image\r\n"
#endif  
  "";
  

/******************************************************************************/

static void PrintMenu(void)
{
  SifWrite("\r\n");
  SifWriteLn(applicationString);

  SifWriteLn(helpText);
}

/******************************************************************************/

#if defined(SUPPORT_DFU_VIA_DFLASH) || defined(USE_FWD_BC_MAKE_DFU_GOLD)

static int_fast8_t VerifyFirmware(uint32_t dataFlashAddr)
{
	uint32_t progFlashAddr = APP_PROGRAM_FLASH_START;  /* desination */
	uint8_t buffer[XFLASH_BUFFER];
  uint16_t cnt = 0;
	int i;
	
	for (;;) {
    if (cnt++ == 0) {
      ; //SifWriteCh('.');
    } else {
      if (cnt >= 0x100) {
        cnt = 0;
      }
    }
    
    // Read data from data flash
#if defined(BLC_USE_DFLASH)
    if (HalDFlashRead(dataFlashAddr, buffer, XFLASH_BUFFER) != 0) {
#else
    if (!HalPFlashRead(dataFlashAddr, (uint32_t)buffer, XFLASH_BUFFER)) {
#endif        
      return -1;
    }
    
    i = memcmp(buffer, (void *)progFlashAddr, XFLASH_BUFFER);
    if (i != 0) {
      return-1;
    }
    
    dataFlashAddr += XFLASH_BUFFER;
    progFlashAddr += XFLASH_BUFFER;
    if (progFlashAddr >= APP_PROGRAM_FLASH_END)
      break;
  }
	return 0;
}
#endif

/******************************************************************************/

#define INACTIVITY_TIMEOUT  (30)   // sec
#define INACTIVITY_STEP     (250)     // msec

#define ACTIVITY_COUNT_MAX ((INACTIVITY_TIMEOUT*1000) / INACTIVITY_STEP)

void BAppMenu(void)
{  
	uint32_t tickstart, lastStep;
  char buf[10];
	
  PrintMenu();
	tickstart = HAL_GetTick();
  lastStep = 0;
  for (;;) {
    int_fast16_t cmd = SioGetChar();
    if (cmd < 0) {  // no character
      int step = TIME_DIF(HAL_GetTick(), tickstart) / INACTIVITY_STEP; 
			if (step > ACTIVITY_COUNT_MAX) { 
        SifWriteLn("restarting while inactivity");
        while (SioTxBusy()) ;  // wait until sio buffer is written...
        pBootControl->app = FWD_LAST_APP_BOOTLOADER;
        pBootControl->cmd = FWD_BC_BOOT_TO_APPL;
        pBootControl->crc = CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);      
#if defined(BOOT_CONTROL_IN_FRAM)
        HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif
        NVIC_SystemReset();
			} else if (step != lastStep) {
        char buf[10];
        int c = ACTIVITY_COUNT_MAX - step; 
        lastStep = step;
        SifWriteCh('\r');        
        SifWrite(Dec2Str(c * INACTIVITY_STEP / 1000, &buf[sizeof(buf)])); 
        SifWrite(" > ");        

//        if (step & 1) {
//            LL_GPIO_ResetOutputPin(DO_LED_HUB_R_GPIO_Port, DO_LED_HUB_R_Pin);
//            LL_GPIO_SetOutputPin(DO_LED_PAY_R_GPIO_Port, DO_LED_PAY_R_Pin);
//        } else {
//            LL_GPIO_SetOutputPin(DO_LED_HUB_R_GPIO_Port, DO_LED_HUB_R_Pin);
//            LL_GPIO_ResetOutputPin(DO_LED_PAY_R_GPIO_Port, DO_LED_PAY_R_Pin);
//          }
//        }

        
      }
		} else {
#if defined(BLC_USE_DFLASH)
      FWDescriptor_t fwd;
#endif
      SioPrintLn();
      switch (cmd) {

        /* COMMON */
        case 'c':
          SifWrite("Firmware check: ");
          switch (FirmwareCheck()) {
            case FIRMWARE_STATUS_OK:  
              SifWriteLn("ok");
              break;
            case FIRMWARE_STATUS_BAD_ID:
              SifWriteLn("failed - bad identity");
              break;
            case FIRMWARE_STATUS_BAD_CRC:
              SifWriteLn("failed - bad crc");
              break;
          }
          break;
          
        case 'v':
          SifWriteLn("System Info:");
          SifWrite(" Bootloader:  ver: ");
          SifWriteLn(versionString);
          SifWrite(" (wdt: ");
          SifWrite(Dec2Str(pFWDescriptor->wdtEnable, &buf[sizeof(buf)]));
          SifWrite(", crc: ");
          SifWrite(Hex2Str(pFWDescriptor->crc, 2*sizeof(Crc16_t), &buf[sizeof(buf)]));
          SifWriteLn(")");
          break;      
        
        case 'e':
#if defined(BLC_USE_DFLASH)
          HalDFlashEraseAddrRange(APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_END);
#else
          HalPFlashEraseAddrRange(APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_END);
#endif        
          break;
        
//        case 'f':
//          SifWrite("Erase Data Flash: ");
//          while (SioTxBusy()) {}
//          //if (HalDFlashEraseAddrRange(0, DFLASH_SIZE-1, true) == 0)
//#if defined(BLC_USE_DFLASH)
//          if (HalDFlashEraseChip(true) == 0) 
//#else 
//          // FIX ME
//          if (false)
//#endif
//            SifWriteLn("ok");
//          else
//            SifWriteLn("failed");
//          break;
          
        case 'x':
          // JUMP TO APPL:
          switch (FirmwareCheck()) {
            case FIRMWARE_STATUS_OK:
              BAppExecuteFirmware();
              break;
            case FIRMWARE_STATUS_BAD_ID:
              SifWriteLn("failed - bad identity");
              break;
            case FIRMWARE_STATUS_BAD_CRC:
              SifWriteLn("failed - bad crc");
              break;
          }
          while (SioTxBusy()) ;  // wait until sio buffer is written...
					// make cold start
					// wrong CRC!
					pBootControl->crc   	= 1+CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);  
#if defined(BOOT_CONTROL_IN_FRAM)
          HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif

          NVIC_SystemReset();
          break;
          
#ifdef USE_FWD_BC_SAVE_GOLDEN_IMAGE
        /* LOAD */
        case 'l':
          {
            int32_t cnt;
            loadToPFlash = true;
            SifWrite("\r\nWait Xmodem transfer BIN Image: ");
            xOffset = _APPLIICATION_FLASH_START;
            if ((cnt = XmodemReceive(XmodemCallback)) > 0) {
              SifWrite("\r\nDone - received ");
              SifWrite(Dec2Str(cnt, &buf[sizeof(buf)]));
              SifWrite(" (");
              SifWrite(Hex2Str(cnt, 0, &buf[sizeof(buf)]));
              SifWriteLn(")");
            } else {
              SifWriteLn("failed!");
            }
          }
          break;
#endif
        
#ifdef SUPPORT_DFU_VIA_DFLASH
        /* DOWNLOAD image and activate */
        case 'a':
          SifWriteLn("Auto (d+p+x)");
          DownloadAndActivate();
          break;
        
        case 'd':
          SifWriteLn("Download");
          Download();
          break;
        
        /* PROGRAM */
        case 'p':
          SifWriteLn("Program");
#if defined(BLC_USE_DFLASH)
          if (BlcCheckDataFlashFirmware(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd) == 0) {
#else
          if (!BlcCheckProgramFlashFirmware(PFLASH_DFU_START, &fwd)) {
#endif        
            if (DoPFlashProgram()) {
              SifWriteLn("ok");
            } else {
						  SifWriteLn("FAILED !!");
						}
          } else {
              SifWriteLn("FW WRONG !!");
          }
          break;
//        case 'n':
//          SaveNewImage();
//          break;
        case '1':
          SifWrite("New Firmware check: ");
          if (VerifyFirmware(DFLASH_DFU_START) >= 0 ) {
            SifWriteLn("ok");
          } else {
              SifWriteLn("error !");
          }
          break;
#endif            
          
#ifdef USE_FWD_BC_MAKE_DFU_GOLD
        case 'b':
          SaveGoldenImage();
          break;
        
        case 'r':
          RestoreGoldenImage();
          break;
        
        case '2':
          SifWrite("Golden Firmware check: ");
          if (VerifyFirmware(DFLASH_IMAGE_GOLDEN_START) >= 0 ) {
            SifWriteLn("ok");
          } else {
              SifWriteLn("error !");
          }
          break;
#endif          

        case -1:
        case 0x1b:
          break;

        default:
          SifWriteLn("Unknown command");
          PrintMenu();
          break;
      }
      tickstart = HAL_GetTick();
      lastStep = 0;
    }
  }			
}

#endif

/******************************************************************************/

