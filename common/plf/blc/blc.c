/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stddef.h>

#define _MAKE_BLC_CODE
#include "plf/blc/blc.h"
#undef _MAKE_BLC_CODE

#include "hal/hal.h"

#include "hal/fram/fram.h"
#include "hal/cfg/fram_cfg.h"

#include "hal/pflash/pflash.h"
#include "hal/dflash/dflash.h"

#include "plf/trc/trc.h"
#include "plf/ver/ver.h"
#include "hal/wdt/wdt.h"
#include "plf/blc/blc.h"
#include "plf/rle/dfx.h"


#if defined(USE_SAVE_APPL_BEFORE_DFU)
#include "app/app.h"
#include "net/net.h"
#endif

/******************************************************************************/

#if defined(BOOT_CONTROL_IN_FRAM)
  BootControl_t         bootControl;
  BootControl_t 			  * pBootControl  = &bootControl;
#else
  BootControl_t 				* pBootControl  = (BootControl_t *)BLC_BOOT_CONTROL_ADDR;
#endif

const FWDescriptor_t 		* pFWDescriptor = (FWDescriptor_t *)BLC_FW_DESCIPTOR_ADDR;

/******************************************************************************/

#if !defined(BOOTLOADER)

#include "sif/sif.h"

#define SifWriteCh(c)   SifWriteCh(c)
#define SifWrite(sz)   SifWrite(sz) 
#define SifWriteLn(sz) SifWriteLn(sz)
  
/******************************************************************************/

#else

#include "hal/pflash/pflash.h"

#include "plf/sio/sio.h"

#define SifTxFlush()      (void)0

#if defined(USE_FWD_BC_MAKE_DFX_NEW)        

// buffer size: DFX_BUFFER
static bool _CpxUnpackToPFlash(uint32_t pFlashAddr, uint8_t * upBuf)
{
  if (!HalPFlashWrite(pFlashAddr, (uint32_t)upBuf, XFLASH_BUFFER)) {
    TRACE(TRC_TA_PLF, TRC_TL_ERROR, "PFLASH write failed");
    return false;
  }
  return true;
}

bool CpxUnpackToPFlash(
  uint32_t destAddr,
  uint32_t dstSize,
  uint32_t srcAddr)
{
  if (!HalPFlashEraseAddrRange(destAddr, destAddr + dstSize-1)) {
    TRACE(TRC_TA_PLF, TRC_TL_ERROR, "DFLASH erease failed");
    return false;
  }
  return CpxUnpack(_CpxUnpackToPFlash, destAddr, dstSize, srcAddr);
}


#endif



#if defined(BLC_USE_DFLASH)

int_fast8_t BlcCopyDataFlashToFirmware(uint32_t dataFlashAddr)
{
	uint32_t progFlashAddr = APP_PROGRAM_FLASH_START;  /* desination */
	uint8_t buf[DFLASH_PP_SIZE];
//  uint16_t cnt = 0;
	int_fast8_t res  = 0;
	
	
	//SifWrite("Erasing Program Flash: ");
  if (!HalPFlashEraseAddrRange(APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_END)){
    return -1;
  }
  
	//SifWriteLn("-------------------------");
  HalWdtFeed();  

	//SifWrite("Writing Program Flash: ");
  // chunks of 256 byte ...
	while (res == 0) {
    
    
    HalWdtFeed();
    // Read data from data flash
    if (HalDFlashRead(dataFlashAddr, buf, DFLASH_PP_SIZE) != 0) {
      res = -2;
    }
    
    // Write data to program - if different
		if (res == 0) {
#ifndef PFLASH_WRITE_BUFFER
      uint64_t * b64 = (uint64_t*)&buf[0];
      int i = 0;
      for (;;) {
			  if (!HalPFlashWrite_64b(progFlashAddr, *b64++)) {
          res = -3;
          break;
			  }
        i += sizeof(uint64_t);
        progFlashAddr += sizeof(uint64_t);
        if (i >= DFLASH_PP_SIZE) break;
      }
#else
			if (!HalPFlashWrite(progFlashAddr, (uint32_t)&buf[0], DFLASH_PP_SIZE)) {
        res = -3;
			}
      progFlashAddr += DFLASH_PP_SIZE;
#endif
		}
  
		if (res == 0) {
			dataFlashAddr += DFLASH_PP_SIZE;
			
			if ((uint32_t)progFlashAddr >= APP_PROGRAM_FLASH_END)
				break;
		}
  }
  HalWdtFeed();
	return res;
}

#endif
#endif

/******************************************************************************/

#if defined(BLC_USE_DFLASH)

int_fast8_t BlcWriteToDataFlash(uint32_t dFlashAddr, const void *buf, uint_fast16_t len)
{
  bool ok;
  HalWdtFeed();
  if ((dFlashAddr % DFLASH_SMALL_BLOCK_SIZE) == 0) {
    if (HalDFlashEraseSmallBlock(dFlashAddr, true) != 0) {
      return -1;
    }
  }
  ok = HalDFlashWrite(dFlashAddr, buf, len, true) == DFLASH_OK;
  //bclExeTime = HAL_GetTick() - bclExeTime;
  return ok;
}

void BclProgressWriteNL(void)
{
  #ifdef BOOTLOADER
    SifWrite("\r\n");
  #else  
    SifWrite(cliNewLine);
  #endif
}

void BclProgressWriteTick(void)
{
//  #ifdef BOOTLOADER
//    SifWrite("+");
//  #else  
//    SifWriteCh ('+');
//  #endif
}

#define COPY_TO_DFLASH_STEP_SIZE (DFLASH_PP_SIZE)

int_fast8_t BlcCopyFirmwareToDataFlash(uint32_t dataFlashAddr, int32_t dfuSize)
{
  int ix = 0;

  //bclExeTime = HAL_GetTick();
	uint32_t progFlashAddr = APP_PROGRAM_FLASH_START; /* source */
  
  HalDFlashEraseAddrRange(dataFlashAddr, dataFlashAddr+dfuSize-1);
  BclProgressWriteNL();
	
	for (;;) {

    HalWdtFeed();
   
    if (++ix>0x10) {
      BclProgressWriteTick();
      ix = 0;
    }
    
    HalWdtFeed();
    if (HalDFlashWrite(dataFlashAddr, (const void *)progFlashAddr, COPY_TO_DFLASH_STEP_SIZE, true) != DFLASH_OK)
      return -1;


    dataFlashAddr += COPY_TO_DFLASH_STEP_SIZE;
    progFlashAddr += COPY_TO_DFLASH_STEP_SIZE;
    dfuSize -= COPY_TO_DFLASH_STEP_SIZE;
    if (dfuSize <= 0)
      break;
	} ;
  HalWdtFeed();
  BclProgressWriteNL();
	return 0;
}

// DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, ..
FwdBootCommand_t BlcCheckDataFlashFirmware(uint32_t dataFlashAddr, uint32_t dataFlashSize, FWDescriptor_t * pfwd)
{
	uint8_t buf[DFLASH_PP_SIZE];
  FWDescriptor_t *  pFWD = (FWDescriptor_t *)&buf[DFLASH_PP_SIZE-BLC_FW_DESCIPTOR_SIZE];
  Crc16_t crc   = CRC16_INITIAL_SEED;

  // chunks of DFLASH_PP_SIZE (256) byte ...
	for (uint32_t size = 0; size < dataFlashSize-DFLASH_PP_SIZE; size += DFLASH_PP_SIZE) {    
    // Read data from data flash
    if (HalDFlashRead(dataFlashAddr, buf, DFLASH_PP_SIZE) != 0) {
      return FWD_BC_UNKNOWN;
    }
    dataFlashAddr += DFLASH_PP_SIZE;
    crc = CalcCrc16(DFLASH_PP_SIZE, (void *)buf, crc);   
  }
  HalWdtFeed();
  
  if (HalDFlashRead(dataFlashAddr, buf, DFLASH_PP_SIZE) != 0) {
    return FWD_BC_UNKNOWN;
  }
  crc = CalcCrc16(DFLASH_PP_SIZE-BLC_FW_DESCIPTOR_SIZE, (void *)buf, crc);    

  if (crc == pFWD->crc) {
    *pfwd = *pFWD;
    return FWD_BC_MAKE_DFU_NEW;
  } else {
    return FWD_BC_UNKNOWN;
  }
  
}

FwdBootCommand_t BlcReadDescriptor(const uint32_t dataFlashAddr, const uint32_t dataFlashSize, FWDescriptor_t * const pfwd)
{
	FWDescriptor_t desc;
  
  if (HalDFlashRead(dataFlashAddr + dataFlashSize - BLC_FW_DESCIPTOR_SIZE, (uint8_t*)&desc, sizeof(FWDescriptor_t)) != 0) {
    return FWD_BC_UNKNOWN;
  }
  
  HalWdtFeed();  
  *pfwd = desc;
  return FWD_BC_MAKE_DFU_NEW;  
}

#elif defined(BLC_USE_PFLASH)


FwdBootCommand_t BlcCheckProgramFlashFirmware(uint32_t progFlashAddr, FWDescriptor_t * pfwd)
{
  FWDescriptor_t *  pFWD = (FWDescriptor_t *)(progFlashAddr + APP_PROGRAM_FLASH_SIZE - BLC_FW_DESCIPTOR_SIZE);

  if (pFWD->crcGuard == RUN_FROM_IDE) {
    if (pfwd)
      *pfwd = *pFWD;
    return FWD_BC_MAKE_DFU_NEW;
  }
  Crc16_t crc = CalcCrc16(APP_PROGRAM_FLASH_SIZE-BLC_FW_DESCIPTOR_SIZE, (void *)progFlashAddr, CRC16_INITIAL_SEED);    

  if (crc == pFWD->crc) {
    if (pfwd)
      *pfwd = *pFWD;
    return FWD_BC_MAKE_DFU_NEW;
  } else {
    return FWD_BC_UNKNOWN;
  }

  
}

int_fast8_t BlcCopyProgramFlashToFirmware(uint32_t dfuFlashAddr)
{
	uint32_t progFlashAddr = APP_PROGRAM_FLASH_START;  /* desination */
	uint8_t buf[1024];
	int_fast8_t res  = 0;
	
	
	//SifWrite("Erasing Program Flash: ");
  if (!HalPFlashEraseAddrRange(APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_END)){
    return -1;
  }
  
	//SifWriteLn("-------------------------");
  HalWdtFeed();  

	//SifWrite("Writing Program Flash: ");
  // chunks of 256 byte ...
	while (res == 0) {
    
    
    HalWdtFeed();
    // Read data from data flash
    if (!HalPFlashRead(dfuFlashAddr, (uint32_t)buf, 1024) != 0) {
      res = -2;
    }
    
    // Write data to program - if different
		if (res == 0) {
#ifndef PFLASH_WRITE_BUFFER
      uint64_t * b64 = (uint64_t*)&buf[0];
      int i = 0;
      for (;;) {
			  if (!HalPFlashWrite_64b(progFlashAddr, *b64++)) {
          res = -3;
          break;
			  }
        i += sizeof(uint64_t);
        progFlashAddr += sizeof(uint64_t);
        if (i >= DFLASH_PP_SIZE) break;
      }
#else
			if (!HalPFlashWrite(progFlashAddr, (uint32_t)&buf[0], 1024)) {
        res = -3;
			}
#endif
		}
  
		if (res == 0) {
			dfuFlashAddr += 1024;
			progFlashAddr += 1024;
			
			if ((uint32_t)progFlashAddr >= APP_PROGRAM_FLASH_END)
				break;
		}
  }
  HalWdtFeed();
	return res;
}


#endif

#ifndef BOOTLOADER
#if defined(USE_FWD_BC_MAKE_DFX_NEW)

FwdBootCommand_t BlcCheckNewDfuDfxImage(uint32_t dflashAddr, FWDescriptor_t * fwd)
{
  // DFX image (RLE) ???
  CpxDfuCheck_t cdc;
	CliPrintf("Checking New DFU/DFX @ 0x%08X" CLI_NL, dflashAddr);

	SifWrite("Checking DFX : ");
  cdc = CpxDfuCheck(dflashAddr, fwd);  
  if (cdc == CDC_OK) {
    SifWrite("OK\r\n");
    return FWD_BC_MAKE_DFX_NEW;
  }
  SifWrite("Not ok\r\n");
  
  // DFU image ???
	SifWrite("Checking DFU : ");
  #if defined(BLC_USE_DFLASH)
    FwdBootCommand_t fbc = BlcCheckDataFlashFirmware(dflashAddr, APP_PROGRAM_FLASH_SIZE, fwd);
  #elif defined(BLC_USE_PFLASH)
    FwdBootCommand_t fbc = BlcCheckProgramFlashFirmware(dflashAddr, fwd);
  #else
    #error "FIX"
  #endif
  if (fbc == FWD_BC_MAKE_DFU_NEW) {
    if (strcmp(fwd->id, BOOTCTRL_HEADER_ID) == 0) {
      SifWrite("OK\r\n");
      return FWD_BC_MAKE_DFU_NEW;
    }
  }
  SifWrite("Not ok\r\n");
  
  return FWD_BC_UNKNOWN;
}

#endif
#endif


#if defined(USE_SAVE_APPL_BEFORE_DFU)

#include "hal/rtc/rtc.h"
#pragma anon_unions
typedef union {
  RtcBackupData_t data;
  struct {
    ItfThingsName_t       name;
    uint8_t               no;    // 
    WiFi_t                wiFi;
    Crc16_t               crc;
  };
}                 DfuRec_t;

static void ApplBefore_BlcActivateNewImage(void)
{
  DfuRec_t    dfuRec;
  dfuRec.no = app.no;
  memcpy(dfuRec.name, app.name, sizeof(dfuRec.name));
  dfuRec.wiFi = wiFi;
  dfuRec.crc = CalcCrc16(offsetof(DfuRec_t, crc), (void *)&dfuRec, CRC16_INITIAL_SEED);
  RtcBackupDataWrite(dfuRec.data);
}

void ApplAfter_BlcActivateNewImage(void)
{
  DfuRec_t    dfuRec;
  RtcBackupDataRead(dfuRec.data);
  if (dfuRec.crc == CalcCrc16(offsetof(DfuRec_t, crc), (void *)&dfuRec, CRC16_INITIAL_SEED)) {
    app.no = dfuRec.no;
    memcpy(app.name, dfuRec.name, sizeof(dfuRec.name));
    app.crc = CalcCrc16(sizeof(app)-sizeof(Crc16_t), (uint8_t*)&app, CRC16_INITIAL_SEED);
    #if (NVM_ACCESS==NVM_ACCESS_ADDRESS)
      NvmWrite(offsetof(NvmStruct_t, appl.app), (uint8_t *)&app, sizeof(app));
    #else
      NvmWrite(NVM_ID_APP, app);
    #endif
    wiFi = dfuRec.wiFi;
    #if (NVM_ACCESS==NVM_ACCESS_ADDRESS)
      NvmWrite(offsetof(NvmStruct_t, appl.wiFi), (uint8_t *)&wiFi, sizeof(WiFi_t));
    #else
      NvmWrite(NVM_ID_WIFI, wiFi);
    #endif
  }
  memset(&dfuRec, 0, sizeof(dfuRec));
  RtcBackupDataWrite(dfuRec.data);
}

#endif

void BlcActivateNewImage(uint32_t dflashAddr, FwdBootCommand_t  fwType)
{
#if defined(USE_SAVE_APPL_BEFORE_DFU)
  ApplBefore_BlcActivateNewImage();
#endif
  // Update BootControl record...
  pBootControl->app	= FWD_LAST_APP_APPLICATION;
  pBootControl->cmd	= fwType;
  pBootControl->adr = dflashAddr;
  pBootControl->crc	= CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);
#if defined(BOOT_CONTROL_IN_FRAM)
  HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif
  SifTxFlush();
  HAL_DeInit();
  NVIC_SystemReset();
}

void BlcReboot(void)
{
  // Update BootControl record...
  pBootControl->app	= FWD_LAST_APP_APPLICATION;
	pBootControl->cmd	= FWD_BC_REBOOT; 
  pBootControl->crc	= CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);
#if defined(BOOT_CONTROL_IN_FRAM)
  HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif

	SifTxFlush();
  HAL_DeInit();
  NVIC_SystemReset();
}

void BclProgressDeleteTick(void)
{
  #ifdef BOOTLOADER
    SifWrite("-");
  #else  
    //SifWrite("---");
  #endif
}



/******************************************************************************/

#if !defined(BOOTLOADER)

#if defined(USE_FWD_BC_BOOT_MENU)
void BlcBtlMenuEnter(const char * info)
{
  if (info)  {
    SifWriteLn(info);
  }

  // Update BootControl record...
  pBootControl->app = FWD_LAST_APP_APPLICATION;
	pBootControl->cmd	= FWD_BC_BOOT_MENU;
  pBootControl->crc	= CalcCrc16(offsetof(BootControl_t, crc), (void *)pBootControl, CRC16_INITIAL_SEED);
#if defined(BOOT_CONTROL_IN_FRAM)
  HalFramWriteBlock(-sizeof(BootControl_t), (uint8_t *)pBootControl, sizeof(BootControl_t));
#endif

	while (SifTxBusy()) {};
  NVIC_SystemReset();
  
}

#endif
#endif

/******************************************************************************/

/* compiletime check of the size */
/* Error: #94-D: the size of an array must be greater than zero */
typedef uint8_t TestDescriptorSize_t[sizeof(FWDescriptor_t) <= BLC_FW_DESCIPTOR_SIZE];

/******************************************************************************/

