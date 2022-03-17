/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/pflash/pflash.h"
#include "plf/trc/trc.h"

#include "stm32l4xx_hal.h"
#include "stm32l4xx_hal_flash.h"
#include "stm32l4xx_hal_flash_ex.h"

uint8_t pflashStatus;

/* FLASH Memory Programming functions *****************************************/   

bool HalPFlashRead(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len)
{
  if (flashAddr < FLASH_BASE) return false;
  if (flashAddr > FLASH_END-len) return false;
  memcpy((void *)ramAddr, (void *)flashAddr, len);
  return true;    
}

/****************************************************************************/

// 2 KB / Sector
bool HalPFlashEraseSector(uint_fast16_t sector)
{
  FLASH_EraseInitTypeDef fe;
  uint32_t pageError;
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalPFlashEraseSector(%u)", sector);
  HAL_FLASH_Unlock();
  __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS);
  //__HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);

  fe.TypeErase  = FLASH_TYPEERASE_PAGES;
  #if defined(FLASH_BANK_2)
    fe.Banks    = FLASH_BANK_2;
  #else
    fe.Banks    = FLASH_BANK_1;
  #endif  
  fe.Page     = sector;
  fe.NbPages  = 1;

  HAL_StatusTypeDef st = HAL_FLASHEx_Erase(&fe, &pageError);
  HAL_FLASH_Lock();

  return st == HAL_OK;
}

bool HalPFlashEraseAddrRange(uint32_t firstAddr, uint32_t lastAddr)
{
  FLASH_EraseInitTypeDef eraseInit;
  HAL_StatusTypeDef stat;
  uint32_t pageError = 0;
	
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalPFlashEraseAddrRange(0x%08X, 0x%08X)", firstAddr, lastAddr);
 
	memset(&eraseInit, 0, sizeof(eraseInit));
  eraseInit.TypeErase 	= FLASH_TYPEERASE_PAGES; 

	if (HAL_FLASH_Unlock() != HAL_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HAL_FLASH_Unlock() failed");
    return false;
  }  
  
#if defined(FLASH_BANK_2)
  if (firstAddr < 0x08040000)
#endif    
  {
    const uint32_t startAddr = firstAddr;
    const uint32_t stopAddr = MIN_VAL(lastAddr, 0x0803FFFF);
    
    eraseInit.Banks    = FLASH_BANK_1;
    eraseInit.Page     = ((startAddr-0x08000000)+(FLASH_PAGE_SIZE-1)) / FLASH_PAGE_SIZE;
    eraseInit.NbPages  = ((1+stopAddr-startAddr)+(FLASH_PAGE_SIZE-1)) / FLASH_PAGE_SIZE;
    
    pFlash.ErrorCode = HAL_FLASH_ERROR_NONE;
    __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS);
    
    FLASH->SR |= ~FLASH_FLAG_SR_ERRORS;  
    stat = HAL_FLASHEx_Erase(&eraseInit, &pageError);
    if (stat != HAL_OK) {
      HAL_FLASH_Lock();
      TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, 
      "HAL_FLASHEx_Erase() failed: stat:%u, co:%X, pe:%X, af:%X, la:%X", 
        stat, pFlash.ErrorCode, pageError, startAddr, stopAddr);
      return false;
    }    
  }
  
#if defined(FLASH_BANK_2)
  if (lastAddr >= 0x08040000) {
    const uint32_t startAddr = MAX_VAL(firstAddr, 0x08040000);
    const uint32_t stopAddr = lastAddr;    
    
    eraseInit.Banks    = FLASH_BANK_2;
    eraseInit.Page     = ((startAddr-0x08000000)+(FLASH_PAGE_SIZE-1)) / FLASH_PAGE_SIZE;
    eraseInit.NbPages  = ((1+stopAddr-startAddr)+(FLASH_PAGE_SIZE-1)) / FLASH_PAGE_SIZE;

    pFlash.ErrorCode = HAL_FLASH_ERROR_NONE;
    __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS);
    
    FLASH->SR |= ~FLASH_FLAG_SR_ERRORS;  
    stat = HAL_FLASHEx_Erase(&eraseInit, &pageError);
    if (stat != HAL_OK) {
      HAL_FLASH_Lock();
      TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, 
      "HAL_FLASHEx_Erase() failed: stat:%u, co:%X, pe:%X, af:%X, la:%X", 
        stat, pFlash.ErrorCode, pageError, startAddr, stopAddr);
      return false;
    }    
  }
#endif    

	HAL_FLASH_Lock();
  return true;
}


/****************************************************************************/


bool HalPFlashWrite(uint32_t pflsahAddr, uint32_t ramAddr, uint32_t len)
{
	uint64_t * data = (uint64_t*)ramAddr;
	uint32_t i;
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalPFlashWrite(0x%08X,..,%u)", pflsahAddr, len);

	if (((pflsahAddr>>2)<<2) != pflsahAddr) {
    pflashStatus = (HAL_StatusTypeDef)101;
    TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, "HalPFlashWrite FLASH ADDR WRONG: 0x%08X", pflsahAddr);
    return false;
  }
	if (((ramAddr>>1)<<1) != ramAddr) {
    pflashStatus = (HAL_StatusTypeDef)102;
    TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, "HalPFlashWrite RAM ADDR WRONG: 0x%08X", ramAddr);
    return false;
  }

  FLASH->SR |= ~FLASH_FLAG_SR_ERRORS;  

  //FLASH_FlushCaches();

	HAL_FLASH_Unlock();
  __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS);
	//__HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);    

  
	for (i=0; i<len; i += sizeof(uint64_t)) {
    //if (*data != *(uint64_t*)(i+pflsahAddr)) {
      FLASH->SR |= ~FLASH_FLAG_SR_ERRORS;  
      pFlash.ErrorCode = 0;      
      pflashStatus = HAL_FLASH_Program(FLASH_TYPEPROGRAM_DOUBLEWORD, i+pflsahAddr, *data);
      if (pflashStatus != HAL_OK) {
        HAL_FLASH_Lock();
        TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, "HalPFlashWrite FAILED: 0x%08X", i+pflsahAddr);
        return false;
      }
    //}
		data++;
	}

	HAL_FLASH_Lock();

  FLASH_FlushCaches();
	return true;
}

/***************************************************************************/
/* COPY */

bool HalPFlashCopy(uint32_t srcAddr, uint32_t dstAddr, size_t size, uint8_t * buffer, size_t bufferSize)
{

  if (!HalPFlashEraseAddrRange(dstAddr, dstAddr+size-1))
    return false;
  
  while (size > 0) {
    size_t s = MIN_VAL(size, bufferSize);
    if (!HalPFlashRead(srcAddr, (uint32_t)buffer, s)) 
      return false;
    if (!HalPFlashWrite(dstAddr, (uint32_t)buffer, s))
      return false;
    srcAddr += s;
    dstAddr += s;
    size -= s;
  }
  return true;
}


int HalPFlashCompare(uint32_t addr1, uint32_t addr2, int32_t size)
{
  for (;;) {
    if (*(uint32_t*)addr1 != *(uint32_t*)addr2) 
      return addr1;
    addr1 += 4;
    addr2 += 4;
    size -= 4;
    if (size <= 0) 
      return 0;
  }
}

/****************************************************************************/

void HalPFlashInit(void)
{
	HAL_FLASH_Unlock();
	//__HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);    
  __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS);
	HAL_FLASH_Lock();
}


/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/


#define READ_LINE_LENGTH (16)

int_fast8_t CliPFlashRead(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint32_t addr = (uint32_t)param1;
  int32_t length = (uint32_t)param2;
  uint_fast8_t width = (uint8_t)param3;
  uint16_t i;
  union {
    uint8_t   w8[READ_LINE_LENGTH];
    uint16_t  w16[READ_LINE_LENGTH>>1];
    uint32_t  w32[READ_LINE_LENGTH>>2];
    uint64_t  w64[READ_LINE_LENGTH>>3];
  } data;

  switch (width) {
    case 8:
    case 4:
    case 2:
    case 1:
      break;
    default:
      return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  length = (((length + READ_LINE_LENGTH -1) / READ_LINE_LENGTH) * READ_LINE_LENGTH);
  

  for (;;) {
    if (HalPFlashRead(addr, (uint32_t)data.w8, READ_LINE_LENGTH)) {
      CliPrintf("0x%08x: ", addr);
      switch (width) {
        case 8:
          for (i = 0; i < READ_LINE_LENGTH>>3; i++) {
            CliPrintf("%016llx ", data.w64[i]);
          }
          break;
        case 4:
          for (i = 0; i < READ_LINE_LENGTH>>2; i++) {
            CliPrintf("%08lx ", data.w32[i]);
          }
          break;
        case 2:
          for (i = 0; i < READ_LINE_LENGTH>>1; i++) {
            CliPrintf("%04x ", data.w16[i]);
          }
          break;
        default:
          for (i = 0; i < READ_LINE_LENGTH; i++) {
            CliPrintf("%02x ", data.w8[i]);
          }
          break;
      }
      CliWrite("    ");
      
      for (i = 0; i < READ_LINE_LENGTH; i++) {
        if ((data.w8[i] >= ' ')  && (data.w8[i] < 0x80))
          CliWriteChar(data.w8[i]);
        else
          CliWriteChar('.');
      }
      CliWrite(cliNewLine);
    } else {
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    addr += READ_LINE_LENGTH;
    length -= READ_LINE_LENGTH;
    if (length < READ_LINE_LENGTH)
      break;    
  }
  
  return CLI_RESULT_OK;
}

int_fast8_t CliPFlashWrite(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint16_t length = MIN_VAL((uint16_t)param3, 16);
  uint16_t i;
  uint8_t data[16];

  for (i = 0; i < length; i++) {
    data[i] = (uint8_t)param2;
  }

  if (HalPFlashWrite((uint32_t)param1, (uint32_t)data, length)) {
    return CLI_RESULT_OK;
  } else {
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}


static int_fast8_t CliPFlashErase(CliParam_t firstAddr, CliParam_t lastAddr, CliParam_t param3)
{
  PlfTime_t t1, t2;
  PlfTimeMsGet(&t1);
  bool ok = HalPFlashEraseAddrRange(firstAddr, lastAddr);
  PlfTimeMsGet(&t2);
  if (ok) {
    CliPrintf("Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(t2, t1));
    return CLI_RESULT_OK;
  } else {
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}


int_fast8_t CliPFlashCopy(CliParam_t addr1, CliParam_t addr2, CliParam_t size)
{
  uint8_t xx[1024];
  if (HalPFlashCopy(addr1, addr2, size, xx, sizeof(xx))) {
    CliPrintf("Copy OK" CLI_NL);
    return CLI_RESULT_OK;
  } else {
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

int_fast8_t CliPFlashCompare(CliParam_t addr1, CliParam_t addr2, CliParam_t size)
{
  int i = HalPFlashCompare(addr1, addr2, size);
  if (i == 0) {
    CliPrintf("Compare OK" CLI_NL);
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Compare Error: 0x%08X" CLI_NL, i);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

CLI_START_TABLE(hal_pflash)

  //CLI_ENTRY0( "show", "Show DFLASH data", TstHalDFlashShow)
  //CLI_ENTRY1( "wsr", "Write Status Register (excl SRWD)", CliFlashStatusWrite, CLI_PARAM_UINT32)

  CLI_ENTRY3( "rd", "Read from DF <addr> <len> <width: 1, 2, 4, 8>", CliPFlashRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY3( "wr", "Write to DF <addr> <byte> <cnt (max 16)>", CliPFlashWrite, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_ENTRY3( "cpy", "Copy <src addr> <dst addr> <len>", CliPFlashCopy, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY3( "cmp", "Compare <addr1> <addr2> <len>", CliPFlashCompare, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_ENTRY2( "erase", "Erase [first addr] [last addr]", CliPFlashErase, CLI_PARAM_UINT32, CLI_PARAM_UINT32)


	CLI_SUBTEST("trc",      "Trace system", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_pflash)

/******************************************************************************/
#endif
/******************************************************************************/

