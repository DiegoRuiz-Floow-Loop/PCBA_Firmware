/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#ifndef __MODULE__
#define __MODULE__ "fram"
#endif

#include "hal/hal.h"
#include "hal/fram/fram.h"
#include "hal/cfg/fram_cfg.h"

#include "hal/dio/dio.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

/***************************************************************************/
// MB85RS16
/***************************************************************************/
  
/* LOW LEVEL I/O operation */

#define FRAM_SPI_TIMEOUT     (1000)

#define SPI_FRAM_SELECT()    HAL_DIO_PIN_CLR(MP_nCS_FRAM)
#define SPI_FRAM_DESELECT()  HAL_DIO_PIN_SET(MP_nCS_FRAM)

#define HAL_FRAM_BUF_SIZE   (256)

/***************************************************************************/

// Write Enable
static bool _WREN(void) 
{
  HAL_StatusTypeDef s;
  uint8_t xx[8];
  xx[0] = 0x06;
  SPI_FRAM_SELECT();
  do {
    s = HAL_SPI_Transmit(&SPI_FRAM, xx, 1, FRAM_SPI_TIMEOUT);
  } while (s == HAL_BUSY);
  SPI_FRAM_DESELECT();
  if (SPI_FRAM.ErrorCode != HAL_SPI_ERROR_NONE) return false;
  return true;
}

// Write Enable
//static bool _WRDI(void) 
//{
//  HAL_StatusTypeDef s;
//  uint8_t xx[8];
//  xx[0] = 0x04;
//  SPI_FRAM_SELECT();
//  do {
//    s = HAL_SPI_Transmit(&SPI_FRAM, xx, 1, FRAM_SPI_TIMEOUT);
//  } while (s == HAL_BUSY);
//  SPI_FRAM_DESELECT();
//  if (SPI_FRAM.ErrorCode != HAL_SPI_ERROR_NONE) return false;
//  return true;
//}


//// Status Register
#define SR_WIP(xx)      ((xx >> 0) & 1)
//#define SR_WEL(xx)      ((xx >> 1) & 1)
//#define SR_BP0(xx)      ((xx >> 2) & 1)
//#define SR_BP1(xx)      ((xx >> 3) & 1)
//#define SR_BP2(xx)      ((xx >> 4) & 1)
//#define SR_SRWD(xx)     ((xx >> 7) & 1)

//#define SR_BP(xx)       ((xx >> 2) & 7)

// Read Status Register
static HAL_StatusTypeDef _RDSR(uint8_t * reg) 
{
  HAL_StatusTypeDef s;
  uint8_t xx[8];
  xx[0] = 0x05;
  SPI_FRAM_SELECT();
  do {
    s = HAL_SPI_Transmit(&SPI_FRAM, xx, 1, FRAM_SPI_TIMEOUT);
    s = HAL_SPI_Receive(&SPI_FRAM, reg, 1, FRAM_SPI_TIMEOUT);
  } while (s == HAL_BUSY);
  SPI_FRAM_DESELECT();
  return s;
}

// Read Status Register
//static bool _WRSR(uint8_t reg) 
//{
//  HAL_StatusTypeDef s;
//  uint8_t xx[HAL_FRAM_BUF_SIZE];
//  xx[0] = 0x01;
//  xx[1] = reg;
//  SPI_FRAM_SELECT();
//  do {
//    s = HAL_SPI_Transmit(&SPI_FRAM, xx, 2, FRAM_SPI_TIMEOUT);
//  } while (s == HAL_BUSY);
//  SPI_FRAM_DESELECT();
//  if (SPI_FRAM.ErrorCode != HAL_SPI_ERROR_NONE) return false;
//  return true;
//}

// Read Data Byte
static bool _READ(uint32_t address, uint8_t * data, uint_fast16_t length) 
{
  HAL_StatusTypeDef s;
  uint8_t xx[3+HAL_FRAM_BUF_SIZE];
  
  memset(xx, 0xFF, sizeof(xx));
  
  xx[0] = 0x03;
  xx[1] = (address >> 8);
  xx[2] = address;
  //xx[3] = 0; // dummy byte
  SPI_FRAM_SELECT();
  do {
    s = HAL_SPI_TransmitReceive(&SPI_FRAM, xx, xx, 3+length, FRAM_SPI_TIMEOUT);
  } while (s == HAL_BUSY);
  SPI_FRAM_DESELECT();
  if (s != HAL_OK)  
    return false;
  memcpy(data, &xx[3], length);
  return true;
}

// Write Data Byte
static bool _WRITE(uint32_t address, uint8_t * data, uint32_t length) 
{
  HAL_StatusTypeDef s;
  uint8_t xx[HAL_FRAM_BUF_SIZE];
  xx[0] = 0x02;
  xx[1] = (address >> 8);
  xx[2] = address;
  SPI_FRAM_SELECT();
  do {
    s = HAL_SPI_Transmit(&SPI_FRAM, xx, 3, FRAM_SPI_TIMEOUT);
  } while (s == HAL_BUSY);
  // data may be on an odd address!
  memcpy(xx, data, length);
  do {
    s = HAL_SPI_Transmit(&SPI_FRAM, xx, length, FRAM_SPI_TIMEOUT);
  } while (s == HAL_BUSY);
  SPI_FRAM_DESELECT();
  if (SPI_FRAM.ErrorCode != HAL_SPI_ERROR_NONE) return false;
  return true;
}


/***************************************************************************/
/* STATUS */

static bool HalFramWaitReady(uint32_t waitTime)
{
  HAL_StatusTypeDef s;
  PlfTime_t tStart=0, tNow;
  
  for (;;) {
		uint8_t reg = 0xFF;
    
    for (;;) {
      s = _RDSR(&reg);
      if (s== HAL_OK) 
        break;
      if (s != HAL_BUSY)
        return false;

      break;
    }

    if (SR_WIP(reg) == 0) 
      return true;
    
    if (tStart == 0)
      PlfTimeMsGet(&tStart);
    
    PlfTimeMsGet(&tNow);
    if (TIME_DIF(tNow, tStart) > waitTime) 
      return false;
    
    HalWdtFeed();
  }
}


/***************************************************************************/
/* READ */

NvmErrorStatus_t HalFramReadBlock(
  int32_t addr,
  uint8_t *b,
  uint16_t length)  
{
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalFRamRead(0x%08X,..,%u)", addr, length);

  if (!HalFramWaitReady(PLF_TIME_1SEC))  
    return NE_HAL_DRIVER_ERROR;

  const bool readOk = _READ(addr, b, length);
  return (readOk ? NE_OK : NE_HAL_DRIVER_ERROR);  
}


/***************************************************************************/
/* WRITE */

NvmErrorStatus_t HalFramWriteBlock(
  int32_t addr,
  uint8_t *b,
  uint16_t length)  
//uint32_t addr, const uint8_t * buffer, uint32_t len, bool waitLastReady)
{  
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalFRamWrite(0x%08X,..,%u)", addr, length);

  if (!HalFramWaitReady(PLF_TIME_1SEC))  
    return NE_HAL_DRIVER_ERROR;

  if (!_WREN()) 
    return NE_HAL_DRIVER_ERROR;

  const bool writeOk = _WRITE(addr, b, length);
  return (writeOk ? NE_OK : NE_HAL_DRIVER_ERROR);  
}


/***************************************************************************/

bool framOK;
NvmErrorStatus_t HalFramInit(void)
{
//  uint32_t framId;
  uint8_t sr;
  //TRACE_VA(TRC_TA_HAL, TRC_TL_COMPONENT, "HalFRamInit() - %u KB used", ((512+sizeof(FRamMap_t)) / 1024));
  
  SPI_FRAM_DESELECT();
  
  _RDSR(&sr);

  framOK = true;
  return (framOK ? NE_OK : NE_HAL_DRIVER_ERROR);
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalFramShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  if (!framOK) {
    CliPrintf("  FRAM not ok" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }

  if (cliShowHeader) {
	  CliWriteLine();
	  CliWrite("| HAL FRAM ...");  CliWriteEol();
	  CliWriteLine();
  }
  
  CliPrintf("| FRAM Size:     %u byte (0x%08X)" ,  HAL_FRAM_SIZE, HAL_FRAM_SIZE); CliWriteEol();
  CliWriteLine();
  
  return CLI_RESULT_OK;

}

#define READ_LINE_LENGTH (16)
int_fast16_t CliFRamRead(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  if (!framOK) {
    CliPrintf("FRAM not ok" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
  
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
    if (HalFramReadBlock(addr, data.w8, READ_LINE_LENGTH)) {
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
      CliPrintf("" CLI_NL);
    } else {
      CliWriteLn("Chip Read error");    
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    addr += READ_LINE_LENGTH;
    length -= READ_LINE_LENGTH;
    if (length < READ_LINE_LENGTH)
      break;    
  }
  
  return CLI_RESULT_OK;
}

int_fast16_t CliFRamWrite(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  if (!framOK) {
    CliPrintf("FRAM not ok" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
  uint16_t length = MIN_VAL((uint16_t)param3, 16);
  uint16_t i;
  uint8_t data[16];

  for (i = 0; i < length; i++) {
    data[i] = (uint8_t)param2;
  }

  if (HalFramWriteBlock((uint32_t)param1, data, length)) {
    return CLI_RESULT_OK;
  } else {
    CliWriteLn("Chip Write error");    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}


CLI_START_TABLE(hal_fram)

  CLI_ENTRY0( "show", "Show FRAM data", TstHalFramShow)

  CLI_ENTRY3( "rd", "Read from DF <addr> <len> <width: 1, 2, 4, 8>", CliFRamRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY3( "wr", "Write to DF <addr> <byte> <cnt (max 16)>", CliFRamWrite, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)


#ifdef TRC_ENABLE
	CLI_SUBTEST("trc",      "Trace system", trc)
#endif
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_fram)

/******************************************************************************/
#endif
/******************************************************************************/

