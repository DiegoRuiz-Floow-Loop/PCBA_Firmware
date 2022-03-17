/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/hal.h"
#include "hal/fram/fram.h"
#include "hal/cfg/fram_cfg.h"

#include "plf/plf.h"
#include "plf/blc/blc.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"
#include "plf/nvm/nvm.h"

#include "sif/sif.h"


/*******************************************************************************/

#define HAL_FRAM_ADDRESS    (0xA0) /* 0b1010 AAA0 - AAA = sug addr */
#define HAL_FRAM_TIMEOUT    (1000) /* 1 sec */

#if defined(BOOT_CONTROL_IN_FRAM)
  #define FRAM_OFFSET()       sizeof(BootControl_t)
#else
  #define FRAM_OFFSET()       0
#endif  
  
/*******************************************************************************/

static bool init = false;

#if (PLF_OS == PLF_OS_RTOS)

#include "plf/rtos/rtos.h"

RtosBinSemaphor_t				sem_i2c_2;

static void LockInit(void)
{
  if (sem_i2c_2 == NULL) {
    RTOS_BIN_SEMAPHOR_CREATE(sem_i2c_2);  		
  }
}
#define Lock() if (rtosRunning) (void)RTOS_BIN_SEMAPHOR_TAKE(sem_i2c_2, 1000)

#define Unlock() if (rtosRunning) (void)RTOS_BIN_SEMAPHOR_GIVE(sem_i2c_2)


#else  /* pure _EVOS */

  #define LockInit()    (void)0
  #define Lock()        (void)0
  #define Unlock()      (void)0

#endif

/*******************************************************************************/
  
  
static NvmErrorStatus_t DoHalFramWriteBlock(int32_t addr, uint8_t *b, uint16_t length)
{
	uint8_t buffer[2+HAL_FRAM_BUF_SIZE];
	HAL_StatusTypeDef ok;
  
  addr += FRAM_OFFSET();

  
  buffer[0] = (addr >> 8);  // HIGH ADDR
  buffer[1] = addr;         // LOW ADDR
	memcpy(&buffer[2], b, length);

	Lock();
  ok = HAL_I2C_Master_Transmit(FRAM_I2C, HAL_FRAM_ADDRESS, buffer, length+2, HAL_FRAM_TIMEOUT); 
	Unlock();
	
  if (ok != HAL_OK) {
    return NE_HAL_DRIVER_ERROR;
  }
  return NE_OK;
}

NvmErrorStatus_t HalFramWriteBlock(int32_t addr, uint8_t *b, uint16_t length)
{
  NvmErrorStatus_t nes;
  if (!init) {
    HalFramInit();
  }
	TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalFramWriteBlock(0x%X, ..., %u)", addr, length);		

  for (;;) {
    int len = MIN_VAL(length, HAL_FRAM_BUF_SIZE);
    nes = DoHalFramWriteBlock(addr, b, len);
    if (nes != NE_OK) break;
    length -= len;
    if (length <= 0) break;
    b += len;
    addr += len;
  }
  return nes;
}


/*******************************************************************************/

NvmErrorStatus_t HalFramReadBlock(int32_t addr, uint8_t *b, uint16_t length)
{
	HAL_StatusTypeDef ok;
	uint8_t buffer[2];
  
  addr += FRAM_OFFSET();

  if (!init) {
    HalFramInit();
  }
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalFramReadBlock(0x%X, ..., %u)", addr, length);		

  buffer[0] = (addr >> 8);  // HIGH ADDR
  buffer[1] = addr;         // LOW ADDR
	
	Lock();
  for (;;) {
    ok = HAL_I2C_Master_Transmit(FRAM_I2C, HAL_FRAM_ADDRESS, buffer, 2, HAL_FRAM_TIMEOUT); 
    if (ok != HAL_BUSY) break;
  }

  if (ok != HAL_OK) {
    Unlock();
    memset(b, 0, length);
    return NE_HAL_DRIVER_ERROR;    
  }

  for (;;) {
    ok = HAL_I2C_Master_Receive(FRAM_I2C, HAL_FRAM_ADDRESS, b, length, HAL_FRAM_TIMEOUT); 
    if (ok != HAL_BUSY) break;
  }
  Unlock();
	
  if (ok != HAL_OK) {
    memset(b, 0, length);
    return NE_HAL_DRIVER_ERROR;    
  }

  return NE_OK;
}

/*******************************************************************************/

NvmErrorStatus_t HalFramInit(void)
{
  if (!init) {
    LockInit();
    init = true;
  }
  return NE_OK;
}


/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalFramShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  if (cliShowHeader)
    CliWriteLn("HAL FRAM State...");
  
  CliPrintf("  Used Size:\t%u byte\t(0x%08X)" CLI_NL,  sizeof(NvmStruct_t), sizeof(NvmStruct_t)); 
  CliPrintf("  Total Size:\t%u byte\t(0x%08X)" CLI_NL,  HAL_FRAM_SIZE, HAL_FRAM_SIZE); 
  
  return CLI_RESULT_OK;
}


int_fast16_t CliFramRead(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint32_t adr = param1;
  uint32_t len = param2;
  uint32_t i;
  uint8_t data[HAL_FRAM_BUF_SIZE];
  
  if (len > HAL_FRAM_BUF_SIZE) {
    CliPrintf("Length to high (>%u)" CLI_NL, HAL_FRAM_BUF_SIZE);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  CliPrintf("Read FRAM(%u, %u)..." CLI_NL "\t", adr, len);
  if (HalFramReadBlock(adr, data, len) == 0) {
    for (i = 0; i < len; i++) {
      CliPrintf("%02x ", data[i]);
    }
    CliWrite(cliNewLine);
    return CLI_RESULT_OK;  
  } else {
    return CLI_RESULT_ERROR_UNDEFINED;
  }  
}

int_fast16_t CliFramWrite(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint8_t data[HAL_FRAM_BUF_SIZE];
  uint32_t adr = param1;
  uint16_t len = param3;

  if (len > HAL_FRAM_BUF_SIZE) {
    CliPrintf("Length to high (>%u)" CLI_NL, HAL_FRAM_BUF_SIZE);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }

  memset(data, param2, sizeof(data));

  if (HalFramWriteBlock(adr, data, len) == 0) {
    return CLI_RESULT_OK;
  } else {
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

CLI_START_TABLE(hal_fram)
  CLI_ENTRY0( "show", "Show State", TstHalFramShow)  
  CLI_ENTRY2( "rd", "Read from FR <addr> <len>", CliFramRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY3( "wr", "Write to FR <addr> <byte> <cnt>", CliFramWrite, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
	CLI_SUBTEST("trc",      "Trace system", trc)
  //CLI_ENTRY1( "spw",      "Set Password", CliPasswordSet, CLI_PARAM_STRING)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_fram)

#endif
