/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/hal.h"
#include "hal/dflash/dflash.h"
#include "hal/mcupin/mcupin.h"
#include "hal/dio/dio.h"

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"


#ifdef BOOTLOADER
  #define   HalWdtFeed()  (void)0
  #define   SupSystemErrorDetected(a, b, c, d) (void)0
  #include "plf/sio/sio.h"
#else
  #include "hal/wdt/wdt.h"
#if (PLF_OS == PLF_OS_RTOS)    
  #include "plf/rtos/rtos.h"
#else
  #include "sif/sif.h"
#endif
#endif

extern SPI_HandleTypeDef hspi1;


/***************************************************************************/
  
const char * dFlashSTateName[DFLASH_Last] = {
  "OK",
  "HAL ERROR",
  "HAL BUSY",
  "HAL TIMEOUT",
  "HAL DRIVER ERROR",
  "DATA UNALLIGNED",
  "DATA_RANG_ERR",
  "TIMEOUT",
};

/*******************************************************************************/

/* Device Locking */


#if (PLF_OS == PLF_OS_RTOS)
  #include "plf/rtos/rtos.h"

  static RtosBinSemaphor_t				semDFlash = NULL;

  static void LockInit(void)
  {
    if (semDFlash == NULL) {
      RTOS_BIN_SEMAPHOR_CREATE(semDFlash);  		
    }
  }
  static void Lock(void)
  {  
    if (rtosRunning)
      (void)RTOS_BIN_SEMAPHOR_TAKE(semDFlash, 1000);
  }
  static void Unlock(void)
  {
    if (rtosRunning)
      (void)RTOS_BIN_SEMAPHOR_GIVE(semDFlash);
  }


#else  /* pure _EVOS */

  #define LockInit()    (void)0
  #define Lock()        (void)0
  #define Unlock()      (void)0
  #define rtosRunning   false
  #define RTOS_TASK_DELAY(x) (void)0

#endif

/***************************************************************************/

/* LOW LEVEL I/O operation */

#define SPI_TIMEOUT     (1000)

#if defined(EVK)  
#define SPI_SELECT()    HAL_DIO_PIN_CLR(MP_SPI_DFLASH_CS)
#define SPI_DESELECT()  HAL_DIO_PIN_SET(MP_SPI_DFLASH_CS)
#else
#define SPI_SELECT()    HAL_DIO_PIN_CLR(MP_nCS_FLASH)
#define SPI_DESELECT()  HAL_DIO_PIN_SET(MP_nCS_FLASH)
#endif  

/***************************************************************************/

static volatile bool ioPending = false;

void DFlashSpiCallback(void)
{
  ioPending = false;
}

#define SPI_WRITE_IT(data, len) { \
  ioPending = true;\
  HAL_SPI_Transmit_IT(&SPI_DFLASH, data, len);\
  while (ioPending) ;\
}
#define SPI_READ_IT(data, len) { \
  ioPending = true;\
  HAL_SPI_Receive_IT(&SPI_DFLASH, data, len);\
  while (ioPending) ;\
}

#define SPI_WRITE_READ_IT(txData, rxData, size) { \
  ioPending = true;\
  HAL_SPI_TransmitReceive_IT(&SPI_DFLASH, txData, rxData, size);\
  while (ioPending) ;\
}

/***************************************************************************/

// Write Enable
static DFlashState_t _WREN(void) 
{
  uint8_t xx[8];
  xx[0] = 0x06;
  SPI_SELECT();
  if (rtosRunning) {
    SPI_WRITE_IT(xx, 1);
  } else {
    DFlashState_t s;
    do {
      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, 1, SPI_TIMEOUT);
    } while (s == HAL_BUSY);
  }
  SPI_DESELECT();
  if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  return DFLASH_OK;
}

// Write Disable - PAS PAA !!!
//static int_fast8_t _WRDI(void) 
//{
//  HAL_StatusTypeDef s;
//  uint8_t xx[8];
//  xx[0] = 0x04;
//  SPI_SELECT();
//  do {
//    s = HAL_SPI_Transmit(SPI, xx, 1, SPI_TIMEOUT);
//  } while (s == HAL_BUSY);
//  SPI_DESELECT();
//  return -1*s;
//}

// Status Register
#define SR_WIP(xx)      ((xx >> 0) & 1)
#define SR_WEL(xx)      ((xx >> 1) & 1)
#define SR_BP0(xx)      ((xx >> 2) & 1)
#define SR_BP1(xx)      ((xx >> 3) & 1)
#define SR_BP2(xx)      ((xx >> 4) & 1)
#define SR_SRWD(xx)     ((xx >> 7) & 1)

#define SR_BP(xx)       ((xx >> 2) & 7)

// Read Status Register
static DFlashState_t _RDSR(uint8_t * reg) 
{
//  DFlashState_t s;
  uint8_t xx = 0x05;
  *reg = 0xAA;
  SPI_SELECT();
//  if (rtosRunning) {
    //SPI_WRITE_READ_IT(xx, reg, 2);  
    SPI_WRITE_IT(&xx, 1);
    SPI_READ_IT(reg, 1);
    SPI_DESELECT();
    if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
//  } else {
//    do {
//      s = (DFlashState_t)HAL_SPI_TransmitReceive(&SPI_DFLASH, &xx, reg, 1+1, SPI_TIMEOUT);
////      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, 1, SPI_TIMEOUT);
////      s = (DFlashState_t)HAL_SPI_Receive(&SPI_DFLASH, reg, 1, SPI_TIMEOUT);
//    } while (s == HAL_BUSY);
//    SPI_DESELECT();
//    if (s != HAL_OK)  return DFLASH_DRIVER_ERROR;
//  }
  return DFLASH_OK;
}

// Read Data Byte
static DFlashState_t _FastRead(uint32_t address, uint8_t * data, uint_fast16_t length) 
{
  DFlashState_t s;
  uint8_t xx[DFLASH_PP_SIZE];

#if (DFLASH_SIZE <= (16 * 1024 * 1024))
  // 24-bit addressing, up to 16 MB
  int len = 5;
  xx[0] = 0x0B;
  xx[1] = (address >> 16);
  xx[2] = (address >> 8);
  xx[3] = address;
  xx[4] = 0; // dummy byte
#else
  // 32-bit addressing, greater than 16 MB
  int len = 6;
  xx[0] = 0x0C;
  xx[1] = (address >> 24);
  xx[2] = (address >> 16);
  xx[3] = (address >> 8);
  xx[4] = address;
  xx[5] = 0; // dummy byte
#endif

  SPI_SELECT();
  if (rtosRunning) {
    SPI_WRITE_IT(xx, len);
    SPI_READ_IT(xx, length);
    SPI_DESELECT();
    if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  } else {
    do {
      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, len, SPI_TIMEOUT);
      s = (DFlashState_t)HAL_SPI_Receive(&SPI_DFLASH, xx, length, SPI_TIMEOUT);
    } while (s == HAL_BUSY);
    SPI_DESELECT();
    if (s != HAL_OK)  return DFLASH_DRIVER_ERROR;
  }
  memcpy(data, xx, length);
  return DFLASH_OK;
}

// Write Data Byte
static DFlashState_t _PP(uint32_t address, uint8_t * data, uint32_t length) 
{
  //DFlashState_t s;
  uint8_t xx[DFLASH_PP_SIZE];

#if (DFLASH_SIZE <= (16 * 1024 * 1024))
  // 24-bit addressing, up to 16 MB
  int len = 4;
  xx[0] = 0x02;
  xx[1] = (address >> 16);
  xx[2] = (address >> 8);
  xx[3] = address;
#else
  // 32-bit addressing, greater than 16 MB
  int len = 5;
  xx[0] = 0x12;
  xx[1] = (address >> 24);
  xx[2] = (address >> 16);
  xx[3] = (address >> 8);
  xx[4] = address;  
#endif

  SPI_SELECT();
  if (rtosRunning) {
    SPI_WRITE_IT(xx, len)
    memcpy(xx, data, length);
    SPI_WRITE_IT(xx, length)
  } else {
    DFlashState_t s;
    do {
      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, len, SPI_TIMEOUT);
    } while (s == HAL_BUSY);
    memcpy(xx, data, length);
    do {
      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, length, SPI_TIMEOUT);
    } while (s == HAL_BUSY);
  }
  SPI_DESELECT();
  if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  return DFLASH_OK;
}

// Sector Erase  - 0x20
static DFlashState_t _EraseBlockSmalest(uint32_t sector)
{
  // 32-bit adressing not supported for this flash command (0x20).
  // Ensure that the application doesn't use more than 16 MB which is max for 24-bit addressing.
  static_assert(sizeof(DFlashMap_t) <= (16 * 1024 * 1024), "DFlash driver can't erase all of data flash used!");

//  DFlashState_t s;
  uint8_t xx[8];

  // 24-bit addressing, up to 16 MB
  int len = 4;
  xx[0] = 0x20;
  xx[1] = (sector >> 16);
  xx[2] = (sector >> 8);
  xx[3] = sector;

  SPI_SELECT();
  ioPending = true;
  if (rtosRunning) {
    HAL_SPI_Transmit_IT(&SPI_DFLASH, xx, len);
    for (;;) {
      if (!ioPending) break;
      RTOS_TASK_DELAY(1);
    };
  } else {
    DFlashState_t s;
    do {
      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, len, SPI_TIMEOUT);
    } while (s == HAL_BUSY);
  }
  SPI_DESELECT();
  if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  return DFLASH_OK;
}

static DFlashState_t _EraseBlock(uint32_t address)
{
  uint8_t xx[8];

#if (DFLASH_SIZE <= (16 * 1024 * 1024))
  // 24-bit addressing, up to 16 MB
  int len = 4;
  xx[0] = 0xD8;
  xx[1] = (address >> 16);
  xx[2] = (address >> 8);
  xx[3] = address;  
#else
  // 32-bit addressing, greater than 16 MB
  int len = 5;
  xx[0] = 0xDC;
  xx[1] = (address >> 24);
  xx[2] = (address >> 16);
  xx[3] = (address >> 8);
  xx[4] = address;  
#endif
  
  SPI_SELECT();
  ioPending = true;
  HAL_SPI_Transmit_IT(&SPI_DFLASH, xx, len);
  for (;;) {
    #if (PLF_OS == PLF_OS_RTOS)
      if (rtosRunning)
        RTOS_TASK_DELAY(1);
    #endif
    if (!ioPending) break;
  };
  SPI_DESELECT();  
  if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  return DFLASH_OK;
}

// Chip Erase	- 0xC7
static DFlashState_t _EraseChip(void)
{
  uint8_t xx[8];
  xx[0] = 0xC7;
 
  SPI_SELECT();
  ioPending = true;
  HAL_SPI_Transmit_IT(&SPI_DFLASH, xx, 1);
  for (;;) {
    #if (PLF_OS == PLF_OS_RTOS)
      if (rtosRunning)
        RTOS_TASK_DELAY(1);
    #endif
    if (!ioPending) break;
  };
  SPI_DESELECT();

  if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
  return DFLASH_OK;
}

//AT25SF

// Read Manufacture ID/ Device ID
static DFlashState_t _REMS(uint32_t * u)
{
  uint8_t xx[8];
  uint8_t yy[8];
  memset(yy, 0x55, sizeof(yy));
  memset(xx, 0xAA, sizeof(xx));
  xx[0] = 0x9F;

  SPI_SELECT();
//  if (rtosRunning) {
  SPI_WRITE_READ_IT(xx, yy, 4);
//SPI_WRITE_IT(xx, 4);
//  SPI_READ_IT(yy, 3);
  SPI_DESELECT();
    if (SPI_DFLASH.ErrorCode != HAL_SPI_ERROR_NONE) return DFLASH_DRIVER_ERROR;
//  } else {
//    DFlashState_t s;
//    do {
//      s = (DFlashState_t)HAL_SPI_Transmit(&SPI_DFLASH, xx, 4, SPI_TIMEOUT);
//      s = (DFlashState_t)HAL_SPI_Receive(&SPI_DFLASH, yy, 3, SPI_TIMEOUT);
//    } while (s == HAL_BUSY);
//    SPI_DESELECT();
//    if (s != HAL_OK) return DFLASH_DRIVER_ERROR;
//  }
  *u = ((uint32_t)yy[1] << 16) | ((uint32_t)yy[2] << 8) | yy[3];
  return DFLASH_OK;
}


/***************************************************************************/
/* STATUS */

static DFlashState_t HalDFlashWaitReady(uint32_t waitTime)
{
  DFlashState_t  st;
  PlfTime_t tStart=0, tNow;
  
  for (;;) {
		uint8_t reg = 0xFF;
    
    for (;;) {
      st = _RDSR(&reg);
      if (st == HAL_OK) break;
      if (st != HAL_BUSY) {
        return st;
      }
#if (PLF_OS == PLF_OS_RTOS)
      else {
        RTOS_TASK_DELAY(1);
      }
#endif
      break;
    }

    if (SR_WIP(reg) == 0) 
      return DFLASH_OK;
    
    if (tStart == 0)
      PlfTimeMsGet(&tStart);
    
    PlfTimeMsGet(&tNow);
    if (TIME_DIF(tNow, tStart) > waitTime) 
      return DFLASH_TIMEOUT;
    
    HalWdtFeed();
  }
}

/***************************************************************************/
/* ERASE */

DFlashState_t HalDFlashEraseChip(bool  dflashWait)
{
  DFlashState_t  st;
  
  Lock();
  st = HalDFlashWaitReady(5*60*PLF_TIME_1SEC);
  if (st == DFLASH_OK) {
    st = _WREN();
    if (st != DFLASH_OK)  {
      Unlock();
      return st;
    }
    
    st = _EraseChip();
    if (st != DFLASH_OK)  {
      Unlock();
      return st;
    }
    
    // _WRDI(); - automatically set when erased
    if (dflashWait) 
      st = HalDFlashWaitReady(DFLASH_CHIP_ERASE_TIMEOUT); // 
  }
  Unlock();
  return st;
}

DFlashState_t HalDFlashEraseBlock(uint32_t addr, bool  dflashWait)
{
  DFlashState_t  st;
  
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalDFlashEraseBlock(0x%08X)", addr);

  Lock();
  st = HalDFlashWaitReady(5*PLF_TIME_1SEC);
  if (st == DFLASH_OK) {
    st = _WREN();
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
    
    st = _EraseBlock(addr);
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }

    // _WRDI(); - automatically set when erased
    if (dflashWait) 
      st = HalDFlashWaitReady(DFLASH_BLOCK_ERASE_TIMEOUT);
  }
  Unlock();
  return st;
}

DFlashState_t HalDFlashEraseSmallBlock(uint32_t addr, bool  dflashWait)
{
  DFlashState_t  st;
  TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HalDFlashEraseSmallBlock(0x%08X)", addr);
  Lock();
  st = HalDFlashWaitReady(5*PLF_TIME_1SEC);
  if (st == DFLASH_OK) {
    st = _WREN();
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
    
    st = _EraseBlockSmalest(addr);
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }

    // _WRDI(); - automatically set when erased
    if (dflashWait) 
      st = HalDFlashWaitReady(DFLASH_BLOCK_ERASE_TIMEOUT); // 120 msec normal
  }
  Unlock();
  return st;
}

DFlashState_t HalDFlashEraseAddrRange(uint32_t addr1, uint32_t addr2)
{
	DFlashState_t st;
  int32_t addrDiff;
  
  HalWdtFeed();
  addr1 = (addr1 / DFLASH_SMALL_BLOCK_SIZE) * DFLASH_SMALL_BLOCK_SIZE;   
  addrDiff = (int32_t)(addr2 - addr1 + 1);

  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalDFlashEraseAddrRange(0x%08X, 0x%08X)", addr1, addr2);
  
  Lock();
  st = HalDFlashWaitReady(10*PLF_TIME_1SEC);
  if (st == DFLASH_OK) {
    
    while ((addrDiff > 0) && (st == DFLASH_OK)) {
      HalWdtFeed();
      
      // First Large Blocks
      if ((addrDiff >= DFLASH_BLOCK_SIZE) && ((addr1 % DFLASH_BLOCK_SIZE)==0)) {
        st = _WREN();
        if (st != DFLASH_OK) 
          goto ExitHalDFlashEraseAddrRange;
        st = HalDFlashEraseBlock(addr1, true); // log itself!!
        if (st != DFLASH_OK) 
          goto ExitHalDFlashEraseAddrRange;
        addr1 += DFLASH_BLOCK_SIZE;
        
      // then Small Blocks
      } else {
        st = _WREN();
        if (st != DFLASH_OK) 
          goto ExitHalDFlashEraseAddrRange;
        st = HalDFlashEraseSmallBlock(addr1, true); // log itself!!
        if (st != DFLASH_OK) 
          goto ExitHalDFlashEraseAddrRange;
        addr1 += DFLASH_SMALL_BLOCK_SIZE;
      }      
      addrDiff = (int32_t)(addr2 - addr1 + 1);
    }
  }

ExitHalDFlashEraseAddrRange:  
  Unlock();  
  HalWdtFeed();
	return st;
}

/***************************************************************************/
/* ERASE in background task */

#if !defined(BOOTLOADER)

static uint32_t halDFlashEraseAddr1;
static uint32_t halDFlashEraseAddr2;
static HalDFlashEraseAddrRangeCallback_t halDFlashEraseAddrRangeCallback = NULL;

#if (PLF_OS == PLF_OS_RTOS)

//static RtosBinSemaphor_t eraseSemaphor = NULL;

static void DFlashEraseTask(void * argument)
{
  DFlashState_t dhs =  HalDFlashEraseAddrRange(halDFlashEraseAddr1, halDFlashEraseAddr2);
  if (halDFlashEraseAddrRangeCallback) {
    halDFlashEraseAddrRangeCallback(dhs);
  }
  RTOS_TASK_DELETE(NULL);
}
DFlashState_t  HalDFlashEraseAddrBackground(uint32_t firstAddr, uint32_t lastAddr, HalDFlashEraseAddrRangeCallback_t cb)
{
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalDFlashEraseAddrBackground(0x%08X, 0x%08X)", firstAddr, lastAddr);
  halDFlashEraseAddrRangeCallback = cb;
  halDFlashEraseAddr1 = firstAddr;
  halDFlashEraseAddr2 = lastAddr;  
  RTOS_TASK_CREATE(DFlashEraseTask, "DFlashEraseTask", RTOS_STACK_SIZE_EXTRA_DFE, RTOS_PRIORITY_DFE);
  return DFLASH_OK;
}

#else

static EvosEventHandle_t dFlashEraseTask = EVOS_UNINITIALIZED_HANDLE;
//static uint8_t    step;

static void DFlashEraseTask(EvosEventParam_t p)
{
	DFlashState_t st;
  int32_t addrDiff;
  
  HalWdtFeed();
  addrDiff = (int32_t)(halDFlashEraseAddr2 - halDFlashEraseAddr1 + 1);

  st = HalDFlashWaitReady(10*PLF_TIME_1SEC);
  if (st != DFLASH_OK) {
     halDFlashEraseAddrRangeCallback(st);
  }

  if (addrDiff > 0) {
    HalWdtFeed();
    
    // First Large Blocks
    if ((addrDiff >= DFLASH_BLOCK_SIZE) && ((halDFlashEraseAddr1 % DFLASH_BLOCK_SIZE)==0)) {
      st = _WREN();
      if (st != DFLASH_OK) {
        halDFlashEraseAddrRangeCallback(st);
        return;
      }
      st = HalDFlashEraseBlock(halDFlashEraseAddr1, true); // log itself!!
      if (st != DFLASH_OK) {
        halDFlashEraseAddrRangeCallback(st);
        return;
      }
      halDFlashEraseAddr1 += DFLASH_BLOCK_SIZE;
      
    // then Small Blocks
    } else {
      st = _WREN();
      if (st != DFLASH_OK) {
        halDFlashEraseAddrRangeCallback(st);
        return;
      }
      st = HalDFlashEraseSmallBlock(halDFlashEraseAddr1, true); // log itself!!
      if (st != DFLASH_OK) {
        halDFlashEraseAddrRangeCallback(st);
        return;
      }
      halDFlashEraseAddr1 += DFLASH_SMALL_BLOCK_SIZE;
    }      
    addrDiff = (int32_t)(halDFlashEraseAddr2 - halDFlashEraseAddr1 + 1);
    EvosEventSetNow(dFlashEraseTask, 0);
  } else {
    halDFlashEraseAddrRangeCallback(DFLASH_OK);
  }
}

DFlashState_t  HalDFlashEraseAddrBackground(uint32_t firstAddr, uint32_t lastAddr, HalDFlashEraseAddrRangeCallback_t cb)
{
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalDFlashEraseAddrBackground(0x%08X, 0x%08X)", firstAddr, lastAddr);
//  step = 0;
  halDFlashEraseAddr1 = (firstAddr / DFLASH_SMALL_BLOCK_SIZE) * DFLASH_SMALL_BLOCK_SIZE;   
  halDFlashEraseAddr2 = lastAddr;
  halDFlashEraseAddrRangeCallback = cb;

  if (dFlashEraseTask == EVOS_UNINITIALIZED_HANDLE) {
    dFlashEraseTask = EvosEventRegister(DFlashEraseTask, "DFlashEraseTask");
  }
  EvosEventSetNow(dFlashEraseTask, 0);

  return DFLASH_OK;
}

#endif

#endif


/***************************************************************************/
/* READ */

DFlashState_t HalDFlashRead(uint32_t addr, uint8_t * buffer, uint32_t len)
{
	DFlashState_t st;
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalDFlashRead(0x%08X,..,%u)", addr, len);
	HalWdtFeed();

  Lock();
  // FIRST bit
	{
    int  lf = MIN_VAL(len, (((addr+DFLASH_PP_SIZE)/DFLASH_PP_SIZE)*DFLASH_PP_SIZE)-addr);    
		st = HalDFlashWaitReady(PLF_TIME_1SEC);
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
    //TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HDFR - first: 0x%X,0x%X,%u", addr, buffer, lf);
		st = _FastRead(addr, buffer, lf);
		len -= lf;
		addr += lf;
		buffer += lf;
	}
  // MIDT pages
  while (len >= DFLASH_PP_SIZE) {
		st = HalDFlashWaitReady(PLF_TIME_1SEC);
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
		st = _FastRead(addr, buffer, DFLASH_PP_SIZE);
		len -= DFLASH_PP_SIZE;
		addr += DFLASH_PP_SIZE;
		buffer += DFLASH_PP_SIZE;
	}
  // LAST bit
  if (len > 0) {
		st = HalDFlashWaitReady(PLF_TIME_1SEC);
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
		//TRACE_VA(TRC_TA_HAL, TRC_TL_4, "HDFR - last: 0x%X,0x%X,%u", addr, buffer, len);
		st = _FastRead(addr, buffer, len);
	}
  Unlock();
	HalWdtFeed();

  return st;
}


/***************************************************************************/
/* WRITE */

DFlashState_t HalDFlashWrite(uint32_t addr, const uint8_t * buffer, uint32_t len, bool waitLastReady)
{
  DFlashState_t  st;  
  
  TRACE_VA(TRC_TA_HAL, TRC_TL_3, "HalDFlashWrite(0x%08X,..,%u)", addr, len);
  HalWdtFeed();
  
  Lock();
  // FIRST bit
  {
    // first bit == MIN(len;(HELTAL((addr+page)/page)*page)-addr)
    int  lf = MIN_VAL(len, (((addr+DFLASH_PP_SIZE)/DFLASH_PP_SIZE)*DFLASH_PP_SIZE)-addr);    
		st = HalDFlashWaitReady(PLF_TIME_1SEC);
    st = _WREN();
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
		//TRACE_VA(TRC_TA_HAL, TRC_TL_4, "DFW - first: 0x%X,%u", addr, lf);
    st = _PP(addr, (uint8_t *)buffer, lf);  
    len -= lf;
    addr += lf;
    buffer += lf;
    if (st != DFLASH_OK) return st;
  }
  // MIDT pages
  while (len >= DFLASH_PP_SIZE) {
    st = HalDFlashWaitReady(PLF_TIME_1SEC);
    st = _WREN(); 
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
		//TRACE_VA(TRC_TA_HAL, TRC_TL_4, "DFW - midt: 0x%X,%u", addr, DFLASH_PP_SIZE);
    st = _PP(addr, (uint8_t *)buffer, DFLASH_PP_SIZE); 
    len -= DFLASH_PP_SIZE;    
    addr += DFLASH_PP_SIZE;
    buffer += DFLASH_PP_SIZE;
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
    //st = HalDFlashWaitReady(PLF_TIME_1SEC);
  }
  // LAST bit
  if (len > 0) {
    st = HalDFlashWaitReady(PLF_TIME_1SEC);
    st = _WREN();
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
		//TRACE_VA(TRC_TA_HAL, TRC_TL_4, "DFW - last: 0x%X,%u", addr, len);
    st = _PP(addr, (uint8_t *)buffer, len);  
    if (st != DFLASH_OK) {
      Unlock();
      return st;
    }
  }
  if (waitLastReady)
    st = HalDFlashWaitReady(PLF_TIME_1SEC);
  
  Unlock();  
  HalWdtFeed();
  return st;  
}


/***************************************************************************/
/* COPY */
DFlashState_t HalDFlashCopy(uint32_t srcAddr, uint32_t dstAddr, size_t size, uint8_t * buffer, size_t bufferSize)
{
  DFlashState_t  dfs;

  dfs = HalDFlashEraseAddrRange(dstAddr, dstAddr+size-1);
  if (dfs != DFLASH_OK) 
    return dfs;
  
  while (size > 0) {
    size_t s = MIN_VAL(size, bufferSize);
    dfs = HalDFlashRead(srcAddr, buffer, s);
    if (dfs != DFLASH_OK) 
      return dfs;
    dfs = HalDFlashWrite(dstAddr, buffer, s, false);
    if (dfs != DFLASH_OK) 
      return dfs;
    srcAddr += s;
    dstAddr += s;
    size -= s;
  }
  return dfs;
}


DFlashState_t HalDFlashCompare(uint32_t addr1, uint32_t addr2, size_t size, uint8_t * buffer1, uint8_t * buffer2, size_t bufferSize, uint32_t * error)
{
  DFlashState_t  dfs;
  size_t s;
  uint32_t i;
  *error = 0;
  while (size > 0) {
    s = MIN_VAL(size, bufferSize);
    dfs = HalDFlashRead(addr1, buffer1, s);
    if (dfs != DFLASH_OK) 
      return dfs;
    dfs = HalDFlashRead(addr2, buffer2, s);
    if (dfs != DFLASH_OK) 
      return dfs;

    for (i=0; i<s; i++) {
      if (buffer1[i] != buffer2[i]) {
        *error = addr1;
        return DFLASH_COMPARE_ERROR;
      }
    }

    addr1 += s;
    addr2 += s;
    size -= s;
  }
  return dfs;
  
}

/***************************************************************************/

#define DFLASH_32MBIT_ID    0x1F8701    // adesto, 	AT25SF321
#define DFLASH_128MBIT_ID   0x1F8901    // adesto, 	AT25SF128
#define DFLASH_512MBIT_ID   0x20BA20    // Micron,  MT25QL512ABB


extern char * Hex2Str(uint32_t x, int minWidth, char * s);

DFlashState_t HalDFlashInit(void)
{
  uint32_t dataFlashMDI;
#ifdef BOOTLOADER
  char buf[16];
#endif
  TRACE_VA(TRC_TA_HAL, TRC_TL_COMPONENT, "HalDFlashInit(), 0x%X used", sizeof(DFlashMap_t));
  if (sizeof(DFlashMap_t) > ((DFLASH_SIZE_MBIT * 1024* 1024) / 8)) {
    TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, "DFLASH to small, need: 0x%08X (%u)", sizeof(DFlashMap_t), sizeof(DFlashMap_t));
    //for (;;) ;
  }
      
  LockInit();

  SPI_DESELECT();
	
  DFlashState_t er = _REMS(&dataFlashMDI);
  if (er != DFLASH_OK) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HalDFlashInit() - ERROR");
    return er;
  }
	
#if (DFLASH_SIZE_MBIT==DFLASH_32MBIT)
  if (dataFlashMDI != DFLASH_32MBIT_ID) {  // AT25SF321
#elif (DFLASH_SIZE_MBIT==DFLASH_128MBIT)
  if (dataFlashMDI != DFLASH_128MBIT_ID) {
#elif (DFLASH_SIZE_MBIT==DFLASH_512MBIT)
  if (dataFlashMDI != DFLASH_512MBIT_ID) {
#else
  if (false) {
#endif

#ifdef BOOTLOADER
		SifWrite("\r\n**DFLASH wrong type: 0x");
    SifWriteLn(Hex2Str(dataFlashMDI, 6, &buf[sizeof(buf)]));
#else
    TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, "DFLASH wrong type (0x9F): 0x%06X", dataFlashMDI);
#endif
  }
  
  return DFLASH_OK;
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

static const char * DataFlashMDI_NameGet(uint32_t mid)
{
  const char * s;
  if (mid == DFLASH_32MBIT_ID) {
    static const char xx[] = "AT25SF321";
    s = xx;;
  } else if (mid == DFLASH_128MBIT_ID) {
    static const char xx[] = "AT25SF128";
    s = xx;;
  } else if (mid == DFLASH_512MBIT_ID) {
    static const char xx[] = "MT25QL512AB";
    s = xx;;
  } else {
    static const char xx[] = "unknown/missing";
    s = xx;;
  }  
  return  s;
}

int_fast8_t TstHalDFlashShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  UNUSED(param1);
  UNUSED(param2);
  UNUSED(param3);
  
  //uint8_t xx[8];
  uint32_t u;
  
  if (cliShowHeader)
    CliWrite("HAL DFLASH State..." CLI_NL); 

  // Manufacture ID/ Device ID
  _REMS(&u);  // first time after boot read garbage...
  _REMS(&u);
  CliPrintf("  Man/Dev ID:\t0x%06X (%s)" CLI_NL,  u, DataFlashMDI_NameGet(u)); 


  // Status
  uint8_t c;
  _RDSR(&c);
  CliPrintf("  Status:\t0x%02x" CLI_NL, c); 
//return CLI_RESULT_OK;
  if (c != 0) {
    char sz[80];
    strcpy(sz, "  ");
    if (SR_WIP(c))  
      strcat(sz, "/Write in Progress"); 
    if (SR_SRWD(c)) 
      strcat(sz, "/Write Disable"); 
    CliWriteLn(sz); 
  }

  CliPrintf("  Total Size:\t%u byte\t(0x%08X)" CLI_NL,  DFLASH_SIZE, DFLASH_SIZE); 
  CliPrintf("  Block Size:\t%u byte\t(0x%08X)" CLI_NL,  DFLASH_BLOCK_SIZE, DFLASH_BLOCK_SIZE); 
#if (DFLASH_SIZE_MBIT!=DFLASH_512MBIT)
  CliPrintf("  Small Blk Sz:\t%u byte\t(0x%08X)" CLI_NL,  DFLASH_SMALL_BLOCK_SIZE, DFLASH_SMALL_BLOCK_SIZE); 
#endif

  return CLI_RESULT_OK;

}

//static int_fast8_t CliFlashStatusWrite(CliParam_t param1, CliParam_t param2, CliParam_t param3)
//{
//  DFlashState_t s;
//  uint8_t reg = param1;
//  uint8_t r2;

//  _WREN();
//  
//  s = _WRSR(reg);
//  if (s == DFLASH_OK) {
//    _RDSR(&r2);
//    CliPrintf("Status: 0x%02x" CLI_NL, r2);
//    return CLI_RESULT_OK;
//  } else {
//    CliPrintf("Error: %s" CLI_NL, dFlashSTateName[s]);
//    return CLI_RESULT_ERROR_UNDEFINED;
//  }  
//}

#define READ_LINE_LENGTH (16)
int_fast8_t CliDFlashRead(CliParam_t param1, CliParam_t param2, CliParam_t param3)
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
  DFlashState_t s;

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
    s = HalDFlashRead(addr, data.w8, READ_LINE_LENGTH);
    if (s == DFLASH_OK) {
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
     
      break;
    }
    addr += READ_LINE_LENGTH;
    length -= READ_LINE_LENGTH;
    if (length < READ_LINE_LENGTH)
      break;    
  }
  
  if (s == DFLASH_OK) {
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Error: %s" CLI_NL, dFlashSTateName[s]);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

int_fast8_t CliFlashWrite(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint16_t length = MIN_VAL((uint16_t)param3, 16);
  uint16_t i;
  uint8_t data[16];
  DFlashState_t s;

  for (i = 0; i < length; i++) {
    data[i] = (uint8_t)param2;
  }

  s = HalDFlashWrite((uint32_t)param1, data, length, false);
  if (s == DFLASH_OK) {
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Chip Write error: %s" CLI_NL, dFlashSTateName[s]);    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

static int_fast8_t CliFlashEraseChip(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  DFlashState_t s;
  PlfTime_t t1, t2;
  PlfTimeMsGet(&t1);
  s = HalDFlashEraseChip(true);
  PlfTimeMsGet(&t2);
  if (s == DFLASH_OK) {
    CliPrintf("Chip Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(t2, t1));
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Chip Erase error: %s" CLI_NL, dFlashSTateName[s]);    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

static int_fast8_t CliFlashEraseBlock(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint32_t addr = (uint32_t)param1;
  DFlashState_t s;
  PlfTime_t t1, t2;

  if (addr >= DFLASH_SIZE) {
    CliPrintf("Sector is: [0..%u]" CLI_NL, DFLASH_SIZE-1);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
    
  PlfTimeMsGet(&t1);
  s = HalDFlashEraseBlock(addr, true);
  PlfTimeMsGet(&t2);
  if (s == DFLASH_OK) {
    CliPrintf("Sector Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(t2, t1));
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Chip Erase error: %s" CLI_NL, dFlashSTateName[s]);    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}

#if !defined(BOOTLOADER)

static PlfTime_t t1, t2;

static void HalDFlashEraseAddrRangeCallback(DFlashState_t dFlashState)
{
  PlfTimeMsGet(&t2);
  if (dFlashState == DFLASH_OK) {
    CliPrintf("Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(t2, t1));
  } else {
    CliPrintf("Erase error: %s" CLI_NL, dFlashSTateName[dFlashState]);    
  }
}

static int_fast8_t CliFlashEraseBG(CliParam_t firstAddr, CliParam_t lastAddr, CliParam_t param3)
{
  DFlashState_t s;
  PlfTimeMsGet(&t1);
  s = HalDFlashEraseAddrBackground(firstAddr, lastAddr, HalDFlashEraseAddrRangeCallback);

  if (s == DFLASH_OK) {
    CliPrintf("Erase started..." CLI_NL);
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Erase ERROR..." CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}
#endif


static int_fast8_t CliFlashErase(CliParam_t firstAddr, CliParam_t lastAddr, CliParam_t param3)
{
  DFlashState_t s;
  PlfTime_t tim1, tim2;

    
  PlfTimeMsGet(&tim1);
  s = HalDFlashEraseAddrRange(firstAddr, lastAddr);
  PlfTimeMsGet(&tim2);
  if (s == DFLASH_OK) {
    CliPrintf("Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(tim2, tim1));
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Erase error: %s" CLI_NL, dFlashSTateName[s]);    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}


#if (DFLASH_SIZE_MBIT!=DFLASH_512MBIT)
static int_fast8_t CliFlashEraseSmallBlock(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint32_t addr = (uint32_t)param1;
  PlfTime_t tim1, tim2;
  DFlashState_t s;

  if (addr >= DFLASH_SIZE) {
    CliPrintf("Block is: [0..%u]" CLI_NL, DFLASH_SIZE-1);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
    
  PlfTimeMsGet(&tim1);
  s = HalDFlashEraseSmallBlock(addr, true);
  if (s == DFLASH_OK) {
    PlfTimeMsGet(&tim2);
    CliPrintf("Block Erase, total " PRINTF_PLFTIME_INT " msec" CLI_NL, TIME_DIF(tim2, tim1));
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Chip Erase error: %s" CLI_NL, dFlashSTateName[s]);    
    return CLI_RESULT_ERROR_UNDEFINED;
  }
}
#endif


#define TEST_LEN      (DFLASH_PP_SIZE / 4)
#define TEST_STEPS    (100)
//#define TEST_STEPS    ((2*DFLASH_BLOCK_SIZE) / TEST_LEN)
#define TEST_START    (0x400)


static void InitBuf(int s, uint8_t * buf1, uint8_t *buf2)
{
  int i;
  for (i=0; i<TEST_LEN; i++) {
    buf1[i] = (s+i);
  }
  memset(buf2, s, TEST_LEN);
}

static int_fast8_t CliFlashTest(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint8_t buf1[TEST_LEN];
  uint8_t buf2[TEST_LEN];
  int i;
  int cc;
  uint32_t startAddr = TEST_START + 12;
  uint32_t endAddr = startAddr + (TEST_STEPS*TEST_LEN)-1;

  CliPrintf("Flast Test from 0x%0X to 0x%X" CLI_NL, startAddr, endAddr);

  //if (HalDFlashEraseBlock(0, true) != DFLASH_OK) {
  if (HalDFlashEraseAddrRange(startAddr, endAddr)  != DFLASH_OK) {
    CliPrintf("Erase failed" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }
  for (;;) {
    int s = 1;
    InitBuf(s, buf1, buf2);


    cc = HalDFlashWrite(startAddr, buf1, TEST_LEN, false);
    if (cc != DFLASH_OK) {
      CliPrintf("Write at 0x%X failed" CLI_NL, startAddr);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    
    cc = HalDFlashRead(startAddr, buf2, TEST_LEN);    
    if (cc != DFLASH_OK) {
      CliPrintf("Read at 0x%X failed" CLI_NL, startAddr);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    
    i = memcmp(buf1, buf2, TEST_LEN);
    if (i != 0) {
      CliPrintf("Verify at 0x%X failed" CLI_NL, startAddr);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    
    startAddr += TEST_LEN;
    if (startAddr >= endAddr-TEST_LEN)
      break;
    
  }
  
  if (HalDFlashEraseAddrRange(startAddr, endAddr)  != DFLASH_OK) {
  //if (HalDFlashEraseBlock(0, true) != DFLASH_OK) {
    CliPrintf("Erase Block 0 failed (#2)" CLI_NL);
    return CLI_RESULT_ERROR_UNDEFINED;
  }

  return CLI_RESULT_OK;

}


static int_fast8_t CliFlashTest2(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  uint8_t buf1[8];
  uint8_t buf2[8];
  uint32_t stepAddr;
  uint32_t addr;
  uint8_t step;
 
  
  CliPrintf("Flast Test 2:" CLI_NL);

  // Write...
  stepAddr = DFLASH_SIZE / 128;
  addr = stepAddr / 2;
  for (step=0; step<128; step++) {
    CliPrintf("  Now addr: 0x%08X - ", addr);
    memset(buf1, step, sizeof(buf1));
    memset(buf2, 0, sizeof(buf1));
    if (DFLASH_OK != HalDFlashWrite(addr, buf1, sizeof(buf1), false)) {
      CliPrintf("Write Error" CLI_NL);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    if (DFLASH_OK != HalDFlashRead(addr, buf2, sizeof(buf1))) {
      CliPrintf("Read Error" CLI_NL);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    if (memcmp(buf1, buf2, sizeof(buf1)) != 0) {
      CliPrintf("Data Error" CLI_NL);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
    CliWrite("OK" CLI_NL);
    addr += stepAddr;
  }
  CliWrite(cliNewLine);
  return CLI_RESULT_OK;

}


int_fast8_t CliFlashCompare(CliParam_t addr1, CliParam_t addr2, CliParam_t size)
{
  uint8_t buffer1[1024];
  uint8_t buffer2[1024];
  uint32_t error;
  DFlashState_t r = HalDFlashCompare(addr1, addr2, size, buffer1, buffer2, sizeof(buffer1), &error);
  if (r == DFLASH_OK) {
    CliPrintf("Compare OK" CLI_NL);
    return CLI_RESULT_OK;
  } else {
    CliPrintf("Compare Error, a1:0x%08X" CLI_NL, error);
    return CLI_RESULT_ERROR_UNDEFINED;
  }

}

CLI_START_TABLE(hal_dflash)

  CLI_ENTRY0( "show", "Show DFLASH data", TstHalDFlashShow)
  //CLI_ENTRY1( "wsr", "Write Status Register (excl SRWD)", CliFlashStatusWrite, CLI_PARAM_UINT32)

  CLI_ENTRY3( "rd", "Read from DF <addr> <len> <width: 1, 2, 4, 8>", CliDFlashRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  CLI_ENTRY3( "wr", "Write to DF <addr> <byte> <cnt (max 16)>", CliFlashWrite, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_ENTRY3( "cmp", "Compare <addr1> <addr2> <len>", CliFlashCompare, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_ENTRY2( "erase", "Erase [first addr] [last addr]", CliFlashErase, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
#if (DFLASH_SIZE_MBIT!=DFLASH_512MBIT)
	CLI_ENTRY1( "ersmb", "Erase Small Block [address in block]", CliFlashEraseSmallBlock, CLI_PARAM_UINT32)
#endif
  CLI_ENTRY1( "erblk", "Erase Block [address in block]", CliFlashEraseBlock, CLI_PARAM_UINT32)
	CLI_ENTRY0( "erchp", "Erase whole DFLASH", CliFlashEraseChip)

#if !defined(BOOTLOADER)
  CLI_ENTRY2( "erasebg", "Erase [first addr] [last addr] IN BACKGROUND TASK", CliFlashEraseBG, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
#endif

  CLI_ENTRY0( "test", "TEST R/W - sector 0", CliFlashTest)
  CLI_ENTRY0( "test2", "TEST R/W in chip", CliFlashTest2)

	CLI_SUBTEST("trc",      "Trace system", trc)
  //CLI_ENTRY1( "spw",      "Set Password", CliPasswordSet, CLI_PARAM_STRING)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_dflash)

/******************************************************************************/
#endif
/******************************************************************************/

