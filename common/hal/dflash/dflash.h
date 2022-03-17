/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef DFLASH_H
#define DFLASH_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************** INCLUDE FILES *******************************/
  
#include "hal/cfg/dflash_cfg.h"

#include "plf/plf.h"
#include "plf/cli/cli.h"
  
/*********************************** DEFINES **********************************/

typedef enum {
  DFLASH_OK,                  // HAL_OK (HAL_StatusTypeDef)
  DFLASH_HAL_ERROR,           // HAL_ERROR
  DFLASH_HAL_BUSY,            // HAL_BUSY
  DFLASH_HAL_TIMEOUT,         // HAL_TIMEOUT  
  DFLASH_DRIVER_ERROR,        // see: hspi.ErrorCode
  DFLASH_DATA_UNALLIGED,
  DFLASH_DATA_RANG_ERR,
  DFLASH_TIMEOUT,
  DFLASH_COMPARE_ERROR,
  DFLASH_Last
}                       DFlashState_t;

extern const char       * dFlashSTateName[];

/***************************************************************************/
/* ERASE */
// all (small) blocks from first to last will be erased...

/* ERASE in background task */
typedef void (*HalDFlashEraseAddrRangeCallback_t)(DFlashState_t dFlashState);
extern DFlashState_t  HalDFlashEraseAddrBackground(
  uint32_t              firstAddr, 
  uint32_t              lastAddr, 
  HalDFlashEraseAddrRangeCallback_t cb);

extern DFlashState_t HalDFlashEraseAddrRange(
  uint32_t              firstAddr, 
  uint32_t              lastAddr);

// DFLASH_SIZE
extern DFlashState_t HalDFlashEraseChip(
  bool                dflashWait);       
// DFLASH_BLOCK_SIZE
extern DFlashState_t HalDFlashEraseBlock(
  uint32_t              addr, 
  bool                dflashWait);      
// DFLASH_SMALL_BLOCK_SIZE
extern DFlashState_t HalDFlashEraseSmallBlock(
  uint32_t            addr, 
  bool              dflashWait);          

/***************************************************************************/
/* READ / WRITE / COPY */

extern DFlashState_t HalDFlashRead( 
  uint32_t addr, 
  uint8_t * buffer, 
  uint32_t len);

// Note, it alwayes check pending operation (device busy) before starting writing ..
extern DFlashState_t HalDFlashWrite(
  uint32_t addr, 
  const uint8_t * buffer, 
  uint32_t len,
  bool waitLastReady);  // wait for device not busy on last page(fraction) ??

extern DFlashState_t HalDFlashCopy(
  uint32_t srcAddr, 
  uint32_t dstAddr, 
  size_t size, 
  uint8_t * buffer, 
  size_t bufferSize);

extern DFlashState_t HalDFlashCompare(
  uint32_t addr1, 
  uint32_t addr2, 
  size_t size, 
  uint8_t * buffer1, 
  uint8_t * buffer2, 
  size_t bufferSize, 
  uint32_t * error);

/***************************************************************************/

extern void DFlashSpiCallback(void);

extern DFlashState_t HalDFlashInit(void);

/***************************************************************************/

#ifdef CLI_ENABLE

  extern int_fast8_t TstHalDFlashShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);
  extern int_fast8_t CliDFlashRead(CliParam_t param1, CliParam_t param2, CliParam_t param3);
  extern int_fast8_t CliFlashCompare(CliParam_t addr1, CliParam_t addr2, CliParam_t size);

#endif

/***************************************************************************/


#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* DFLASH_H */
