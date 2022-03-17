/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef CPX_H
#define CPX_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************/


#include "plf/plf.h"
#include "plf/blc/blc.h"


/******************************************************************************/

#if defined(BLC_USE_DFLASH)
  #define DFX_BUFFER   (DFLASH_PP_SIZE)
#elif defined(BLC_USE_PFLASH)
  #define DFX_BUFFER   (FLASH_PAGE_SIZE)
#else
  #error "FIX"
#endif

// buffer size: DFX_BUFFER
typedef bool (*SaveDataFunction_t)(uint32_t addr, uint8_t * buffer);

/******************************************************************************/

extern bool CpxUnpack(
  SaveDataFunction_t  saveDataFunction,
  uint32_t dstAddr, // used to forward current write "addr" to SaveDataFunction_t
  uint32_t dstSize,
  uint32_t srcAddr);

extern bool CpxUnpackToDFlash(
  uint32_t destAddr, 
  uint32_t dstSize,
  uint32_t srcAddr);

extern bool CpxUnpackToPFlash(
  uint32_t destAddr, 
  uint32_t dstSize,
  uint32_t srcAddr);

typedef enum {
  CDC_OK,
  CDC_UNPACK_ERROR,   // internal
  CDC_WRONG_SIZE,     // pFWDescriptor not found
  CDC_WRONG_ID,       // pFWDescriptor ->id wrong
  CDC_WRONG_CRC,      // pFWDescriptor ->crc wrong
  CDC_Last
}                 CpxDfuCheck_t;

#if defined(MAKE_CODE)
  const char * cdcErrorList[] = { "OK", "Err", "Size", "ID", "CRC" };
#endif

extern const char * cdcErrorList[];

extern CpxDfuCheck_t CpxDfuCheck(
  uint32_t  dFlashAddr, 
  FWDescriptor_t * fwd);



/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* CPX_H */
