/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef NVM_H
#define NVM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

#include "plf/trc/trc.h"
#include "plf/crc/crc16.h"
#include "plf/cfg/nvm_cfg.h"


/****************************************************************************/


typedef enum {
  NE_OK,
  NE_HAL_DRIVER_ERROR,
  NE_Last
}               NvmErrorStatus_t;

extern NvmErrorStatus_t nvmErrorStatus;


typedef enum {
  NVM_OK,                              // == 0
  NVM_BOOTING,
  NVM_BOOTING2,
  NVM_NOT_READY,
  NVM_INITIALIZED,
  NVM_ERROR,
  NVM_Last
}                       NvmState_t;


extern volatile NvmState_t nvmState;

typedef char        MarkStr_t[8];

/****************************************************************************/

#ifdef NVM_ENABLE


#if (NVM_ACCESS == NVM_ACCESS_ADDRESS)

#pragma pack(push, 1)

typedef struct {
  MarkStr_t             beginMark;
  uint64_t              bootCount;
  
  Appl_t                appl;               /* APPLICATION -> nvm_config.h */

  #ifdef TRC_ENABLE
    TrcConfig_t         trcConfig;          /* trc.c */
  #endif

  MarkStr_t             endMark;
  Crc16_t               crc; // onle if used !!
}                       NvmStruct_t;

#pragma pack(pop)

extern void NvmWrite(
  uint_fast16_t        nvmAddr,
  void  const           * pData,
  uint_fast16_t         size);

extern void NvmRead(
  uint_fast16_t         nvmAddr,
  void                  * pData,
  uint_fast16_t         size);

#elif (NVM_ACCESS==NVM_ACCESS_ID)

#include "hal/pflash/pflash_nvm.h"

extern void NvmWriteIds(NvmId_t id, uint8_t * pui, int len);
extern bool NvmReadIds(NvmId_t id, uint8_t * pui, int len);

#define NvmWrite(id, val) NvmWriteIds(id, (uint8_t *)&val, sizeof(val))
#define NvmRead(id, val) NvmReadIds(id, (uint8_t *)&val, sizeof(val))

 
#endif
  
/****************************************************************************/

extern void NvmInit(void);



#endif /* NVM_ENABLE */

/*****************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* NVM_H */

