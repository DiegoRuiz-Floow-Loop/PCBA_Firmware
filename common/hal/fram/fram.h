/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_FRAM_H
#define HAL_FRAM_H

#ifdef __cplusplus
extern "C" {
#endif


#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/nvm/nvm.h"

/******************************************************************************/
  
#define HAL_FRAM_SIZE       (32*1024) /* 32 KByte */
#if defined(EVK)
#define HAL_FRAM_BUF_SIZE   (64)      /* max data in onw write or read */
#else
#define HAL_FRAM_BUF_SIZE   (256)     /* max data in onw write or read */
#endif

/******************************************************************************/

extern NvmErrorStatus_t HalFramWriteBlock(
  int32_t addr,
  uint8_t *b,
  uint16_t length);

extern NvmErrorStatus_t HalFramReadBlock(
  int32_t addr,
  uint8_t *b,
  uint16_t length);

extern NvmErrorStatus_t HalFramInit(void);

#ifdef CLI_ENABLE

  extern int_fast16_t TstHalFramShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);

#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // HAL_FRAM_H
