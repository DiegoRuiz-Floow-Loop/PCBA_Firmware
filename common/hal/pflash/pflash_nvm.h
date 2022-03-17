/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_PFLASH_NVM
#define HAL_PFLASH_NVM

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"
#include "plf/cfg/nvm_cfg.h"
#include "plf/cli/cli.h"


/**************************************************************************/

extern bool HalPFlashNvmSet(
  uint_fast8_t  no,       // id for variable, 0..255
  void          * value,  // variable data
  uint_fast8_t  size      // size of data in bytes 1..HAL_PFLASH_NVM_MAX_VARIABLE_SIZE
);

extern bool HalPFlashNvmGet(
  uint_fast8_t  no,       // id for variable, 0..255
  void          * value,  // variable data
  uint_fast8_t  size      // size of data in bytes 1..HAL_PFLASH_NVM_MAX_VARIABLE_SIZE
);

extern void HalPFlashNvmErase(void);
  
extern bool HalPFlashNvmInit(void);
  
/**************************************************************************/
  
#ifdef CLI_ENABLE

	extern int_fast16_t TstHalFlashNvmShow(CliParam_t param1, CliParam_t param2, CliParam_t param3);

#endif

/**************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
