/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef BLC_H
#define BLC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "hal/cfg/pflash_cfg.h"
#include "plf/plf.h"
#include "plf/blc/blc_def.h"
#include "plf/cfg/blc_cfg.h"

/******************************************************************************/

extern BootControl_t          * pBootControl;

#if (PLF_OS!=PLF_OS_WINDOWS)

extern const FWDescriptor_t   * pFWDescriptor;

#endif

extern void ApplAfter_BlcActivateNewImage(void);


/******************************************************************************/

#if defined(BLC_USE_DFLASH)

extern int_fast8_t BlcWriteToDataFlash(uint32_t dFlashAddr, const void *buf, uint_fast16_t len);

extern int_fast8_t BlcCopyFirmwareToDataFlash(uint32_t dataFlashAddr, int32_t dfuSize);

extern int_fast8_t BlcCopyDataFlashToFirmware(uint32_t dataFlashAddr);

extern FwdBootCommand_t BlcCheckDataFlashFirmware(uint32_t dataFlashAddr, uint32_t dataFlashSize, FWDescriptor_t * pFWD);

extern FwdBootCommand_t BlcReadDescriptor(uint32_t dataFlashAddr, uint32_t dataFlashSize, FWDescriptor_t * pfwd);

#elif defined(BLC_USE_PFLASH)

extern FwdBootCommand_t BlcCheckProgramFlashFirmware(uint32_t dataFlashAddr, FWDescriptor_t * pFWD);
extern int_fast8_t BlcCopyProgramFlashToFirmware(uint32_t flashAddr);

#else
#warning "fix - blc.cfh.h"
#endif

/******************************************************************************/

extern void BlcActivateNewImage(uint32_t dflashAddr, FwdBootCommand_t  fwType);

#ifdef USE_FWD_BC_BOOT_MENU
  extern void BlcBtlMenuEnter(const char * info);
#endif

#if defined(USE_FWD_BC_MAKE_DFX_NEW)
  extern FwdBootCommand_t BlcCheckNewDfuDfxImage(uint32_t dflashAddr, FWDescriptor_t * fwd);
#endif

extern void BlcReboot(void);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // BLC_H

