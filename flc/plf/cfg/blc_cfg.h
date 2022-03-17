/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_BLC_CFG_H
#define PLF_BLC_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "hal/cfg/pflash_cfg.h"

#include "plf/blc/blc_def.h"

/*******************************************************************************/

#define BLC_USE_DFLASH
//#define BLC_USE_PFLASH

#define XFLASH_BUFFER  (1024)

/*******************************************************************************/

/* Configure Bootloader support */
#define SUPPORT_DFU_VIA_DFLASH
 

/* Configure Bootloader Commands */
//#define USE_FWD_BC_MAKE_DFX_NEW
//#define USE_FWD_BC_BOOT_MENU
//#define USE_FWD_BC_MAKE_DFU_GOLD
//#define USE_FWD_BC_SAVE_GOLDEN_IMAGE

typedef enum {
	FWD_BC_UNKNOWN = 0xF0,
	FWD_BC_NONE,  
	FWD_BC_REBOOT,
	FWD_BC_BOOT_TO_APPL,
	FWD_BC_MAKE_DFU_NEW,      //  Update from DFU Image
#ifdef USE_FWD_BC_MAKE_DFX_NEW  
  FWD_BC_MAKE_DFX_NEW,      //  Update from RLE DFU Image == DFX file
#endif
  //FWD_BC_MAKE_DFU_GOLD,     //  Update from GOLDEN Image
  //FWD_BC_SAVE_GOLDEN_IMAGE,	
  
#ifdef USE_FWD_BC_BOOT_MENU  
	FWD_BC_BOOT_MENU,
#endif  

  FWD_BC_Last
} 											FwdBootCommand_t;

/*******************************************************************************/

// RAM settings

#define IRAM1_BASE                (0x20000000)
#define IRAM2_BASE                (0x10000000)

#if defined(EVK)

#define IRAM1_SIZE                (0x10000)
#define BLC_BOOT_CONTROL_ADDR     (IRAM1_BASE + IRAM1_SIZE - BLC_BOOT_CONTROL_SIZE)

#else

// The bootloader control structure is placed in IRAM2 instead of IRAM1
#define IRAM1_SIZE                (0x18000)
#define IRAM2_SIZE                (0x08000)
#define BLC_BOOT_CONTROL_ADDR     (IRAM2_BASE + IRAM2_SIZE - BLC_BOOT_CONTROL_SIZE)

#endif

/*******************************************************************************/

#if defined(EVK) && (BLC_FW_DESCIPTOR_ADDR != 0x0803FFC0)
	#error "check FWDescriptor address"
#elif !defined(EVK) && (BLC_FW_DESCIPTOR_ADDR != 0x0803FFC0)
  #error "check FWDescriptor address"
#endif

#if defined(_MAKE_BLC_CODE)
#if !defined(BOOTLOADER)

#if defined(EVK)
const FWDescriptor_t fWDescriptor  __attribute__((section(".ARM.__at_0x0803FFC0"))) = 
#else
// Changed from 0x0807FFC0 to 0x0803FFC0 because bootloader pflash driver cant 
// successfully write to bank2.
const FWDescriptor_t fWDescriptor  __attribute__((section(".ARM.__at_0x0803FFC0"))) = 
#endif
{
  .id         = BOOTCTRL_HEADER_ID,
  .swVerMajor = PLF_VERSION_MAJOR,
  .swVerMinor = PLF_VERSION_MINOR,
  .swVerType  = 'D',
  /* the following fields are patched by hex2bin.exe */
#ifdef HAL_WDT_ENABLE
	.wdtEnable  = true,   	/* WDT started by bootloader */  
#else
	.wdtEnable  = false,   	/* WDT not started by bootloader */
#endif
  .crcGuard   = RUN_FROM_IDE,  
  .crc        = 0,
  //.extCrc     = 0,
  /* the following is not part of the CRC check */
};  

#endif
#endif

/*******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* PLF_BLC_CFG_H */
