/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef BLC_DEF_H
#define BLC_DEF_H

#ifdef __cplusplus
extern "C" {
#endif

#include "hal/cfg/pflash_cfg.h"
#include "plf/plf.h"
#include "plf/crc/crc16.h"

/******************************************************************************/

typedef enum {
  FWD_LAST_APP_APPLICATION = 0xAA,
  FWD_LAST_APP_BOOTLOADER = 0xBB,
}                                 FwdLastApp_t;

#pragma pack(push, 1)
typedef struct          BootControl_tag {   
	uint8_t          			app;      // FwdLastApp_t
	uint8_t      					cmd; 			// FwdBootCommand_t
  uint32_t              adr;
	Crc16_t               crc;
  // not part of CRC protected record -->
  uint32_t              rccCsr;             // last reset reason...
}                       BootControl_t;
#pragma pack(pop)

#define BLC_BOOT_CONTROL_SIZE     (0x40)

/******************************************************************************/

#define RUN_FROM_IDE              (0xDEADBEAF)
#define RUN_FROM_PATCHED_FILE     (0xA55A8BB8)

#define BOOTCTRL_HEADER_ID	      "(c) Flow Loop FLC"

#pragma pack(push, 1)
typedef struct   {
  char              		id[40];
  uint8_t               swVerMajor;
  uint8_t               swVerMinor;
  char                  swVerType; 
  /* the following is patched by xxx_hex2bin.exe */
  bool                  wdtEnable;    /* true in release, false in debug */
  uint32_t              crcGuard;     // if == RUN_FROM_IDE, crc is not checked
  Crc16_t               crc;
}                       FWDescriptor_t;
#pragma pack(pop)


/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // BLC_DEF_H

