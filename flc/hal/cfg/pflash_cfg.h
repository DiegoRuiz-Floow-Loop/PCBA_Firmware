/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PFLASH_CFG_H
#define PFLASH_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************/

/* define if dflash supports writing any buffer length */
#define PFLASH_WRITE_BUFFER

/****************************************************************************/

#define _FLASH_BASE                 (0x08000000)
#if defined(EVK)
#define _FLASH_SIZE                 (0x00040000)
#else
// Changed from 0x00080000 to 0x00040000 because bootloader pflash driver cant 
// successfully write to bank2 (program flash memory map addr. 256-512 KB).
#define _FLASH_SIZE                 (0x00040000)
#endif
#define _FLASH_END                  (_FLASH_BASE + _FLASH_SIZE)

// BOOTLOADER  
#define _BOOTLOADER_SIZE            (0x4000)

// APPLICATION  
#define APP_PROGRAM_FLASH_START     (_FLASH_BASE + _BOOTLOADER_SIZE)		 
#define APP_PROGRAM_FLASH_SIZE      (_FLASH_SIZE - _BOOTLOADER_SIZE)
#define APP_PROGRAM_FLASH_END       (APP_PROGRAM_FLASH_START + APP_PROGRAM_FLASH_SIZE - 1)

// NVM PFLASH
#define HAL_PFLASH_NVM_SECTOR_SIZE  (0x800) // 2K
#define _NVM_PFLASH_SECTOR_SIZE	    (0x0)   // not used
#define _NVM_PFLASH_SIZE            (0*_NVM_PFLASH_SECTOR_SIZE)
//#define _NVM_PFLASH_START		        (_FLASH_END-_NVM_PFLASH_SIZE)	

// Firmware Descriptor
#define BLC_FW_DESCIPTOR_SIZE       (0x40)
#define BLC_FW_DESCIPTOR_ADDR       (APP_PROGRAM_FLASH_START + APP_PROGRAM_FLASH_SIZE - _NVM_PFLASH_SIZE - BLC_FW_DESCIPTOR_SIZE) 

/******************************************************************************/


#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* PFLASH_CFG_H */
