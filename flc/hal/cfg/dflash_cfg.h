/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef DFLASH_CFG_H
#define DFLASH_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************************************************************/

#include "stm32l4xx_hal.h"
#include <plf/plf.h>

extern SPI_HandleTypeDef hspi1;

#define SPI_DFLASH      hspi1
 
/******************************************************************************/

#if defined(EVK) 
#define DFLASH_SIZE_MBIT		DFLASH_32MBIT
#else
#define DFLASH_SIZE_MBIT		DFLASH_128MBIT  // adesto,  AT25SF128
//#define DFLASH_SIZE_MBIT		DFLASH_512MBIT  // Micron,  MT25QL512AB
#endif

#include "hal/dflash/dflash_def.h"

/******************************************************************************/
  
/* PARTITIONING */  

// EVK - DFU 0.5 MB, CMD files + ELOG 0.5 MB, DLOG  3.0 MB
// APP - DFU 0.5 MB, CMD files + ELOG 0.5 MB, DLOG 15.0 MB

// DFU data
#define DFLASH_DFU_SIZE             (512 * 1024)	// 512 KB  
#define DFLASH_DFU_START            (offsetof(DFlashMap_t, dfu))

// Firmware command file
#define DFLASH_CMDFILE_SIZE         (128 * 1024)  // 128 KB
#define CMDFILE_ENTRY_SIZE          (16 * 1024) 	//  16 KB
#define CMDFILE_ENTRIES             (DFLASH_CMDFILE_SIZE / CMDFILE_ENTRY_SIZE)
#define DFLASH_CMDFILE_START        (offsetof(DFlashMap_t, cmdfile))

// Event Log - common
#define DFLASH_ELOG_SIZE            (384 * 1024)  // 384 KB
#define ELOG_ENTRY_SIZE             (128)        	// 128 B
#define ELOG_ENTRIES                (DFLASH_ELOG_SIZE / ELOG_ENTRY_SIZE)  
#define DFLASH_ELOG_START           (offsetof(DFlashMap_t, elog))

// Data Log 
#if defined(EVK)
#define DFLASH_DLOG_SIZE            (3 * 1024 * 1024)  	//   3 MB
#else
#define DFLASH_DLOG_SIZE            (15 * 1024 * 1024) 	//  15 MB
#endif
#define DLOG_ENTRY_SIZE             (256)     					// 256 B
#define DLOG_ENTRIES                (DFLASH_DLOG_SIZE / DLOG_ENTRY_SIZE)
#define DFLASH_DLOG_START           (offsetof(DFlashMap_t, dlog))

#pragma pack(push, 1)  
typedef struct {
  uint8_t               dfu[DFLASH_DFU_SIZE];
  uint8_t               cmdfile[DFLASH_CMDFILE_SIZE];					  
  uint8_t               elog[DFLASH_ELOG_SIZE];  
  uint8_t               dlog[DFLASH_DLOG_SIZE];
}                       DFlashMap_t;
#pragma pack(pop)

#if defined(EVK)
static_assert(sizeof(DFlashMap_t) == (4 * 1024 * 1024), "Dflash map size is not 4MB!");
#else
static_assert(sizeof(DFlashMap_t) == (16 * 1024 * 1024), "Dflash map size is not 16MB!");
#endif

/******************************************************************************/


#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* DFLASH_CFG_H */
