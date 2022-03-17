/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HAL_PFLASH_H
#define HAL_PFLASH_H

#ifdef __cplusplus
extern "C" {
#endif

#include "hal/cfg/pflash_cfg.h"
#include "plf/plf.h"

/****************************************************************************/

extern uint8_t pflashStatus;

extern bool HalPFlashEraseApplicatioFirmware(void);

extern bool HalPFlashEraseAddrRange(
  uint32_t    address,
  uint32_t    endAddress);

extern bool HalPFlashRead(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len);
    
#ifndef PFLASH_WRITE_BUFFER
  extern bool HalPFlashWrite_64b(uint32_t flashAddr,  uint64_t data);           
#endif

#ifdef PFLASH_WRITE_BUFFER
  extern bool HalPFlashWrite(uint32_t flashAddr,  uint32_t ramAddr, uint32_t len);     /* multiplum of 4 byte used (4, 8, 16, ... */
#endif

/****************************************************************************/

extern void HalPFlashInit(void);

/****************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // HAL_PFLASH_H

