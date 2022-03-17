/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef CRC16_H
#define CRC16_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf\plf.h"

/*********************************************************** Global defines */

#define CRC16_INITIAL_SEED (0xFFFFu)

/********************************************************** Export typedefs */

typedef unsigned short Crc16_t;

/********************************************* Global function declarations */

#if defined(BOOTLOADER)
/* small+slow, Using calculation */
extern Crc16_t CalcCrc16_algo(uint32_t length, const uint8_t * address, Crc16_t seed);
#define CalcCrc16(length, address, seed)  CalcCrc16_algo(length, (const uint8_t *)address, seed)
#define CALC_CRC16(length, address, seed) CalcCrc16_algo(length, (const uint8_t *)address, seed)
#else
/* Fast, Using Table */
extern Crc16_t CalcCrc16_table(uint32_t length, const uint8_t * address, Crc16_t seed);
#define CalcCrc16(length, address, seed)  CalcCrc16_table(length, (const uint8_t *)address, seed)
#define CALC_CRC16(length, address, seed) CalcCrc16_table(length, (const uint8_t *)address, seed)
#endif


/*lint --flb */     /* end of treat file as library header */
#ifdef __cplusplus
}
#endif

#endif /*crc*/


