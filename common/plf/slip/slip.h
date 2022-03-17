/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef SLIP_H
#define SLIP_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdint.h>
#include <stdbool.h>

/******************************************************************************/
#define SLIP_END        0xC0    // END == End of messages
#define SLIP_ESC        0xDB    // ESC == Start of spec. char

#define SLIP_ESC_END    0xDC    // ESC+ESC_END == END
#define SLIP_ESC_ESC    0xDD    // ESC+ESC_ESC == ESC

// First Raw Data Buffer To Send Function
typedef void (*SlipSendFunc_t)(uint8_t c);

extern void Slip_RawToSlipSendFirst(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc); 

// Last Raw Data Buffer To Send Function
extern void Slip_RawToSlipSendLast(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc); 
	
extern void Slip_RawToSlipSend(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc);

/******************************************************************************/

extern bool Slip_SlipCharToRawChars(
  uint8_t ch,
  uint8_t * raw,
	uint16_t * rawLength,
  bool * slipNormal, 
  const uint16_t msgSizeMax);
  
/******************************************************************************/
  
  
#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* SLIP_H */

