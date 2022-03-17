/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef rle_cnv_H
#define rle_cnv_H

#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>
#include <stdint.h>
  
extern size_t Decompress(
  uint8_t               * input, 
  uint8_t               * output, 
  size_t                inputSize,
  size_t                maxOutputSize) ;
  
extern size_t Compress(
  uint8_t               * input,
  uint8_t               * output,
  size_t                inputSize);

//#define DO_TEST

extern size_t RleCode(
  char        * dstFileName,
  uint8_t     * imageData,
  size_t      size);


#ifdef __cplusplus
}
#endif


#endif // rle_cnv_H

