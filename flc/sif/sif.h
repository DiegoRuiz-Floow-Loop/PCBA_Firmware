/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef SIF_H
#define SIF_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/evos/evos.h"
#include "plf/nvm/nvm.h"
#include "plf/cli/cli.h"

 
/******************************************************************************/
  
extern void SifWrite(const char * stringPtr);  
extern void SifWriteLn(const char * stringPtr);  
extern void SifWriteCh(uint_fast8_t ch);
extern void SifTxFlush(void);  

extern bool SifTxBusy(void);

extern bool SifCharReady(void);


/******************************************************************************/

#ifdef CLI_ENABLE
extern int_fast16_t TstIOpwm(CliParam_t param1, CliParam_t param2, CliParam_t param3);
extern int_fast16_t TstIOStop(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

// calle first with true the with FASLE
 
extern void SIF_Init(bool lowLevel);

/******************************************************************************/

#ifdef __cplusplus
}
#endif
#endif
