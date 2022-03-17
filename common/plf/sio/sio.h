/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef SIO_H
#define SIO_H

#include "plf/plf.h"


extern bool SioTxBusy(void);

extern int_fast16_t SioGetChar(void);

extern void SifWriteCh(char ch);
extern void SioPrintLn(void);
extern void SifWrite(const char * BufferPtr);
extern void SifWriteLn(const char * BufferPtr);

extern void SioInit(void);


#endif // UART_H
