/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef JSONX_H
#define JSONX_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"
#include "plf/heap/heapx.h"

#include "jansson.h"

/******************************************************************************/

//extern volatile uint32_t mySeq;


extern void JsonxInit(void);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // JSONX_H

