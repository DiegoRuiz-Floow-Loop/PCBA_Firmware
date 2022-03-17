/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef HEAPX_H
#define HEAPX_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"

/******************************************************************************/

extern void HeapxUsed(int * used, int * pct);

// Call this to free all json objects...
extern void HeapxClean(void);

extern void * HeapxMalloc(size_t size);
extern void HeapxFree(void * ptr);

extern void HeapxInit(void);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // HEAPX_H

