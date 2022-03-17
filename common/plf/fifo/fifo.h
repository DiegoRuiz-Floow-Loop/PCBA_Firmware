/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef FIFO_H
#define FIFO_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"


/******************************************************************************/

/**
 * Type definition for a FIFO.
 *
 * @param SIZE Number of bytes to allocate for the FIFO.
 */
#define FifoT(SIZE) struct {\
  volatile uint16_t     in; /* in pointer - first vacant element */ \
  volatile uint16_t     out; /* out pointer - first valid element */ \
  uint16_t              len; /* length of fifo (one less than allocated) must be == SIZE */ \
  uint8_t               elem[SIZE+1]; /* the elements. NOTE one extra */ \
}

/**
 * Get the total size (number of elements) of the FIFO.
 *
 * @param pFifo Pointer to FIFO instance.
 * @returns Number of bytes allocated for the FIFO.
 */
#define FIFO_SIZE(pFifo) ((pFifo)->len)

/**
 * Initialize the fifo.
 * @param pFifo Pointer to the FIFO instance.
 * @param size Should be the same as given to FIFO_T.
 */
#define FIFO_INIT(pFifo, size) ((pFifo)->in = (pFifo)->out = 0, (pFifo)->len=size)

/**
 * Get the condition if the FIFO is empty.
 *
 * @param p Pointer to FIFO instance.
 * @returns true if the FIFO has 0 elements, otherwise false.
 */
extern bool FifoIsEmpty(void* p);

/**
 * Get the condition if the FIFO is full.
 *
 * @param p Pointer to FIFO instance.
 * @returns true: empty
 */
extern bool FifoIsFull(void* p);

/**
 * Get the number of elements in the fifo.
 *
 * @param p Pointer to FIFO instance.
 * @returns true: full
 */
extern uint16_t  FifoCount(void* p);

/**
 * Put one byte in the fifo.
 *
 * If fifo is full return -1 else put ch in fifo and return 0.
 *
 * @param p Pointer to FIFO instance.
 * @param ch The byte to insert into the FIFO.
 * @returns true: success
 */
extern bool FifoPut(void* p, uint8_t ch);

/**
 * Get a character from fifo.
 *
 * If fifo is empty return -1 else get and return first element from fifo.
 *
 * @param p Pointer to FIFO instance.
 * @returns true: success
 */
extern bool FifoGet(void* p, uint8_t * ch);

/**
 * Peek a character from fifo.
 *
 * If fifo is empty return -1 else get and return first element from fifo,
 * but compared to FifoCharGet the character will remain in the fifo.
 *
 * @param p Pointer to FIFO instance.
 * @returns true: success
 */
extern bool FifoPeek(void* p, uint8_t * ch);



#ifdef __cplusplus
}
#endif

#endif
