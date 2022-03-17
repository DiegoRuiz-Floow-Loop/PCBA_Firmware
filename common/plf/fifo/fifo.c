/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/fifo/fifo.h"


/*******************************************************************************
 *    Defines
 ******************************************************************************/
#define HAL_START_ATOMIC() {}
#define HAL_END_ATOMIC() {}

/* NOTE it is essensial that the in/out pointer never contains a value bigger than len */
#define FIFO_INCR(pFifo, inout) if ((pFifo)->inout == (pFifo)->len) (pFifo)->inout=0; else (pFifo)->inout++;

/* generic fifo type */
typedef FifoT(0) Fifo_t;

/*******************************************************************************/


bool FifoPut(void* p, uint8_t ch)
{
  Fifo_t* pFifo=(Fifo_t*)p;
  if (FifoIsFull(pFifo)) {
    return false;
  } else {
    pFifo->elem[pFifo->in] = ch;
    FIFO_INCR(pFifo, in);
    return true;
  }
}

bool FifoGet(void* p, uint8_t * ch)
{
  Fifo_t* pFifo=(Fifo_t*)p;
  if (FifoIsEmpty(pFifo)) {
    return false;
  } else {
    *ch = pFifo->elem[pFifo->out];
    FIFO_INCR(pFifo, out);
    return true;
  }
}

bool FifoPeek(void* p, uint8_t * ch)
{
  Fifo_t* pFifo=(Fifo_t*)p;

  if (FifoIsEmpty(pFifo)) {
    return false;
  } else {
    *ch = pFifo->elem[pFifo->out];
    return true;
  }
}

uint16_t FifoCount(void* p)
{
  Fifo_t* pFifo=(Fifo_t*)p;
  int16_t cnt;
  int16_t o;
  int16_t i;

  HAL_START_ATOMIC();
  o = pFifo->out;
  i = pFifo->in;
  HAL_END_ATOMIC();

  if ( o <= i)
    cnt = i - o;
  else
    cnt = pFifo->len + 1 - o + i;
  return cnt;
}

bool FifoIsFull(void* p)
{
  Fifo_t* pFifo=(Fifo_t*)p;
  uint16_t i, o;
  HAL_START_ATOMIC();
  i = pFifo->in;
  o = pFifo->out;
  HAL_END_ATOMIC();
  if (i == pFifo->len) {
    return o==0;
  } else {
    return i+1 == o;
  }
}

bool FifoIsEmpty(void* p)
{
  Fifo_t* pFifo=(Fifo_t*)p;
  uint16_t i, o;
  HAL_START_ATOMIC();
  i = pFifo->in;
  o = pFifo->out;
  HAL_END_ATOMIC();
  return i==o;
}
