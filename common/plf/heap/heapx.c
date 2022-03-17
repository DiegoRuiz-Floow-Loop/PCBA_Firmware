/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdlib.h>

#include "plf/plf.h"
#include "plf/trc/trc.h"
#if (PLF_OS==PLF_OS_RTOS)
#include "plf/rtos/rtos.h"
#endif
#include "plf/heap/heapx.h"
#include "plf/cfg/heapx_cfg.h"

/******************************************************************************/

#if (PLF_OS==PLF_OS_EVOS)

  static char	            * heapPtr;
  static char	            * heapMaxPtr;  // to check is the heap is to small!!!
  static char	              heap[((HEAPX_SIZE+1)>>1)<<1] 
#if defined(HEAPX_SPEC_ADDR)   
  HEAPX_SPEC_ADDR
  //__attribute__((section(".ARM.__at_0x10000000")))
#endif
  ;
#endif

void * HeapxMalloc(size_t size)
{
#if (PLF_OS==PLF_OS_EVOS)
	if(heapPtr > heap + HEAPX_SIZE) {
    return NULL;
		//for (;;) ;
	} else {
    char	* ptr= heapPtr;
    size = (((size + 3) >> 2) << 2);
    heapPtr += size;
    if (heapPtr > heapMaxPtr) {
      heapMaxPtr = heapPtr;
    }
    //CliPrintf(VT100_TextAttrBright ">> HeapxMalloc: %d, 0x%08X" VT100_TextAttrReset CLI_NL, size, ptr);
    return ptr;
  }
#elif (PLF_OS==PLF_OS_RTOS)
  return RTOS_MALLOC(size);
#else
  return malloc(size);
#endif
}

void HeapxFree(void * ptr)
{
  //CliPrintf(VT100_TextAttrBright "<< HeapxFree 0x%08X" VT100_TextAttrReset CLI_NL, ptr);
#if (PLF_OS==PLF_OS_EVOS)
#elif (PLF_OS==PLF_OS_RTOS)
  RTOS_FREE(ptr);
#else
  free(ptr);
#endif
}

void HeapxUsed(int * used, int * pct)
{
#if (PLF_OS==PLF_OS_EVOS)
  *used = (uint32_t)heapMaxPtr-(uint32_t)heap;
  *pct =(100UL * *used) / HEAPX_SIZE;
#elif (PLF_OS==PLF_OS_RTOS)
  *used = configTOTAL_HEAP_SIZE - xPortGetFreeHeapSize();
  *pct =(100UL * *used) / configTOTAL_HEAP_SIZE;
#else
  *used = 0;
  *pct =  0;
#endif
}

void HeapxClean(void)
{
#if (PLF_OS==PLF_OS_EVOS)
  //TRACE(TRC_TA_PLF, TRC_TL_4, "HeapxClean()");
	heapPtr = heap;
  heapMaxPtr = heapPtr;
#else
#if 0  
  int used = configTOTAL_HEAP_SIZE - xPortGetFreeHeapSize();
  int pct =(100UL * used) / configTOTAL_HEAP_SIZE;
  TRACE_VA(TRC_TA_PLF, TRC_TL_4, "HeapxClean(), used: %d, Pct: %d", used, pct);
#endif  
#endif  
}

void HeapxInit(void)
{
#if (PLF_OS!=PLF_OS_EVOS)
//	if (heap == NULL) {
//		#if (PLF_OS==PLF_OS_RTOS)
//			heap = RTOS_MEM_ALLOCATE(((HEAPX_SIZE+1)>>1)<<1);
//		#elif (PLF_OS==PLF_OS_WINDOWS)
//			heap = malloc(((HEAPX_SIZE+1)>>1)<<1);
//		#else
//			#error "FIX"
//		#endif
//		if (heap == NULL)
//			for (;;) ;
//  }
#endif  
  HeapxClean();
}


/******************************************************************************/

