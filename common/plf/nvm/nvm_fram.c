/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stddef.h>

#include "hal/fram/fram.h"

#include "plf/vtim/vtim.h"
#include "plf/nvm/nvm.h"

/****************************************************************************/

#ifdef NVM_ENABLE

//BootCount_t           bootCount;

volatile NvmState_t nvmState = NVM_BOOTING;
NvmErrorStatus_t nvmErrorStatus = NE_OK;

// In development it was observed that the FRAM driver in some special cases
// could return HAL error. To circumvent this problem polling the FRAM 
// write/read functions would result in correct NVM start up.
// A retry timeout is implemented to ensure the read/write functions doesn't
// end in an endless loop.
#define RETRY_TIMEOUT_MSEC  (100)

/****************************************************************************/

void NvmWrite(
  uint_fast16_t         nvmAddr,
  void const            * pData,
  uint_fast16_t         size)
{
  if (nvmState == NVM_BOOTING) NvmInit();

  VTim_t timer;
  VTimSetMsec(&timer, RETRY_TIMEOUT_MSEC);
  do {
    nvmErrorStatus = HalFramWriteBlock(nvmAddr, (uint8_t *)pData, size);
  } while ((nvmErrorStatus == NE_HAL_DRIVER_ERROR) && !VTimIsExpired(&timer));  
}


/****************************************************************************/

void NvmRead(
  uint_fast16_t         nvmAddr,
  void                  * pData,
  uint_fast16_t         size)
{
  if (nvmState == NVM_BOOTING) NvmInit();

  VTim_t timer;
  VTimSetMsec(&timer, RETRY_TIMEOUT_MSEC);
  do {
    nvmErrorStatus = HalFramReadBlock(nvmAddr, pData, size);
  } while ((nvmErrorStatus == NE_HAL_DRIVER_ERROR) && !VTimIsExpired(&timer));   
}


/****************************************************************************/

static const MarkStr_t            beginMarkValue = "1234567";
static const MarkStr_t            endMarkValue =   "ABCDEFG";

void NvmInit(void)
{
  if (nvmState == NVM_BOOTING) {
    nvmState = NVM_BOOTING2;

    uint64_t bootCount;

    MarkStr_t             beginMark, endMark;
    bool								ok;
    uint32_t nvmSize = sizeof(NvmStruct_t);
   
    if (nvmSize > HAL_FRAM_SIZE) {
      for (;;) ;
    }

    NvmRead(offsetof(NvmStruct_t, beginMark), (uint8_t *)beginMark, sizeof(MarkStr_t));
    NvmRead(offsetof(NvmStruct_t, endMark), (uint8_t *)endMark, sizeof(MarkStr_t));

    ok  = (strcmp(beginMark, beginMarkValue) == 0) && (strcmp(endMark, endMarkValue) == 0);
    //If the begin mark or endmark is invalid there is configuration available
    //or the nvm layout has been changed
    if (!ok) {
      NvmWrite(offsetof(NvmStruct_t, beginMark), (uint8_t *)beginMarkValue, sizeof(MarkStr_t));
      NvmWrite(offsetof(NvmStruct_t, endMark), (uint8_t *)endMarkValue, sizeof(MarkStr_t));
      bootCount = 1;		
      nvmState = NVM_INITIALIZED;
    } else {
      NvmRead(offsetof(NvmStruct_t, bootCount), (uint8_t *)&bootCount, sizeof(bootCount));
      bootCount++;		
      nvmState = NVM_OK;
    }
    NvmWrite(offsetof(NvmStruct_t, bootCount), (uint8_t *)&bootCount, sizeof(bootCount));
  }

} 



#endif

/****************************************************************************/

