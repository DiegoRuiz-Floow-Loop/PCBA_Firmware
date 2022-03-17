/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/
#include <plf/evos/evos.h>

#include <hal/dflash/dflash.h>

#include "blc.h"

#include "blc_crc.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define CRC_BUFFER_SIZE     512u
#define BLC_FW_SIZE_WO_DESC ((uint32_t)(APP_PROGRAM_FLASH_SIZE - BLC_FW_DESCIPTOR_SIZE))

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/
 
static uint8_t  crcBuffer[CRC_BUFFER_SIZE];
static uint32_t crcBytesRead;
static uint16_t crcDFlash;
static bool     fwCrcCheckDone;
static crcCallback_t callback;
static EvosEventHandle_t crcTask = EVOS_UNINITIALIZED_HANDLE;
static bool isRunning = false;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static void FWCrcCheck(EvosEventParam_t event);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void BlcCrcValidate(const crcCallback_t cb)
{
  if(isRunning) {
    return;
  }
  
  if (crcTask == EVOS_UNINITIALIZED_HANDLE) {
    crcTask = EvosEventRegister(FWCrcCheck, "CRC");
  }
  
  isRunning = true;
  EvosEventSetDelta(crcTask, 0, 0);
  callback = cb;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/


static void FWCrcCheck(const EvosEventParam_t event)
{
  static FWDescriptor_t desc;
	
	if(crcBytesRead == 0) {
	  EvosEventClear(crcTask);		
	  crcDFlash = CRC16_INITIAL_SEED;	
	}
  
  const uint32_t bytesToRead = BLC_FW_SIZE_WO_DESC - crcBytesRead >= CRC_BUFFER_SIZE ? CRC_BUFFER_SIZE : BLC_FW_SIZE_WO_DESC - crcBytesRead;
  
  HalDFlashRead(DFLASH_DFU_START + crcBytesRead, crcBuffer, bytesToRead);
  crcDFlash = CalcCrc16(bytesToRead, crcBuffer, crcDFlash);						
  crcBytesRead += bytesToRead;
  
	fwCrcCheckDone = crcBytesRead == BLC_FW_SIZE_WO_DESC;

	if(!fwCrcCheckDone) {
    EvosEventSetDelta(crcTask, 0, 0);
		return;
	}
	
  HalDFlashRead(APP_PROGRAM_FLASH_SIZE - BLC_FW_DESCIPTOR_SIZE, (uint8_t*)&desc, sizeof(FWDescriptor_t));
	
  crcBytesRead = 0;
  
  isRunning = false;
  if(callback != NULL) {
    callback(crcDFlash == desc.crc);
  }
}

/******************************************************************************/
