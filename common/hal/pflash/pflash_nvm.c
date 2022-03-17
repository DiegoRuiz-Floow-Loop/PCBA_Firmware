/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/hal.h"
#include "hal/pflash/pflash_nvm.h"
#include "hal/pflash/pflash.h"
//#include "plf/blc/blc.h"

#include "plf/nvm/nvm.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

#if (NVM_ACCESS==NVM_ACCESS_ID)

/******************************************************************************/


// sector flags
#define SECTOR_EMPTY_FLAG         (0xFFFFFFFFFFFFFFFFLLU)
#define SECTOR_INITIALIZING_FLAG  (0xFFFFFFFFFFFFAAAALLU)
#define SECTOR_VALID_FLAG         (0xFFFFFFFFAAAAAAAALLU)
#define SECTOR_INVALID_FLAG       (0xFFFFAAAAAAAAAAAALLU)

// max number of variables
#define HAL_FLASH_NVM_MAX_VARIABLE       ((int)NVM_ID_Last)



/******************************************************************************/

//#if (PLF_OS==PLF_OS_WINDOWS) // TEST
#pragma pack(push, 1)
//#endif

typedef uint64_t        SectorFlag_t;

// defines a variable record
// members must be byte aligned
typedef struct VariableRecord_tag {
  uint8_t               flag;                 // flags indicate variable status
  uint8_t               no;                   // unique variable id
  uint8_t               data[HAL_FLASH_NVM_MAX_VARIABLE_SIZE]; // variable data
  uint8_t               checksum;            // 2's complement checksum of id and data
}                       VariableRecord_t;

#define NVM_REC_NUMBERS ((_NVM_PFLASH_SECTOR_SIZE-sizeof(SectorFlag_t))/HAL_FLASH_NVM_RECORD_SIZE)

// defines a sector (its address)
typedef struct {
  SectorFlag_t          flag;
  VariableRecord_t      rec[NVM_REC_NUMBERS];  
}                       Sector_t;

//#if (PLF_OS==PLF_OS_WINDOWS) // TEST
#pragma pack(pop)
//#endif


/*************************************************************************/

#define HAL_PFLASH_NVM_SECTOR_1_ADDR      (_NVM_PFLASH_START)
#define HAL_PFLASH_NVM_SECTOR_2_ADDR      (_NVM_PFLASH_START+_NVM_PFLASH_SECTOR_SIZE)


#if (PLF_OS==PLF_OS_WINDOWS) // TEST
  static Sector_t * pSector1 = ((Sector_t *)(((uint32_t)&flash)+0));
  static Sector_t * pSector2 = ((Sector_t *)(((uint32_t)&flash)+0+HAL_PFLASH_NVM_SECTOR_SIZE));
#else
  static Sector_t * pSector1 = ((Sector_t *)(HAL_PFLASH_NVM_SECTOR_1_ADDR));
  static Sector_t * pSector2 = ((Sector_t *)(HAL_PFLASH_NVM_SECTOR_2_ADDR));
#endif


// the variable reference number lookup table
static int16_t          lookupTable[HAL_FLASH_NVM_MAX_VARIABLE];

// the next free record in the valid sector
static int16_t          nextFreeRecord;  // -1, 0 .. NVM_REC_NUMBERS

// pointer to valid sector
static Sector_t         * activeSector;

/*************************************************************************/
static bool_t NVOL_EraseSector(
  Sector_t * sector )
{
  HalPFlashErase((uint32_t)sector, (uint32_t)sector+_NVM_PFLASH_SECTOR_SIZE-1);
  return TRUE;
}

/**************************************************************************/
static bool_t NVOL_SetSectorFlags(
  Sector_t    * sector,                                   // pointer to sector
  uint64_t    flags                                      // new flags to write
  )
{
#ifdef PFLASH_WRITE_BUFFER  
  HalPFlashWrite((uint32_t)&sector->flag, (uint32_t)&flags, sizeof(flags));
#else
  HalPFlashWrite_64b((uint32_t)&sector->flag, flags);
#endif  
  return TRUE;
}

/**************************************************************************/
static int_fast16_t NVOL_GetNextFreeRecord(
  Sector_t * sector)
{
  int_fast16_t next;
  
  for (next = 0; next < (int_fast16_t)NVM_REC_NUMBERS; next++) {
    if (sector->rec[next].flag == 0xFF) 
      return next;
  }
  return -1;
}

/**************************************************************************/

static bool_t NVOL_SetVariableRecord (
  VariableRecord_t  * pVarRec,    // variable record to store
  Sector_t          * sector,     // sector to write to
  uint_fast16_t     record        // record number
)
{
#ifdef PFLASH_WRITE_BUFFER  
  HalPFlashWrite((uint32_t)&sector->rec[record], (uint32_t)pVarRec, sizeof(VariableRecord_t));
#else
  uint32_t d = (uint32_t)&sector->rec[record];
  uint64_t * v = (uint64_t *)pVarRec;
  HalPFlashWrite_64b(d, *v++);

  HalPFlashWrite_64b(d+8, *v);
#endif  
  return TRUE;
}

/**************************************************************************/
static bool_t NVOL_IsVariableRecordValid(
  VariableRecord_t * pVarRec)
{
  uint8_t checksum;
  uint_fast16_t by;

  // check flags
  if (pVarRec->flag != 0xAA)
    return FALSE;

  // check checksum
  checksum = pVarRec->no & 0xFF;
  //checksum += (pVarRec->id >> 8) & 0xFF;
  for (by = 0; by < HAL_FLASH_NVM_MAX_VARIABLE_SIZE; by++){
    checksum += pVarRec->data[by];
  }
  checksum = 0x100 - checksum;
  if (pVarRec->checksum != checksum) 
    return FALSE;

  return TRUE;
}

/**************************************************************************/
static void NVOL_ConstructLookupTable(void)
{
  int_fast16_t i;

  for (i=0; i<HAL_FLASH_NVM_MAX_VARIABLE; i++) {
    lookupTable[i] = -1;
  }  
  
  for (i=0; i < (int_fast16_t)NVM_REC_NUMBERS; i++) {
    VariableRecord_t * pRec = &activeSector->rec[i];
    // if variable record is valid then add to lookup table
    if (NVOL_IsVariableRecordValid(pRec)) {
      //lookupTable[pRec->no] = (uint16_t)((uint32_t)pRec - (uint32_t)mValidSector);
      lookupTable[pRec->no] = i;
    }
  }
}

/**************************************************************************/
static bool_t NVOL_SwapSectors(
  Sector_t * srcSector,                               // pointer to source sector
  Sector_t * dstSector                               // pointer to destination sector
)
{
  uint_fast16_t var;
  uint_fast16_t dstRec;

  // make sure destination sector is erased
  if (dstSector->flag != SECTOR_EMPTY_FLAG) {
    // erase it
    if (!NVOL_EraseSector(dstSector)) 
      return FALSE;
  }

  // mark destination sector as being initialized
  if (!NVOL_SetSectorFlags(dstSector, SECTOR_INITIALIZING_FLAG))
    return FALSE;
  dstRec = 0;

  // copy variables to destination sector
  for (var = 0; var < HAL_FLASH_NVM_MAX_VARIABLE; var++) {
    if (lookupTable[var] >= 0) {
      if (!NVOL_SetVariableRecord(&srcSector->rec[lookupTable[var]], dstSector, dstRec)) 
        return FALSE;
    }
    dstRec++;
  }

  // mark source sector as being invalid
  if (!NVOL_SetSectorFlags(srcSector, SECTOR_INVALID_FLAG))
    return FALSE;
  // mark destination sector as being valid
  if (!NVOL_SetSectorFlags(dstSector, SECTOR_VALID_FLAG))
    return FALSE;
  // erase source sector

  // now using destination sector
  activeSector = dstSector;
  
  // get next free location in destination sector
  nextFreeRecord = NVOL_GetNextFreeRecord(dstSector);
  
  // regenerate lookup table
  NVOL_ConstructLookupTable();

  return TRUE;
}

/**************************************************************************/
static Sector_t * NVOL_InitSectors(void)
{
  // if sector 1 has invalid flags then erase it
  if ((pSector1->flag != SECTOR_EMPTY_FLAG)        &&
      (pSector1->flag != SECTOR_INITIALIZING_FLAG) &&
      (pSector1->flag != SECTOR_VALID_FLAG)        &&
      (pSector1->flag != SECTOR_INVALID_FLAG))
  {
    NVOL_EraseSector(pSector1);
  }

  // if sector 2 has invalid flags then erase it
  if ((pSector2->flag != SECTOR_EMPTY_FLAG)        &&
      (pSector2->flag != SECTOR_INITIALIZING_FLAG) &&
      (pSector2->flag != SECTOR_VALID_FLAG)        &&
      (pSector2->flag != SECTOR_INVALID_FLAG))
  {
    NVOL_EraseSector(pSector2);
  }

  // what happens next depends on status of both sectors
  switch (pSector1->flag) {
    case SECTOR_EMPTY_FLAG:
      switch (pSector2->flag) {
        // sector 1 empty, sector 2 empty
        case SECTOR_EMPTY_FLAG:
          // use sector 1
          if (!NVOL_SetSectorFlags(pSector1, SECTOR_VALID_FLAG))
            return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 empty, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG:
          // use sector 2
          if (!NVOL_SetSectorFlags(pSector2, SECTOR_VALID_FLAG))
            return 0;
          nextFreeRecord = 0;
          return pSector2;
        // sector 1 empty, sector 2 valid
        case SECTOR_VALID_FLAG:
          nextFreeRecord = NVOL_GetNextFreeRecord(pSector2);
          // sector 2 is already active
          return pSector2;
        // sector 1 empty, sector 2 invalid
        case SECTOR_INVALID_FLAG:
          // swap sectors 2 -> 1
          if (!NVOL_SwapSectors(pSector2, pSector1))
            return 0;
          nextFreeRecord = NVOL_GetNextFreeRecord(pSector1);
          // use sector 1
          return pSector1;
      }
    break;

    case SECTOR_INITIALIZING_FLAG:
      switch (pSector2->flag) {
        // sector 1 initializing, sector 2 empty
        case SECTOR_EMPTY_FLAG:
          // use sector 1
          if (!NVOL_SetSectorFlags(pSector1, SECTOR_VALID_FLAG)) return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 initializing, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG:
          // erase sector 2
          if (!NVOL_EraseSector(pSector2)) return 0;
          // use sector 1
          if (!NVOL_SetSectorFlags(pSector1, SECTOR_VALID_FLAG)) return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 initializing, sector 2 valid
        case SECTOR_VALID_FLAG:
          // erase sector 1
          if (!NVOL_EraseSector(pSector1)) return 0;
            // swap sectors 2 -> 1
          if (!NVOL_SwapSectors(pSector2, pSector1)) return 0;
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector1);
          // use sector 1
          return pSector1;
        // sector 1 initializing, sector 2 invalid
        case SECTOR_INVALID_FLAG:
          // erase sector 2
          if (!NVOL_EraseSector(pSector2)) return 0;
          // use sector 1
          if (!NVOL_SetSectorFlags(pSector1, SECTOR_VALID_FLAG)) return 0;
          nextFreeRecord = 0;
          return pSector1;
      }
      break;

    case SECTOR_VALID_FLAG:
      switch (pSector2->flag) {
        // sector 1 valid, sector 2 empty
        case SECTOR_EMPTY_FLAG:
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector1);
          // sector 1 is active
          return pSector1;
        // sector 1 valid, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG:
          // erase sector 2
          if (!NVOL_EraseSector(pSector2)) return 0;
            // swap sectors 1 -> 2
          if (!NVOL_SwapSectors(pSector1, pSector2)) return 0;
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector2);
          // use sector 2
          return pSector2;
        // sector 1 valid, sector 2 valid
        case SECTOR_VALID_FLAG:
          // erase sector 2 and use sector 1
          if (!NVOL_EraseSector(pSector2)) return 0;
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector1);
          return pSector1;
        // sector 1 valid, sector 2 invalid
        case SECTOR_INVALID_FLAG:
          // erase sector 2 and use sector 1
          if (!NVOL_EraseSector(pSector2)) return 0;
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector1);
          return pSector1;
      }
      break;

    case SECTOR_INVALID_FLAG:
      switch (pSector2->flag) {
        // sector 1 invalid, sector 2 empty
        case SECTOR_EMPTY_FLAG:
          // swap sectors 1 -> 2
          if (!NVOL_SwapSectors(pSector1, pSector2)) return 0;
          // use sector 2
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector2);
          return pSector2;
        // sector 1 invalid, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG:
          // erase sector 1
          if (!NVOL_EraseSector(pSector1)) return 0;
          // use sector 2
          if (!NVOL_SetSectorFlags(pSector2, SECTOR_VALID_FLAG)) return 0;
          nextFreeRecord = 0;
          return pSector2;
        // sector 1 invalid, sector 2 valid
        case SECTOR_VALID_FLAG:
          // erase sector 1
          if (!NVOL_EraseSector(pSector1)) return 0;
            nextFreeRecord = NVOL_GetNextFreeRecord(pSector2);
          // use sector 2
          return pSector2;
        // sector 1 invalid, sector 2 invalid
        case SECTOR_INVALID_FLAG:
          // both sectors invalid so erase both and use sector 1
          if (!NVOL_EraseSector(pSector1)) return 0;
          if (!NVOL_EraseSector(pSector2)) return 0;
          if (!NVOL_SetSectorFlags(pSector1, SECTOR_VALID_FLAG)) return 0;
          nextFreeRecord = 0;
          return pSector1;
      }
      break;
  }

  return 0;
}


/**************************************************************************/
bool_t HalPFlashNvmSetVariable(
  uint_fast8_t  no,         // id for variable, 0..255
  void *        vvalue,     // variable data
  uint_fast8_t  size        // size of data in bytes 1..HAL_FLASH_NVM_MAX_VARIABLE_SIZE
  )
{
  uint8_t * value = vvalue;
  uint_fast16_t by;
  VariableRecord_t varRec;
  uint8_t currentValue[HAL_FLASH_NVM_MAX_VARIABLE_SIZE+1];
  uint_fast16_t numSameBytes = 0;
  
  size =  MIN_VAL(size, HAL_FLASH_NVM_MAX_VARIABLE_SIZE);

  // get current value for this variable, if one exists
  // and compare with new value
  if (HalPFlashNvmGetVariable(no, currentValue, size)) {
    for (by = 0; by < size; by++) {
      if (value[by] == currentValue[by]) 
        numSameBytes++;
    }
  }
  // if new value is the same as the current value then no need to store
  // the new value
  if (numSameBytes == size) 
    return TRUE;

  // if sector is full then swap sectors
  if (nextFreeRecord >= (int_fast16_t)NVM_REC_NUMBERS) {
    if (activeSector == pSector1) {
      if (!NVOL_SwapSectors(pSector1, pSector2))
        return FALSE;
    } else if (activeSector == pSector2) {
      if (!NVOL_SwapSectors(pSector2, pSector1))
        return FALSE;
    } else {
      return FALSE;
    }
    if (nextFreeRecord >= (int_fast16_t)NVM_REC_NUMBERS) {
      return FALSE;
    }
  }

  varRec.flag = 0xAA;
  varRec.no = no;
  for (by = 0; by < size; by++) {
    varRec.data[by] = value[by];
  }
  for (; by < HAL_FLASH_NVM_MAX_VARIABLE_SIZE; by++) {
    varRec.data[by] = 0xFF;
  }

  varRec.checksum = no;
  for (by = 0; by < HAL_FLASH_NVM_MAX_VARIABLE_SIZE; by++) {
    varRec.checksum += varRec.data[by];
  }
  varRec.checksum = 0x100 - varRec.checksum;

  // store record in sector
  if (!NVOL_SetVariableRecord(&varRec, activeSector, nextFreeRecord)) 
    return FALSE;

  // get offset of next free location
  nextFreeRecord++;

  // add new variable record to lookup table
  NVOL_ConstructLookupTable();

  return TRUE;
}


/**************************************************************************/
bool_t HalPFlashNvmGetVariable(
  uint_fast8_t  id,         // id for variable, 0..255
  void          * vvalue,   // variable data
  uint_fast8_t  size        // size of data in bytes 1..12
  )
{

  if (lookupTable[id] < 0) {
    return FALSE;
  } else {
    memcpy(vvalue, activeSector->rec[lookupTable[id]].data, MIN_VAL(size, HAL_FLASH_NVM_MAX_VARIABLE_SIZE));
    return TRUE;
  }
}

/**************************************************************************/

void HalPFlashNvmErase(void)
{
  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
//  HAL_Delay(3000);
	HalReboot();
}


bool_t HalPFlashNvmInit(void)
{
//  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
//  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
// 
  // initialize sectors
  if ((activeSector = NVOL_InitSectors()) == 0)
    return FALSE;

  // generate lookup table
  NVOL_ConstructLookupTable();

  return TRUE;
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstFlashNvmShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
	int id, j;
	CliPrintf("DFLASH NVM:\n");
	CliPrintf("  activeSector: 0x%06X\n", activeSector);	
	
	for (id=0; id<HAL_FLASH_NVM_MAX_VARIABLE; id++) {
		if (lookupTable[id] < 0) {
			CliPrintf("  %u: not set\n", id);
		} else {
			uint8_t currentValue[HAL_FLASH_NVM_MAX_VARIABLE_SIZE];
      
			NvmReadIds(id, currentValue, HAL_FLASH_NVM_MAX_VARIABLE_SIZE);
      
			CliPrintf("  %u: ", id );
			for (j=0; j<HAL_FLASH_NVM_MAX_VARIABLE_SIZE; j++)
				CliPrintf("0x%02X ", currentValue[j]);
      
      
      CliPrintf(" (0x%08X)", (uint32_t)activeSector->rec[lookupTable[id]].data);
			CliPrintf("\n");
			
		}
	}
	
  return CLI_RESULT_OK;
}

static int_fast16_t TstFlashNvmTest(CliParam_t no, CliParam_t param2, CliParam_t param3)
{
	int i;
	char sz[HAL_FLASH_NVM_MAX_VARIABLE_SIZE];
	for (i=0; i<(uint32_t)no; i++) {
		sprintf(sz, "now:%u", i);
		NvmWriteIds(NVM_ID_XYZ, (uint8_t *)sz, strlen(sz)+1);
	}
	return TstFlashNvmShow(0,0,0);
}

static int_fast16_t TstFlashNvmInit(CliParam_t no, CliParam_t param2, CliParam_t param3)
{
  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  HalPFlashErase(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
	HalPFlashNvmInit();		
	return TstFlashNvmShow(0,0,0);
}
CLI_DECLARE_SUBTEST(trc)

CLI_START_TABLE(hal_nvm_flash)
  CLI_ENTRY0( "show", "Show FLASH NVM ...", TstFlashNvmShow)  

  CLI_ENTRY0( "init", "Initialize NVM PFLASH", TstFlashNvmInit)  
  CLI_ENTRY1( "test", "TEST ...[counts]", TstFlashNvmTest, CLI_PARAM_UINT32)  
  CLI_SUBTEST("trc", "Trace System", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_nvm_flash)

/******************************************************************************/
#endif
/******************************************************************************/

#endif
