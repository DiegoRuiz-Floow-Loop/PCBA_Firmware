/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/hal.h"
#include "hal/pflash/pflash_nvm.h"
#include "hal/pflash/pflash.h"
//#include "plf/blc/blc.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"
#include "plf/nvm/nvm.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"

#if (NVM_ACCESS==NVM_ACCESS_ID)

/******************************************************************************/

typedef uint64_t                  SectorFlag_t[4];

// sector flags

static const SectorFlag_t sectorEmptyflag   = {
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU };
static const SectorFlag_t sectorInitFlag    = {
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU,
  0xAAAAAAAAAAAAAAAALLU };
static const SectorFlag_t sectorValidFlag   = {
  0xFFFFFFFFFFFFFFFFLLU,
  0xFFFFFFFFFFFFFFFFLLU,
  0xAAAAAAAAAAAAAAAALLU,
  0xAAAAAAAAAAAAAAAALLU };
static const SectorFlag_t sectorInvalidFlag = {
  0xFFFFFFFFFFFFFFFFLLU,
  0xAAAAAAAAAAAAAAAALLU,
  0xAAAAAAAAAAAAAAAALLU,
  0xAAAAAAAAAAAAAAAALLU };

#define SECTOR_EMPTY_FLAG_SHORT           (0xFFFFU)
#define SECTOR_INITIALIZING_FLAG_SHORT    (0xFFFAU)
#define SECTOR_VALID_FLAG_SHORT           (0xFFAAU)
#define SECTOR_INVALID_FLAG_SHORT         (0xFAAAU)
  
static uint16_t FlagToShortFlag(SectorFlag_t sectorFlag)
{
  return  (uint16_t)(0xF & sectorFlag[3]) 
       | ((uint16_t)(0xF & sectorFlag[2]) << 4) 
       | ((uint16_t)(0xF & sectorFlag[1]) << 8) 
       | ((uint16_t)(0xF & sectorFlag[0]) << 12);
}

// max number of variables
#define HAL_FLASH_NVM_MAX_VARIABLE       ((int)NVM_ID_Last)

/******************************************************************************/

#pragma pack(push, 1)

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
  SectorFlag_t          sectorFlag;
  VariableRecord_t      rec[NVM_REC_NUMBERS];  
}                       Sector_t;

#pragma pack(pop)

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
static bool EraseSector(
  Sector_t * sector )
{
  if (!HalPFlashEraseAddrRange((uint32_t)sector, (uint32_t)sector+_NVM_PFLASH_SECTOR_SIZE-1)) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "HalPFlashEraseAddrRange ERROR");
  }
  return true;
}

/**************************************************************************/

static bool SetSectorFlags(
  Sector_t        * sector,                                   // pointer to sector
  const SectorFlag_t    * flags                                     // new flags to write
  )
{
#ifdef PFLASH_WRITE_BUFFER  
  if (!HalPFlashWrite((uint32_t)&sector->sectorFlag, (uint32_t)flags, sizeof(SectorFlag_t))) {
    TRACE_VA(TRC_TA_HAL, TRC_TL_FATAL, 
    "HalPFlashWrite ERROR: %u, ec:%X, af:%X, ar:%X", 
    pflashStatus, pFlash.ErrorCode, (uint32_t)&sector->sectorFlag, (uint32_t)&flags);
    return false;
  }
#else
  HalPFlashWrite_64b((uint32_t)&sector->flag, flags);
#endif  
  return true;
}

/**************************************************************************/
static int_fast16_t GetNextFreeRecord(
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

static bool SetVariableRecord (
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
  return true;
}

/**************************************************************************/
static bool NVOL_IsVariableRecordValid(
  VariableRecord_t * pVarRec)
{
  uint8_t checksum;
  uint_fast16_t by;

  // check flags
  if (pVarRec->flag != 0xAA)
    return false;

  // check checksum
  checksum = pVarRec->no & 0xFF;
  //checksum += (pVarRec->id >> 8) & 0xFF;
  for (by = 0; by < HAL_FLASH_NVM_MAX_VARIABLE_SIZE; by++){
    checksum += pVarRec->data[by];
  }
  checksum = 0x100 - checksum;
  if (pVarRec->checksum != checksum) 
    return false;

  return true;
}

/**************************************************************************/
static void ConstructLookupTable(void)
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
static bool SwapSectors(
  Sector_t * srcSector,                               // pointer to source sector
  Sector_t * dstSector                               // pointer to destination sector
)
{
  uint_fast16_t var;
  uint_fast16_t dstRec;

  // make sure destination sector is erased
  if (dstSector->sectorFlag != sectorEmptyflag) {
    // erase it
    if (!EraseSector(dstSector)) 
      return false;
  }

  // mark destination sector as being initialized
  if (!SetSectorFlags(dstSector, &sectorInitFlag)) {
    return false;
  }
  dstRec = 0;

  // copy variables to destination sector
  for (var = 0; var < HAL_FLASH_NVM_MAX_VARIABLE; var++) {
    if (lookupTable[var] >= 0) {
      if (!SetVariableRecord(&srcSector->rec[lookupTable[var]], dstSector, dstRec)) 
        return false;
    }
    dstRec++;
  }

  // mark destination sector as being valid
  if (!SetSectorFlags(dstSector, &sectorValidFlag))
    return false;
  // mark source sector as being invalid
  if (!SetSectorFlags(srcSector, &sectorInvalidFlag))
    return false;

  // now using destination sector
  activeSector = dstSector;
  
  // get next free location in destination sector
  nextFreeRecord = GetNextFreeRecord(dstSector);
  
  // regenerate lookup table
  ConstructLookupTable();

  return true;
}

/**************************************************************************/

#define SECTOR_EMPTY_FLAG_SHORT           (0xFFFFU)
#define SECTOR_INITIALIZING_FLAG_SHORT    (0xFFFAU)
#define SECTOR_VALID_FLAG_SHORT           (0xFFAAU)
#define SECTOR_INVALID_FLAG_SHORT         (0xFAAAU)


static Sector_t * InitSectors(void)
{
  bool ok;
  uint16_t flags;
  // if sector 1 has invalid flags then erase it
  flags = FlagToShortFlag(pSector1->sectorFlag);
  ok = (flags == SECTOR_EMPTY_FLAG_SHORT) || (flags == SECTOR_VALID_FLAG_SHORT);
  
//  if ((pSector1->sectorFlag != sectorEmptyflag)        &&
//      (pSector1->sectorFlag != sectorInitFlag) &&
//      (pSector1->sectorFlag != sectorValidFlag)        &&
//      (pSector1->sectorFlag != sectorInvalidFlag))

  if (!ok) {
    EraseSector(pSector1);
  }

//  // if sector 2 has invalid flags then erase it
//  if ((pSector2->sectorFlag != sectorEmptyflag)        &&
//      (pSector2->sectorFlag != sectorInitFlag) &&
//      (pSector2->sectorFlag != sectorValidFlag)        &&
//      (pSector2->sectorFlag != sectorInvalidFlag))
  flags = FlagToShortFlag(pSector2->sectorFlag);
  ok = (flags == SECTOR_EMPTY_FLAG_SHORT) || (flags == SECTOR_VALID_FLAG_SHORT);
  if (!ok) {
    EraseSector(pSector2);
  }

  // what happens next depends on status of both sectors
  switch (FlagToShortFlag(pSector1->sectorFlag)) {
    case SECTOR_EMPTY_FLAG_SHORT:
      switch (FlagToShortFlag(pSector2->sectorFlag)) {
        // sector 1 empty, sector 2 empty
        case SECTOR_EMPTY_FLAG_SHORT:
          // use sector 1
          if (!SetSectorFlags(pSector1, &sectorValidFlag))
            return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 empty, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG_SHORT:
          // use sector 2
          if (!SetSectorFlags(pSector2, &sectorValidFlag))
            return 0;
          nextFreeRecord = 0;
          return pSector2;
        // sector 1 empty, sector 2 valid
        case SECTOR_VALID_FLAG_SHORT:
          nextFreeRecord = GetNextFreeRecord(pSector2);
          // sector 2 is already active
          return pSector2;
        // sector 1 empty, sector 2 invalid
        case SECTOR_INVALID_FLAG_SHORT:
          // swap sectors 2 -> 1
          if (!SwapSectors(pSector2, pSector1))
            return 0;
          nextFreeRecord = GetNextFreeRecord(pSector1);
          // use sector 1
          return pSector1;
      }
    break;

    case SECTOR_INITIALIZING_FLAG_SHORT:
      switch (FlagToShortFlag(pSector2->sectorFlag)) {
        // sector 1 initializing, sector 2 empty
        case SECTOR_EMPTY_FLAG_SHORT:
          // use sector 1
          if (!SetSectorFlags(pSector1, &sectorValidFlag)) return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 initializing, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG_SHORT:
          // erase sector 2
          if (!EraseSector(pSector2)) return 0;
          // use sector 1
          if (!SetSectorFlags(pSector1, &sectorValidFlag)) return 0;
          nextFreeRecord = 0;
          return pSector1;
        // sector 1 initializing, sector 2 valid
        case SECTOR_VALID_FLAG_SHORT:
          // erase sector 1
          if (!EraseSector(pSector1)) return 0;
            // swap sectors 2 -> 1
          if (!SwapSectors(pSector2, pSector1)) return 0;
            nextFreeRecord = GetNextFreeRecord(pSector1);
          // use sector 1
          return pSector1;
        // sector 1 initializing, sector 2 invalid
        case SECTOR_INVALID_FLAG_SHORT:
          // erase sector 2
          if (!EraseSector(pSector2)) return 0;
          // use sector 1
          if (!SetSectorFlags(pSector1, &sectorValidFlag)) return 0;
          nextFreeRecord = 0;
          return pSector1;
      }
      break;

    case SECTOR_VALID_FLAG_SHORT:
      switch (FlagToShortFlag(pSector2->sectorFlag)) {
        // sector 1 valid, sector 2 empty
        case SECTOR_EMPTY_FLAG_SHORT:
            nextFreeRecord = GetNextFreeRecord(pSector1);
          // sector 1 is active
          return pSector1;
        // sector 1 valid, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG_SHORT:
          // erase sector 2
          if (!EraseSector(pSector2)) return 0;
            // swap sectors 1 -> 2
          if (!SwapSectors(pSector1, pSector2)) return 0;
            nextFreeRecord = GetNextFreeRecord(pSector2);
          // use sector 2
          return pSector2;
        // sector 1 valid, sector 2 valid
        case SECTOR_VALID_FLAG_SHORT:
          // erase sector 2 and use sector 1
          if (!EraseSector(pSector2)) return 0;
            nextFreeRecord = GetNextFreeRecord(pSector1);
          return pSector1;
        // sector 1 valid, sector 2 invalid
        case SECTOR_INVALID_FLAG_SHORT:
          // erase sector 2 and use sector 1
          if (!EraseSector(pSector2)) return 0;
            nextFreeRecord = GetNextFreeRecord(pSector1);
          return pSector1;
      }
      break;

    case SECTOR_INVALID_FLAG_SHORT:
      switch (FlagToShortFlag(pSector2->sectorFlag)) {
        // sector 1 invalid, sector 2 empty
        case SECTOR_EMPTY_FLAG_SHORT:
          // swap sectors 1 -> 2
          if (!SwapSectors(pSector1, pSector2)) return 0;
          // use sector 2
            nextFreeRecord = GetNextFreeRecord(pSector2);
          return pSector2;
        // sector 1 invalid, sector 2 initializing
        case SECTOR_INITIALIZING_FLAG_SHORT:
          // erase sector 1
          if (!EraseSector(pSector1)) return 0;
          // use sector 2
          if (!SetSectorFlags(pSector2, &sectorValidFlag)) return 0;
          nextFreeRecord = 0;
          return pSector2;
        // sector 1 invalid, sector 2 valid
        case SECTOR_VALID_FLAG_SHORT:
          // erase sector 1
          if (!EraseSector(pSector1)) return 0;
            nextFreeRecord = GetNextFreeRecord(pSector2);
          // use sector 2
          return pSector2;
        // sector 1 invalid, sector 2 invalid
        case SECTOR_INVALID_FLAG_SHORT:
          // both sectors invalid so erase both and use sector 1
          if (!EraseSector(pSector1)) return 0;
          if (!EraseSector(pSector2)) return 0;
          if (!SetSectorFlags(pSector1, &sectorValidFlag)) return 0;
          nextFreeRecord = 0;
          return pSector1;
      }
      break;
  }

  return 0;
}


/**************************************************************************/

bool HalPFlashNvmSet(
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
  if (HalPFlashNvmGet(no, currentValue, size)) {
    for (by = 0; by < size; by++) {
      if (value[by] == currentValue[by]) 
        numSameBytes++;
    }
  }
  // if new value is the same as the current value then no need to store
  // the new value
  if (numSameBytes == size) 
    return true;

  // if sector is full then swap sectors
  if (nextFreeRecord >= (int_fast16_t)NVM_REC_NUMBERS) {
    if (activeSector == pSector1) {
      if (!SwapSectors(pSector1, pSector2)) {
        TRACE(TRC_TA_HAL, TRC_TL_FATAL, "Swap Sectors 1-2 ERROR");
        return false;
      }
    } else if (activeSector == pSector2) {
      if (!SwapSectors(pSector2, pSector1)) {
        TRACE(TRC_TA_HAL, TRC_TL_FATAL, "Swap Sectors 2-1 ERROR");
        return false;
      }
    } else {
      TRACE(TRC_TA_HAL, TRC_TL_FATAL, "activeSector ...ERROR");
      return false;
    }
    
    if (nextFreeRecord >= (int_fast16_t)NVM_REC_NUMBERS) {
      TRACE(TRC_TA_HAL, TRC_TL_FATAL, "nextFreeRecord ERROR");
      return false;
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
  if (!SetVariableRecord(&varRec, activeSector, nextFreeRecord)) {
    TRACE(TRC_TA_HAL, TRC_TL_FATAL, "SetVariableRecord ERROR");
    return false;
  } else {
    TRACE_BLK(TRC_TA_HAL, "HalPFlashNvmSet", vvalue, size);
  }

  // get offset of next free location
  nextFreeRecord++;

  // add new variable record to lookup table
  ConstructLookupTable();

  return true;
}


/**************************************************************************/

bool HalPFlashNvmGet(
  uint_fast8_t  id,         // id for variable, 0..255
  void          * vvalue,   // variable data
  uint_fast8_t  size        // size of data in bytes 1..
  )
{
  if (lookupTable[id] < 0) {
    memset(vvalue, 0xFF, MIN_VAL(size, HAL_FLASH_NVM_MAX_VARIABLE_SIZE));
    TRACE(TRC_TA_HAL, TRC_TL_ERROR, "HalPFlashNvmGet lookup ERROR");
    return false;
  } else {
    memcpy(vvalue, activeSector->rec[lookupTable[id]].data, MIN_VAL(size, HAL_FLASH_NVM_MAX_VARIABLE_SIZE));
    TRACE_BLK(TRC_TA_HAL, "HalPFlashNvmGet", vvalue, size);
    return true;
  }
}

/**************************************************************************/

void HalPFlashNvmErase(void)
{
  TRACE(TRC_TA_HAL, TRC_TL_4, "HalPFlashNvmErase");
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
//  HAL_Delay(3000);
	HalReboot();
}


bool HalPFlashNvmInit(void)
{
//  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
//  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);

  // initialize sectors
  if ((activeSector = InitSectors()) == 0) {
    HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
    HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
    if ((activeSector = InitSectors()) == 0) {
      TRACE(TRC_TA_HAL, TRC_TL_FATAL, "InitSectors ERROR");
      return false;
    }
  }

  // generate lookup table
  ConstructLookupTable();


  return true;
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t TstHalFlashNvmShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
	int id, j;
  if (cliShowHeader) {
	  CliPrintf("PFLASH NVM:\n");
	  CliPrintf("  activeSector: 0x%06X\n", activeSector);	
  }
	
	for (id=0; id<HAL_FLASH_NVM_MAX_VARIABLE; id++) {
		if (lookupTable[id] < 0) {
			CliPrintf("  %u: not set\n", id);
		} else {
			uint8_t currentValue[HAL_FLASH_NVM_MAX_VARIABLE_SIZE];
      
			NvmReadIds((NvmId_t)id, currentValue, HAL_FLASH_NVM_MAX_VARIABLE_SIZE);
      
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
		snprintf(sz, sizeof(sz), "n:%u, t:" PRINTF_PLFTIME_UINT, i, evosCurrentTime);
		NvmWriteIds(NVM_ID_XYZ, (uint8_t *)sz, strlen(sz)+1);
	}
	return TstHalFlashNvmShow(0,0,0);
}

static int_fast16_t TstFlashNvmTErase(CliParam_t no, CliParam_t param2, CliParam_t param3)
{
  uint32_t ad;
  uint8_t v = 0;
  char sz[16];
  char sz2[16];

  HalPFlashEraseAddrRange(0x080FE000, 0x080FE7FF);
  HalPFlashEraseAddrRange(0x080FE800, 0x080FEFFF);
  HalPFlashEraseAddrRange(0x080FF000, 0x080FF7FF);
  HalPFlashEraseAddrRange(0x080FF800, 0x080FFFFF); 

  memset(sz, 0xFF, sizeof(sz));
  for (ad = 0x080FE000; ad < 0x08100000; ad += sizeof(sz)) {
    HalPFlashRead(ad, (uint32_t)sz2, sizeof(sz));
    if (memcmp(sz, sz2, sizeof(sz)) != 0) {
      CliPrintf("Erase Error: 0x%08X" CLI_NL, ad);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
  }

  for (ad = 0x080FE000; ad < 0x08100000; ad += sizeof(sz)) {
    memset(sz,v++, sizeof(sz));
    HalPFlashWrite(ad, (uint32_t)sz, sizeof(sz));
    HalPFlashRead(ad, (uint32_t)sz2, sizeof(sz));
    if (memcmp(sz, sz2, sizeof(sz)) != 0) {
      CliPrintf("Write Error: 0x%08X" CLI_NL, ad);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
  }  
  
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  for (ad = HAL_PFLASH_NVM_SECTOR_1_ADDR; ad < HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE; ad += sizeof(sz)) {
    HalPFlashRead(ad, (uint32_t)sz2, sizeof(sz));
    if (memcmp(sz, sz2, sizeof(sz)) != 0) {
      CliPrintf("Erase Error: 0x%08X" CLI_NL, ad);
      return CLI_RESULT_ERROR_UNDEFINED;
    }
  }
  
	return CLI_RESULT_OK;
}

static int_fast16_t TstFlashNvmInit(CliParam_t no, CliParam_t param2, CliParam_t param3)
{
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_1_ADDR, HAL_PFLASH_NVM_SECTOR_1_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
  HalPFlashEraseAddrRange(HAL_PFLASH_NVM_SECTOR_2_ADDR, HAL_PFLASH_NVM_SECTOR_2_ADDR+_NVM_PFLASH_SECTOR_SIZE-1);
	HalPFlashNvmInit();		
	return TstHalFlashNvmShow(0,0,0);
}

//#define READ_LINE_LENGTH (16)
//int_fast8_t CliFlashRead(CliParam_t param1, CliParam_t param2, CliParam_t param3)
//{
//  uint32_t addr = (uint32_t)param1;
//  int32_t length = (uint32_t)param2;
//  uint_fast8_t width = (uint8_t)param3;
//  uint16_t i;
//  union {
//    uint8_t   w8[READ_LINE_LENGTH];
//    uint16_t  w16[READ_LINE_LENGTH>>1];
//    uint32_t  w32[READ_LINE_LENGTH>>2];
//    uint64_t  w64[READ_LINE_LENGTH>>3];
//  } data;
//  bool ok;

//  switch (width) {
//    case 8:
//    case 4:
//    case 2:
//    case 1:
//      break;
//    default:
//      return CLI_RESULT_ERROR_PARAMETER_VALUE;
//  }
//  
//  length = (((length + READ_LINE_LENGTH -1) / READ_LINE_LENGTH) * READ_LINE_LENGTH);
//  

//  for (;;) {
//    ok = HalPFlashRead(addr, (uint32_t)data.w8, READ_LINE_LENGTH);
//    if (ok) {
//      CliPrintf("0x%08x: ", addr);
//      switch (width) {
//        case 8:
//          for (i = 0; i < READ_LINE_LENGTH>>3; i++) {
//            CliPrintf("%016llx ", data.w64[i]);
//          }
//          break;
//        case 4:
//          for (i = 0; i < READ_LINE_LENGTH>>2; i++) {
//            CliPrintf("%08lx ", data.w32[i]);
//          }
//          break;
//        case 2:
//          for (i = 0; i < READ_LINE_LENGTH>>1; i++) {
//            CliPrintf("%04x ", data.w16[i]);
//          }
//          break;
//        default:
//          for (i = 0; i < READ_LINE_LENGTH; i++) {
//            CliPrintf("%02x ", data.w8[i]);
//          }
//          break;
//      }
//      CliWrite("    ");
//      
//      for (i = 0; i < READ_LINE_LENGTH; i++) {
//        if ((data.w8[i] >= ' ')  && (data.w8[i] < 0x80))
//          CliWriteChar(data.w8[i]);
//        else
//          CliWriteChar('.');
//      }
//      CliWrite(cliNewLine);
//    } else {
//     
//      break;
//    }
//    addr += READ_LINE_LENGTH;
//    length -= READ_LINE_LENGTH;
//    if (length < READ_LINE_LENGTH)
//      break;    
//  }
//  
//  if (ok) {
//    return CLI_RESULT_OK;
//  } else {
//    CliPrintf("Error:" CLI_NL);
//    return CLI_RESULT_ERROR_UNDEFINED;
//  }
//}

CLI_DECLARE_SUBTEST(trc)

extern int_fast8_t CliFlashRead(CliParam_t param1, CliParam_t param2, CliParam_t param3);


CLI_START_TABLE(hal_nvm_flash)
  CLI_ENTRY0( "show", "Show FLASH NVM ...", TstHalFlashNvmShow)  

  CLI_ENTRY0( "init", "Initialize NVM PFLASH", TstFlashNvmInit)  
  CLI_ENTRY1( "titem", "TEST ...[counts]", TstFlashNvmTest, CLI_PARAM_UINT32)  
  CLI_ENTRY0( "terase", "TEST erase", TstFlashNvmTErase)  

  CLI_ENTRY3( "rd", "Read from flash <addr> <len> <width: 1, 2, 4, 8>", CliFlashRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
  //CLI_ENTRY3( "wr", "Write to flash <addr> <byte> <cnt (max 16)>", CliFlashWrite, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)

  CLI_SUBTEST("trc", "Trace System", trc)
  CLI_ENTRY0( "reboot",   "Re-boot System", CliReboot)
CLI_END_TABLE(hal_nvm_flash)

/******************************************************************************/
#endif
/******************************************************************************/

#endif
