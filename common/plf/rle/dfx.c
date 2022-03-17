/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>

#include "hal/wdt/wdt.h"

#include "hal/pflash/pflash.h"
#include "hal/dflash/dflash.h"

#include "plf/blc/blc_def.h"
#define MAKE_CODE
#include "plf/rle/dfx.h"
#undef MAKE_CODE

#include "plf/trc/trc.h"


/******************************************************************************/

static SaveDataFunction_t pSaveDataFunction = NULL;

// destination
static uint32_t     imgAddr;      // next write address...
static uint32_t     imgAddrStart; //

/******************************************************************************/

static uint8_t      upBuf[XFLASH_BUFFER];
static uint32_t     upUsed;

static void SaveData(uint_fast8_t d)
{
  upBuf[upUsed++] = d;
  if (upUsed >= XFLASH_BUFFER) {
#if (PLF_OS != PLF_OS_WINDOWS)
    HalWdtFeed();
#endif
    if (pSaveDataFunction)
      pSaveDataFunction(imgAddr, upBuf);
    imgAddr += XFLASH_BUFFER;
    upUsed = 0;
  }
}

/******************************************************************************/

static int32_t UnpackFragment(uint8_t * rleData, int32_t len)
{
  static uint8_t    marker;
  uint32_t          count;
	uint32_t          i;
  int32_t           ix;
  uint8_t           symbol;

  ix = 0;
  if ((imgAddr+upUsed) == imgAddrStart) {  // First
	  marker = rleData[ix++];
  }

	do {
		symbol = rleData[ix++];

		if (symbol == marker) {
			count = rleData[ix++];

			if (count <= 2) {
				for (i = 0; i <= count; ++i) {
					SaveData(marker);
				}
			} else {
				if (count & 0x80) {
					count = ((count & 0x7f) << 8) + rleData[ix++];
				}

				symbol = rleData[ix++];

				for (i = 0; i <= count; ++i) {
					SaveData(symbol);
				}
			}
		} else {
			SaveData(symbol);
		}
	} while (ix < len);
  return ix;
}

/******************************************************************************/

bool CpxUnpack(
  SaveDataFunction_t  saveDataFunction,
  uint32_t dstAddr,
  uint32_t dstSize,
  uint32_t rleAddr)
{

  uint8_t               data[128];
  int32_t               len, used, rleLen;
  /* Start */
  upUsed = 0;
  pSaveDataFunction = saveDataFunction;
  imgAddrStart  = dstAddr;
  imgAddr       = imgAddrStart;

  /* Feed Data */
  len = 0;
  used = 0;

  while (imgAddr < (imgAddrStart + dstSize)) {
    if (len < (int32_t)sizeof(data)) {
      if (len > 0)
        memcpy(&data[0], &data[used], sizeof(data));
      rleLen = sizeof(data)-len;
#if defined(BLC_USE_DFLASH)
      if (HalDFlashRead(rleAddr, &data[len], rleLen) != DFLASH_OK) {
        TRACE(TRC_TA_PLF, TRC_TL_FATAL, "DFLASH read failed");
#elif defined(BLC_USE_PFLASH)
      if (!HalPFlashRead(rleAddr, (uint32_t)&data[len], rleLen)) {
        TRACE(TRC_TA_PLF, TRC_TL_FATAL, "PFLASH read failed");
#endif
        return false;
      }
      len += rleLen;
      rleAddr += rleLen;
    }

    used = UnpackFragment(data, sizeof(data)-5);
    if (used < 0)
      return false;
    len -= used;
    HalWdtFeed();
  }

  if (len > 0) {
    UnpackFragment(&data[used], len);
  }
  pSaveDataFunction = NULL;
  return true;
}

/******************************************************************************/

// buffer size: DFX_BUFFER
static bool _CpxUnpackToDFlash(uint32_t dflashAddr, uint8_t * upBuf)
{
#if defined(BLC_USE_DFLASH)
  if (HalDFlashWrite(dflashAddr, upBuf, DFX_BUFFER, true) != DFLASH_OK) {
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "DFLASH write failed");
#elif defined(BLC_USE_PFLASH)
  if (!HalPFlashWrite(dflashAddr, (uint32_t)upBuf, DFX_BUFFER)) {
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "PFLASH write failed");
#endif
    return false;
  }
  return true;
}

bool CpxUnpackToDFlash(
  uint32_t destAddr,
  uint32_t dstSize,
  uint32_t srcAddr)
{
#if defined(BLC_USE_DFLASH)
  if (HalDFlashEraseAddrRange(destAddr, destAddr + dstSize-1) != DFLASH_OK) {
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "DFLASH erease failed");
#elif defined(BLC_USE_PFLASH)
  if (!HalPFlashEraseAddrRange(destAddr, destAddr + dstSize-1)) {
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "PFLASH erease failed");
#endif
    return false;
  }
  return CpxUnpack(_CpxUnpackToDFlash, destAddr, dstSize, srcAddr);
}

// buffer size: DFX_BUFFER
static FWDescriptor_t * pFWD;
static Crc16_t crc;

static bool _CpxDfuCheck(uint32_t dstAddr, uint8_t * upBuf)
{
  if ((dstAddr <= BLC_FW_DESCIPTOR_ADDR) && ((dstAddr+DFX_BUFFER-BLC_FW_DESCIPTOR_SIZE) >= BLC_FW_DESCIPTOR_ADDR)) {
    pFWD = (FWDescriptor_t*)(upBuf + DFX_BUFFER - BLC_FW_DESCIPTOR_SIZE);
    crc = CalcCrc16(DFX_BUFFER - BLC_FW_DESCIPTOR_SIZE, upBuf, crc);
  } else {
    crc = CalcCrc16(DFX_BUFFER, upBuf, crc);
  }
  return true;
}

CpxDfuCheck_t CpxDfuCheck(
  uint32_t dFlashAddr, 
  FWDescriptor_t * fwd)
{
  pFWD = NULL;
  crc =  CRC16_INITIAL_SEED;

  if (!CpxUnpack(_CpxDfuCheck, APP_PROGRAM_FLASH_START, APP_PROGRAM_FLASH_SIZE, dFlashAddr))
    return CDC_UNPACK_ERROR;
  if (pFWD == NULL)
    return CDC_WRONG_SIZE;
  if (strcmp(pFWD->id, BOOTCTRL_HEADER_ID) != 0)
    return CDC_WRONG_ID;
  if (pFWD->crc != crc)
    return CDC_WRONG_CRC;
  *fwd = *pFWD;
  return CDC_OK;
}


/******************************************************************************/

#ifdef CLI_ENABLE

#endif

/******************************************************************************/

