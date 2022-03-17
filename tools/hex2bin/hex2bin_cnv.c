/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/



#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <dir.h>
#include <time.h>
#include <io.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <sys/timeb.h>

#include "plf/plf.h"
#include "hex2bin_cnv.h"
#include "plf/crc/crc16.h"

#include "plf/blc/blc.h"
#include "plf/blc/blc_def.h"
#include "plf/rle/rle.h"

//---------------------------------------------------------------------------

static char             destFile[1024];
static unsigned char    imageDataInt[APP_PROGRAM_FLASH_SIZE];
static unsigned long    lastIxInt = 0uL;
static unsigned long    firstIxInt = APP_PROGRAM_FLASH_SIZE;

//---------------------------------------------------------------------------

static uint8_t          fillChar = 0xFF;  // ???
static char             verType = 'D';  // Default Debug Version
static bool							doDfu = false;
static bool							doDfx = false;

/* FW definition */
FWDescriptor_t 	* pFWDescriptor;

/* Intel Hex definition */

typedef struct {
	unsigned char         head;
	unsigned char         chLen[2];
	unsigned char         chAddr[4];
	unsigned char         chRecId[2];
	union {
		struct {
			unsigned char     chOffset[4];
		}                   ihr_04;
		struct {
			unsigned char     chData[32];
		}                   ihr_00;
	};
}                       ih_t;

//---------------------------------------------------------------------------

static int _HexCh2Int(unsigned char hc)
{
	hc = toupper(hc);
  if (hc >= 'A') {
    return hc-'A'+10;
  } else {
    return hc-'0';
  }
}

static int _HexStrToInt(unsigned char * h, int len)
{
  int val;
  if (len==2) {
    val = _HexCh2Int(h[0]) * 16 +
          _HexCh2Int(h[1]);
  } else if (len==4) {
    val = _HexCh2Int(h[0]) * 16*16*16 +
          _HexCh2Int(h[1]) * 16*16 +
          _HexCh2Int(h[2]) * 16 +
          _HexCh2Int(h[3]);
  } else {
    val = 0;
  }
	return val;
}

uint8_t CalcCheckSum(char * sz)
{

  uint8_t val = 0;
  int i = strlen(sz)-1; /* ignore \n */
  int j = 1;


  while (j<i) {
    val += _HexStrToInt(&sz[j], 2);
    j += 2;
  }
  return  val;
}

uint8_t CalcNewCheckSum(char * sz)
{
  uint8_t cs = CalcCheckSum(sz);
  if (cs == 0)
    return 0;
  else
    return 0x100-cs;
}

bool CheckChecSum(char * sz)
{
  return  CalcCheckSum(sz) == 0;
}

const char hexTbl[] = "0123456789ABCDEF";

void strcathex(char * sz, uint8_t hex)
{
  int l = strlen(sz);
  sz[l++] = hexTbl[hex >> 4];
  sz[l++] = hexTbl[hex & 0xF];
  sz[l] = 0;

}
//---------------------------------------------------------------------------

typedef enum {
	CR_OK,
	CR_WrongParam,
	CR_NotAHexFile,
	CR_HEX_FileNotFound,
	CR_HEX_CheckSumErrorInFile,
	CR_HEX_NotPropperFile,
	CR_HEX_WrongFingerprint,
	CR_HEX_WdtNotEnabled,  // 7
  CR_HEX_DataOutOfRange,
	CR_BIN_FileCreationError,
	CR_BIN_FileWriteError,
	CR_Last
}                 ConvertResult_t;

const char * lstConvertResult[CR_Last] = {
	"OK",
	"WrongParam",
	"NotAHexFile",
	"HEX_FileNotFound",
	"HEX_CheckSumErrorInFile",
	"HEX_NotPropperFile",
	"HEX_WrongFingerprint",
	"HEX_WdtNotEnabled",
  "HEX_DataOutOfRange",
	"BIN_FileCreationError",
	"BIN_FileWriteError",
};

const char * crText[CR_Last] = {
		"OK",
		"Wrong Parameter",
		"Not a Hex File",
    "Hex File Not Found",
    "Check Sum Error in Hex File",
		"Not a Propper File",
		"Wrong Fingerpring in HEX file",
    "Watch Dog not enabled in Application",
    "Data Out Of Range"
		"File Creation Error",
    "FileWriteError"
};


static ConvertResult_t LoadHex(
  char                  * fiNam)
{
  FILE                  * fSrc;
	time_t                t;
	char                  sz[1024];
	ih_t                  * ih;
	long                   i;
	unsigned long         recOffset;
	int                   lcnt;
	int                   sum;
	unsigned long         addr;
	unsigned long         lastSegment;

  uint32_t a1, a2;
  a1 = APP_PROGRAM_FLASH_START;
  a2 = APP_PROGRAM_FLASH_END;

  // unprogrammed ares is in STM32F0 ???
	memset(&imageDataInt, fillChar, sizeof(imageDataInt));


	if ((fSrc = fopen(fiNam, "rt")) == NULL) {
		return CR_HEX_FileNotFound;
	}

	time(&t);

	ih = (ih_t *)sz;
	recOffset = 0;
  while (!feof(fSrc)) {
    unsigned char   len;
    unsigned long  segAddr;
    unsigned char   recId;
    fgets(sz, sizeof(sz), fSrc);
    if (ih->head != ':')
      return CR_NotAHexFile;
    if (!CheckChecSum(sz))
      return CR_HEX_CheckSumErrorInFile;
    recId = _HexStrToInt(ih->chRecId, 2);
    len   = _HexStrToInt(ih->chLen, 2);
    segAddr  = _HexStrToInt(ih->chAddr, 4);
    if (recId == 4) {
      // :02 0000 04 xxxx CS
      recOffset = _HexStrToInt(ih->ihr_04.chOffset, 4) << 16;

    } else if (recId == 0) {
      // :10 1000 00 DD100008C1100008C310000800000000 37
      addr = segAddr+recOffset;
      if ((addr >= a1) & (addr < a2)) {
        for (i=0; i < len; i++) {
          unsigned long ix =  addr-APP_PROGRAM_FLASH_START+i;
          unsigned char *ch = &ih->ihr_00.chData[i*2];
          imageDataInt[ix] = _HexStrToInt(ch, 2);
          if (ix>lastIxInt) {
						lastIxInt = ix;
          }
          if (ix<firstIxInt) {
            firstIxInt = ix;
					}
        }
      } else {
        //if (recOffset != 0x10000000) {
          return CR_HEX_DataOutOfRange;
        //}
      }
    }
  }
  fclose(fSrc);

  return CR_OK;

	}

//static Crc16_t crc;
//static Crc16_t extCrc;

static int PatchFile(void)
{

  /* Externa Flash */
	/* Internal Flash */
	if (strcmp(pFWDescriptor->id, BOOTCTRL_HEADER_ID) != 0) {
		return CR_HEX_WrongFingerprint;
  }

  if (!pFWDescriptor->wdtEnable) {
    //return CR_HEX_WdtNotEnabled;
	}
	//pFWDescriptor->wdtEnable = TRUE;


	pFWDescriptor->swVerType = verType; // D/r
	pFWDescriptor->crcGuard = RUN_FROM_PATCHED_FILE;
	pFWDescriptor->crc = CalcCrc16(APP_PROGRAM_FLASH_SIZE-BLC_FW_DESCIPTOR_SIZE, imageDataInt, CRC16_INITIAL_SEED);

	if (!pFWDescriptor->wdtEnable) {
		return CR_HEX_WdtNotEnabled;
	}

	return CR_OK;
}


#define BYTE_PR_LINE  (16)

static ConvertResult_t SaveBin(void)
{
	ConvertResult_t        ok;
	FILE                  * fDst;
	uint32_t							l;

	if ((fDst = fopen(destFile, "wb")) == NULL)
		 return CR_BIN_FileCreationError;
	l = fwrite(imageDataInt, 1, sizeof(imageDataInt), fDst);
	fclose(fDst);
  ok = (l == sizeof(imageDataInt) ? CR_OK : CR_BIN_FileWriteError);
  if (ok != CR_OK) return ok;

	return ok;
}

//---------------------------------------------------------------------------

// empty arrea ???
#define UNUSED_LEN  (256)
static int PctUsed(void)
{
  uint8_t unused[UNUSED_LEN];
  int i, j, u;

  memset(unused,fillChar, sizeof(unused));

  u = 0;
  for (i=0; i < APP_PROGRAM_FLASH_SIZE; i += UNUSED_LEN) {
    if (memcmp(&imageDataInt[i], unused, UNUSED_LEN) == 0) {
      u += UNUSED_LEN;
    }
  }

  //return ((SYSTEM_SIZE-u)*100)/SYSTEM_SIZE;

  return ((APP_PROGRAM_FLASH_SIZE-u)*100)/(APP_PROGRAM_FLASH_SIZE);

}

//uint32_t ss;
//  argv[0] == program name
//  argv[1] == source hex file
//  argv[2] == destination bin file - optionel

static char drive[1024];
static char dir[1024];
static char name[1024];

static int MyStrcmp(char * s1, char * s2)
{
	int i = 0;
	for(;;) {
		if (*s1 != *s2) return i;
		i++;
		s1++;
		if (*s1 == 0) return i;
		s2++;
		if (*s2 == 0) return i;
	}
}


static void MakeFileName(char * n, bool dafaultExt)
{
	char ext[1024];
	fnsplit(n, drive, dir, name, ext);
	fnmerge(destFile, drive, dir, name, dafaultExt ? ".dfu" : ext);

	mkdir(dir);

}

static ConvertResult_t ReadParametres(int argc, char * argv[])
{
	int i;


	if (argc < 2) {
		return CR_WrongParam;
	}


	MakeFileName(argv[1], true);

	if (argc >= 3) {
		for (i = 3; i <= argc; i++) {
			char * p = argv[i-1];
			if (MyStrcmp(p, "out=") == 4) {
				MakeFileName(&p[4], false);
			}
			else if (MyStrcmp(p, "fill=") == 5) {
				fillChar = atoi(&p[5]);
			}
			else if (MyStrcmp(p, "release") == 7) {
				verType = 'r';
			}
			else if (MyStrcmp(p, "dfu") == 3) {
				doDfu = true;
			}
			else if (MyStrcmp(p, "dfx") == 3) {
				doDfx = true;
			}
			else {
				// Unknown parameter
			}
		}
	}

	return CR_OK;

}



int Hex2Bin_Conv(int argc, char * argv[])
{
	//int fstart, fsize, fwd;
  int r;
	ConvertResult_t  cr, res;


	printf(PLF_VENDOR_NAME " - " PLF_PRODUCT_NAME " hex2bin converter (" __DATE__ ", " __TIME__ ")\n");


	//fwd = DFU_PROGRAM_FLASH_SIZE-BLC_FW_DESCIPTOR_SIZE;
	//fstart = _APPLIICATION_FLASH_START;
	//fsize = SYSTEM_SIZE;



/*
  r = APP_PROGRAM_FLASH_START;
  r = APP_PROGRAM_FLASH_SIZE;
  r = _NVM_PFLASH_SIZE;
  r = BLC_FW_DESCIPTOR_SIZE;
  r = BLC_FW_DESCIPTOR_ADDR;
  r = APP_PROGRAM_FLASH_START;
*/
  r = BLC_FW_DESCIPTOR_ADDR-APP_PROGRAM_FLASH_START;

  pFWDescriptor = (FWDescriptor_t *)&imageDataInt[r];

	cr = ReadParametres(argc, argv);
	res = cr;

	if (cr == CR_OK) {
		res = cr = LoadHex(argv[1]);
	}

	if (cr == CR_OK) {
		res = cr = PatchFile();
	}

	if (cr==CR_HEX_WdtNotEnabled) {
		printf("!!! WDT IS NOT ENABLED !!!\n");
	}

	if (((cr == CR_OK) || (cr==CR_HEX_WdtNotEnabled) ) && doDfu) {
		cr = SaveBin();
		if (cr == CR_OK) {
			printf("Binary file \"%s\" created (size:0x%08X, used:%u%%, type=%c, fill:%02X, CRC:0x%04X)\n",
        destFile, APP_PROGRAM_FLASH_SIZE, PctUsed(), verType, fillChar, pFWDescriptor->crc);

		} else {
			res = cr;
			printf("ERROR: \"%s\"\n", crText[cr]);
		}
	}

	if (((cr == CR_OK) || (cr==CR_HEX_WdtNotEnabled) ) && doDfx) {
		char rleFile[1024];
		size_t s;
		fnmerge(rleFile, drive, dir, name, ".dfx");
		s = RleCode(rleFile, imageDataInt, sizeof(imageDataInt));
		if (s != 0) {
			printf("Compressed file \"%s\" created (%u%% of bin)\n", rleFile, (100*s)/sizeof(imageDataInt));
		}

	}

	if ((cr != CR_OK) && (cr!=CR_HEX_WdtNotEnabled) ) {
    printf("Error: %s/#%d\n", lstConvertResult[cr], cr);
  }
	return res; //(res == CR_OK) ? 0 : 1;

}
