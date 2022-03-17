/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <sys\types.h>
#include <sys\timeb.h>
#include <io.h>
#include <fcntl.h>
#include <sys\stat.h>
#include <assert.h>
#include <windows.h>

#include "UnitMain.h"

#include "hal/hal.h"
//#include "hal/eeprom/eeprom.h"
//#include "hal/i2c/i2c.h"

#include "plf/plf.h"
#include "plf/nvm/nvm.h"
//#include "plf/trc/trc.h"
//#include "plf/cli/cli.h"


uint_fast8_t HwVerGet(void)
{
  return 3;
}

/* IRQ **********************************************************************/

void HalInterruptDisable(HalIrqStat_t * istat) {};
void HalInterruptEnable(HalIrqStat_t * istat) {};
void HalInterruptRestore(HalIrqStat_t istat) {};


/* EEPROM **********************************************************************/

#include "hal/eeprom/eeprom.h"


static NvmStruct_t nvm;
volatile uint8_t * eeprom = (uint8_t*)&nvm;
static char eepromFileName[128];

void HalEepromSave(void)
{
	int   handle;
	handle = open(
		eepromFileName,
		O_WRONLY | O_CREAT | O_TRUNC | O_BINARY,
		S_IREAD | S_IWRITE);
	if (handle != -1) {
		write(handle, &nvm, sizeof(NvmStruct_t));
		close(handle);
	}
}

void HalEepromLoad(char * fileName)
{
	int   handle;
	strncpy(eepromFileName, (const char *)fileName, sizeof(eepromFileName)-1);
	handle = open(
		eepromFileName,
		O_RDONLY | O_BINARY,
		S_IREAD);
	if (handle != -1) {
		read(handle, &nvm, sizeof(NvmStruct_t));
		close(handle);
	} else {
		 memset(&nvm, 0, sizeof(NvmStruct_t));
		 HalEepromSave();
	}
}


void HalEepromWriteByte(
  uint_fast16_t addr,
  uint8_t b)
{
  assert(addr < sizeof(eeprom));
  eeprom[addr] = b;
}
void HalEepromReadByte(
  uint_fast16_t addr,
	uint8_t *b)
{
  assert(addr < sizeof(eeprom));
  *b = eeprom[addr];
}

bool HalEepromReadBlock(uint_fast32_t src, uint8_t * dst, uint_fast16_t len)
{
  uint8_t * b = (uint8_t*)dst;
  do {
    *b++ = eeprom[src++];
  } while (--len > 0);
  return TRUE;
}

bool HalEepromWriteBlock(uint_fast32_t dst, uint8_t * src, uint_fast16_t len)
{
  uint8_t * b = (uint8_t*)src;
  do {
    eeprom[dst++] = *b++;
  } while (--len > 0);
  return TRUE;
}


size_t HalEepromSizeGet(void)
{
  return sizeof(eeprom);
}


void EepromRead(unsigned addr, void * dst, size_t len)
{
  (void)HalEepromReadBlock(addr, (uint8_t*)dst, len);
}

void EepromWrite(unsigned addr, void const * src, size_t len)
{
  (void)HalEepromWriteBlock(addr, (uint8_t*)src, len);
}



/* *****************************************************************************/



const uint8_t  halNoOfStacks = 2;
//const char  halStackNames[2][20] = {"C stack", "Return stack"};


void HalUdelay(uint32_t t) {}



/* WDT ************************************************************************/

void HalWdtFeed(void)
{
}

void HalWdgReload(bool alternate) { }
int8_t HalWdgEnable(void) { return 0; }
int8_t HalWdgDisable(void) { return 0; }
void HalWdgInit(uint32_t timeout) {}

/* KEY ************************************************************************/

static void * xxx;

//extern HWND  hWndHandle;

void HalReadButtons(uint32_t  * keyState)
{
//  uint16_t key = 0;
  if (xxx)
    SendMessage((HWND)xxx, WM_USER, UM_KEY_GET, (long)keyState);
//  *keyState = key;

}


/******************************************************************************/

void HalInit(void)
{
}
 
/******************************************************************************/

void HalSetWndHandle(void * handle)
{
  xxx = handle;
}

void * HalGetWndHandle(void)
{
  return xxx;
}

/******************************************************************************/


void xHAL_Init(void)
{

}
