/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include "base.h"
#include "stm32l1xx_flash.h"
#include "HalPFlash.h"
#include "Hal.h"
#include "FailSafe.h"


/* FLASH Memory Programming functions *****************************************/   

bool8_t HalPFlashRead(void * pflashAddr,  void * ramAddr, uint16_t len)
{
    memcpy(ramAddr, pflashAddr, len);
    return TRUE;    
}

/****************************************************************************/

bool8_t HalPFlashErase(void * pflsahAddr,  uint16_t len)
{
    uint32_t pages, addr;
    int i;
    
    if ((((int)pflsahAddr>>2)<<2) != (int)pflsahAddr) {
        FailSafeErrorHandler(FS_HAL_PFLASH_ERASE_ODD_ADDR, (uint32_t)pflsahAddr);
        return FALSE;
    }
    
    FLASH_Unlock();
    FLASH_ClearFlag(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);    

    // Erase pages
    pages = (len + FLASH_PAGE_SIZE - 1) / FLASH_PAGE_SIZE;
    addr =  ((uint32_t)pflsahAddr / FLASH_PAGE_SIZE) * FLASH_PAGE_SIZE;
    for (i=0; i < pages; i++) {
        if (FLASH_ErasePage(addr+(i*FLASH_PAGE_SIZE)) != FLASH_COMPLETE) {
            FLASH_Lock();
            FailSafeErrorHandler(FS_HAL_PFLASH_ERASE_FAIL, (uint32_t)pflsahAddr);
            return FALSE;
        }
    }
    
    FLASH_Lock();
    return TRUE;
}

/****************************************************************************/

bool8_t HalPFlashWrite(void * pflsahAddr,  void * ramAddr, uint16_t len, bool8_t eraseFirst)
{
    uint32_t i, *data;
    
    if ((((int)pflsahAddr>>2)<<2) != (int)pflsahAddr) return FALSE;
    if ((((int)ramAddr>>1)<<1) != (int)ramAddr) return FALSE;

    
    FLASH_Unlock();
    FLASH_ClearFlag(FLASH_FLAG_BSY | FLASH_FLAG_EOP| FLASH_FLAG_PGAERR | FLASH_FLAG_WRPERR);    

    // Erase pages
    if (eraseFirst) {
        if (!HalPFlashErase(pflsahAddr, len)) {
            return FALSE;
        }
    }
    
    // Write data
    FLASH_Unlock();
    data = (uint32_t*)ramAddr;
    for (i=0; i<len; i += 4) {
        if (FLASH_FastProgramWord(i+(uint32_t)pflsahAddr, *data) != FLASH_COMPLETE) {
            FLASH_Lock();
            FailSafeErrorHandler(FS_HAL_PFLASH_WRITE_FAIL, (uint32_t)pflsahAddr);
            return FALSE;
        }
        data++;
    }

    FLASH_Lock();
    return TRUE;
}

/****************************************************************************/

void HalPFlashWrite_SPECIAL(
    uint32_t dst,
    int16_t len)
{
    uint32_t        * data;
    uint32_t        lastAddr;
    FLASH_Status    status;   

    /* Erase pages */
    if (!HalPFlashErase((void *)dst, len))
        return ;
    
    data = (uint32_t *)APPL_RAM_START; 
    lastAddr = 0x1000 + (uint32_t)dst;
    do {
        HalInterruptDisable();
        FLASH_Unlock();
        status = FLASH_ProgramHalfPage(dst, data); 
        FLASH_Lock();
        HalInterruptEnable();
        if (status != FLASH_COMPLETE) {
            FailSafeErrorHandler(FS_HAL_PFLASH_HALF_PAGE_WRITE_FAIL, (uint32_t)dst);
            return;
        }
        data += 32;  /* 32 bit ptr */
        dst += 128;  /* byte counter */      
    } while (dst<lastAddr);

    FLASH_Lock();
    return;
}


/****************************************************************************/

void HalPFlashInit(void)
{
    // nop
}
