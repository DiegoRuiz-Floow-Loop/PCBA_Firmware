/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdio.h>
#include <string.h>

#include "sif/sif.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"
#include "plf/fifo/fifo.h"
#include "plf/nvm/nvm.h"
#include "plf/ver/ver.h"
#include "plf/blc/blc.h"
#include "plf/heap/heapx.h"

#include "hal/hal.h"
#include "hal/uart/uart.h"
#include "hal/rtc/rtc.h"

#include "app/app.h"

/******************************************************************************/

#define TX_BUF_SIZE     (128)
typedef FifoT(TX_BUF_SIZE) TxBuf_t;
static TxBuf_t          sifTxBuf;

#define RX_BUF_SIZE     (32 )
typedef FifoT(RX_BUF_SIZE) RxBuf_t;
static RxBuf_t          sifRxBuf;

static EvosEventHandle_t  sifRx;

Sif_t                   sif;

/******************************************************************************/

bool SifTxBusy(void)
{
  return !HalUartTxEmpty(HAL_UART_SIF);
}

static void SifCheckTxEmpty(void)
{
  if (HalUartTxEmpty(HAL_UART_SIF)) {
    uint8_t xx;
    if (FifoGet(&sifTxBuf, &xx)) {
      HalUartTxFirst(HAL_UART_SIF, xx);
    }
  }
}
void SifWriteCh(uint_fast8_t ch)
{
  int cnt = 0;
  while (!FifoPut(&sifTxBuf, ch)) {
    SifCheckTxEmpty();  
    if (++cnt > 1000) break;
  }
  SifCheckTxEmpty();
}

void SifWrite(const char * stringPtr)
{
  while (*stringPtr) {
    SifWriteCh(*stringPtr++);
  }
}

void SifWriteLn(const char * stringPtr)
{
  SifWrite(stringPtr);
  SifWrite(cliNewLine);
}

void SifTxFlush(void)
{
	while (!FifoIsEmpty(&sifTxBuf)) {
		SifCheckTxEmpty();
	}
}

/******************************************************************************/

#define VT100_WindowSet2           "\x1B[3;r"  /* lines + 1 */

static const char VT100_Save[] = VT100_CursorHide VT100_CursorAttrSave VT100_CursorHome;
static const char VT100_Restore[] = VT100_CursorAttrRestore VT100_CursorShow;
const char VT100_EnableBanner[] = VT100_CursorAttrSave VT100_WindowSet2 VT100_CursorAttrRestore;
const char VT100_DisableBanner[] = VT100_CursorAttrSave VT100_WindowEnd VT100_CursorAttrRestore;


#ifdef TRC_ENABLE
extern TrcConfig_t trcConfig;
#endif

#if (NVM_ACCESS==NVM_ACCESS_NAME)
static void SifNvmPut(EvosEventParam_t p)
{
  NvmReadWriteResult_t s;
  s = NVM_WRITE(NVM_ID_SIF, sif);
  if (s == NRWR_OK) {
    EvosEventDelete(EVOS_CURRENT_HANDLE);
  }
}
#endif


void BannerToggle(void)
{
  sif.banner = !sif.banner;
#if (NVM_ACCESS==NVM_ACCESS_NAME)
  EvosEventSetAndReload(EvosEventRegister(SifNvmPut, "SifNvmPut"), 0, 10, 0);
#elif (NVM_ACCESS==NVM_ACCESS_ADDRESS)
  NvmWrite(offsetof(NvmStruct_t, appl.sif), (uint8_t *)&sif, sizeof(Sif_t));
#elif (NVM_ACCESS==NVM_ACCESS_ID)
  NVM_WRITE(NVM_ID_SIF, sif);
#else
  #error ""
#endif  

  SifWrite(sif.banner ? VT100_EnableBanner : VT100_DisableBanner);
}

  
#define LINE_LENGTH (60)

static void SifBanner(EvosEventParam_t argument)
{
  if (sif.banner) {
    int i;
    char xx[2*(LINE_LENGTH+4)];
    char * psz;
    //RtcTime_t rtcTime;
    
    // 1st line
    psz = xx;
    
    strcpy(psz, "| ");
    strcat(psz, applicationString);
    //sprintf(&psz[strlen(psz)], " | #%u", app.no);
    
#ifdef TRC_ENABLE    
    strcat(psz, " | ");
    if (trcConfig.trcEnabled)
      strcat(psz, "T");
    else
      strcat(psz, ".");
#endif
    
    int used, pct;
    HeapxUsed(&used, &pct);
    sprintf(&psz[strlen(psz)], " | H:%u/%u%%", used, pct);

    for (i=strlen(psz); i<=LINE_LENGTH; i++) {
      psz[i] = ' ';
    }
    psz[i++] = '|';
    psz[i++] = '\r';
    psz[i++] = '\n';
    
    // 2st line
    psz = &psz[i];    
    strcpy(psz, "| UTC:");
    //RtcGet(&rtcTime);
    RtcGetStr(&psz[strlen(psz)]);
    strcat(&psz[strlen(psz)], " | Up time:");    
    PlfTime_DHMS_Str(&psz[strlen(psz)], evosCurrentTime);
    
    for (i=strlen(psz); i<=LINE_LENGTH; i++) {
      psz[i] = ' ';
    }
    psz[i++] = '|';
//    psz[i++] = '\r';
//    psz[i++] = '\n';
    psz[i] = 0;
    

    SifWrite(VT100_Save);
    SifWrite(xx);    
    SifWrite(VT100_Restore);    

  }
}

void LogToggle(void)
{
 }
/******************************************************************************/

bool SifCharReady(void)
{
  return (!FifoIsEmpty(&sifRxBuf));
}

volatile bool sifRxFuncHasRun = false;
static void SifRx(EvosEventParam_t p)
{
  uint8_t ch;
  while (FifoGet(&sifRxBuf, &ch)) {
#ifdef CLI_ENABLE
    CliHandleInput(ch);
#endif    
  }
}

/******************************************************************************/

#ifdef TRC_ENABLE
static TrcOutputFunction_t oldTrcPrint = 0;

static void SifMngTraceSend(const char * szTrace)
{
  if (szTrace) {
    SifWrite(szTrace);
  }
  if (oldTrcPrint) {
    oldTrcPrint(szTrace);
  }
}
#endif


static bool SifTxCallback(uint8_t * ch)
{
  return FifoGet(&sifTxBuf, ch);
}

static void SifRxCallback(uint8_t ch)
{
  FifoPut(&sifRxBuf, ch);
  EvosEventSetNow(sifRx, 0);
}

/******************************************************************************/

#ifdef CLI_ENABLE
static CliSend_t oldCliSend = 0;

static void MySend(const char * szDebug)
{
  const char * psz = szDebug;

  while (*szDebug) {
    SifWriteCh(*szDebug++);
  }
  if (oldCliSend)
    oldCliSend(psz);
}
#endif

/******************************************************************************/

#ifdef CLI_ENABLE
/* in SifControllerMenu */
CLI_DECLARE_SUBTEST(root) // CliRoot_Table
#endif

//extern uint32_t rccCsr;

void SIF_Init(bool lowLevel)
{
  char sz[80];
  if (lowLevel) {
    memset(&sif, 0, sizeof(sif));

    sifRx = EvosEventRegister(SifRx, "SifRx");
    EvosEventSetDelta(sifRx, 100, 0);
    FIFO_INIT(&sifRxBuf, RX_BUF_SIZE);
    FIFO_INIT(&sifTxBuf, TX_BUF_SIZE);
    HalUartInit(HAL_UART_SIF, SifRxCallback, SifTxCallback, MCUPIN_NA);
    
    SifWrite(cliNewLine);
    SifWriteLn(applicationString);
    SifWrite("Application started" " (ver:" _STRIZE(PLF_VERSION_MAJOR) "." _STRIZE(PLF_VERSION_MINOR));

    uint8_t hc;
    HalCfgGet(&hc);
    sprintf(sz, "), Cfg:%u, HW Rev:%u", hc, HalHwRevGet());
    SifWriteLn(sz);
  } else {    
    #define REBOOT_STR_MAX    (64)
    typedef char            RebootSz_t[REBOOT_STR_MAX];
    RebootSz_t szSifReset = "Boot Reason: ";

    #if (NVM_ACCESS!=NVM_ACCESS_NAME)
      if (nvmState != NVM_OK) {
        memset(&sif, 0, sizeof(sif));
        sif.banner = false;
        #if (NVM_ACCESS==NVM_ACCESS_ADDRESS)
          NvmWrite(offsetof(NvmStruct_t, appl.sif), (uint8_t *)&sif, sizeof(Sif_t));
        #else
          NVM_WRITE(NVM_ID_SIF, sif);
        #endif
      } else {
        #if (NVM_ACCESS==NVM_ACCESS_ADDRESS)
          NvmRead(offsetof(NvmStruct_t, appl.sif), (uint8_t *)&sif, sizeof(sif));
        #else
          NVM_READ(NVM_ID_SIF, sif);
        #endif
      }
    #endif    

    EvosEventSetAndReload(EvosEventRegister(SifBanner, "SifBanner"), 0, 1000, 0);

    #ifdef CLI_ENABLE
      CliInit(root_Table);  
      oldCliSend  = CliOutputDefine(MySend);  
    #endif  

      #ifdef TRC_ENABLE
      TrcInit();
      oldTrcPrint = TrcOutputDefine(SifMngTraceSend);
      #if !defined(CLI_DEBUG_COMMANDS)
        TrcNvmInit();
      #endif 
    #endif  


    SifWrite((sif.banner) ? VT100_EnableBanner : VT100_DisableBanner);
    
    if (pBootControl->rccCsr & RCC_CSR_PINRSTF) {  /* PIN reset flag  */
      strcat(szSifReset, "/PIN");
    } 
    #if defined(RCC_CSR_PORRSTF)
      if (pBootControl->rccCsr & RCC_CSR_PORRSTF) {  /* POR/PDR reset flag */
        strcat(szSifReset, "/POR+PDR");
      } 
    #endif
    if (pBootControl->rccCsr & RCC_CSR_IWDGRSTF) { /* Iwatchdog reset flags */
      strcat(szSifReset, "/IWDT");
    } 
    if (pBootControl->rccCsr & RCC_CSR_WWDGRSTF) { /* Wwatchdog reset flags */
      strcat(szSifReset, "/WWDT");
    } 
    if (pBootControl->rccCsr & RCC_CSR_LPWRRSTF) { /* low power reset flags */
      strcat(szSifReset, "/LPW");
    } 
    #if defined(RCC_CSR_OBLRSTF)
      if (pBootControl->rccCsr & RCC_CSR_OBLRSTF) {  /*!< OBL reset flag */
        strcat(szSifReset, "/OBL");
      } 
    #endif
    #if defined(RCC_CSR_V18PWRRSTF)
      if (pBootControl->rccCsr & RCC_CSR_V18PWRRSTF) {  /*!< Reset flag of the 1.8 V domain. */
        strcat(szSifReset, "/V18");
      } 
    #endif   
    #if defined(RCC_CSR_BORRSTF)
      if (pBootControl->rccCsr & RCC_CSR_BORRSTF) {  /*!< BOR reset flag */
        strcat(szSifReset, "/BOR");
      } 
    #endif
    
    if (pBootControl->rccCsr & RCC_CSR_SFTRSTF) {  /* sw reset flags */
      strcat(szSifReset, "/SW");
    } 

    strcat(szSifReset, "/" CLI_NL);
    CliWrite(szSifReset); 

      
    TRACE(TRC_TA_PLF, TRC_TL_COMPONENT, "SIF_Init()");
  }
 
  

}

/******************************************************************************/

