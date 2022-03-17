/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stdio.h>

#include "plf/plf.h"
#include "plf/trc/trc.h"
#include "plf/cli/cli.h"
#include "plf/blc/blc.h"
#include "plf/elog/elog.h"

#include "sif/sif.h"

#if (PLF_OS==PLF_OS_RTOS)
  #include "plf/rtos/rtos.h"
#endif

#include "net/rest/rest.h"

#include "hal/dflash/dflash.h"
#include "modem.h"
#include "modem_tcp.h"

/****************************************************************************/
#define HTTP_OK           (200)
#define TX_RX_BUFFER_SIZE (256)
#define FILE_NAME_LEN     (80)
typedef struct {
  uint8_t               dataBuf[TX_RX_BUFFER_SIZE];
  int32_t               bufIx;
  int32_t               totalData;
  uint32_t              fAddr;
  int32_t               fSize;
  RestGetAsyncCB_t       cbFunc;
  PlfTime_t             startTime;
  char                  fileName[FILE_NAME_LEN];

  bool                  isHeader;
  char                  xbuffer[256]; 
  int                   maxLen;
}                       RestGetStruct_t;

static RestGetStruct_t  restGetStruct;

static const char head[] = 
  "GET /download?file=%s&key=" HTTP_KEY " HTTP/1.1\r\n" 
  "Host: " HTTP_HOST ":" _STRIZE(GET_POST_HTTP_PORT) "\r\n"
  "\r\n";


static void _RestGetCharFlush(void)
{
  if (restGetStruct.bufIx > 0) {
    if (!restGetStruct.isHeader) {
      if (HalDFlashWrite(restGetStruct.fAddr, restGetStruct.dataBuf, restGetStruct.bufIx, false) != DFLASH_OK) {        
        static char sz[64];
        snprintf(sz, sizeof(sz), "Could not write DFlash addr: 0x%08X", restGetStruct.fAddr);
        
        TRACE_VA(TRC_TA_CIF, TRC_TL_ERROR, "%s", sz);
        EVENT_LOG_ADD_S(sz);
      }
    }
  }
}

static bool _RestGetChar(const uint8_t ch)
{
  if (restGetStruct.isHeader) {
    if (ch == '\n') {
      if (restGetStruct.bufIx > 0) {
        int dataLength;
        int httpResponse;
        
        restGetStruct.dataBuf[restGetStruct.bufIx] = 0;
        
        if (sscanf((char *)restGetStruct.dataBuf, "HTTP/1.1 %d ", &httpResponse) == 1) {
          if (httpResponse != HTTP_OK) {
            return false;
          }            
        } else if (sscanf((char *)restGetStruct.dataBuf, "Content-Length: %d", &dataLength) == 1) {
          if (dataLength > restGetStruct.fSize) {
            return false;            
          } else {
            restGetStruct.fSize = dataLength;
          }
        }
        
        // Reset line buffer
        restGetStruct.bufIx = 0;
      } else {
        
        // All HTTP header has been read. The next content is data/body
        restGetStruct.isHeader = false;
        restGetStruct.totalData = 0;
        restGetStruct.bufIx = 0;
      }
    } else if (ch == '\r') {
      // Ignore
    } else {
      
      // Load header parameter into buffer
      restGetStruct.dataBuf[restGetStruct.bufIx] = ch;
      restGetStruct.bufIx++;
      
      if (restGetStruct.bufIx >= TX_RX_BUFFER_SIZE)
        restGetStruct.bufIx--;
    }
  } else { 
    restGetStruct.totalData++;
    restGetStruct.dataBuf[restGetStruct.bufIx] = ch;  
    restGetStruct.bufIx++;
    
    if (restGetStruct.bufIx >= TX_RX_BUFFER_SIZE) {      
      if (HalDFlashWrite(restGetStruct.fAddr, restGetStruct.dataBuf, TX_RX_BUFFER_SIZE, true) != DFLASH_OK) {
        static char sz[64];
        snprintf(sz, sizeof(sz), "Could not write DFlash addr: 0x%08X", restGetStruct.fAddr);
        
        TRACE_VA(TRC_TA_CIF, TRC_TL_ERROR, "%s", sz);
        EVENT_LOG_ADD_S(sz);
        return false;
      }
      restGetStruct.fAddr += TX_RX_BUFFER_SIZE;
      restGetStruct.fSize -= TX_RX_BUFFER_SIZE;
      restGetStruct.bufIx = 0;
    } 
  }
  return true;
}

/****************************************************************************/

static void RestGetAsyncCleanUp(void)
{
  xModemTcpContinue = NULL;
  restGetStruct.cbFunc = NULL;
}

static void RestGetAsyncTcpContinue(const int step, const uint8_t * data, const int dataLen)
{
  TRACE_VA(TRC_TA_CIF, TRC_TL_4, "RestGetAsyncTcpContinue(%d)", step);
  switch (step) {
    
    // Header download
    case 0:
      restGetStruct.isHeader = true;
      sprintf(restGetStruct.xbuffer, head, restGetStruct.fileName);
      int xlen = strlen(restGetStruct.xbuffer);
      (void)ModemTcpSendData((uint8_t *)restGetStruct.xbuffer, xlen);
      break;
    
    // Data/body download
    case 1:
      for (int i=0; i<dataLen; i++) {
        if (!_RestGetChar(*data++)) {
          (void)ModemTcpClose();
          return;
        }
      };
      break;
      
    // Connection close and flush
    default:
        _RestGetCharFlush();
        (void)ModemTcpClose();
        if (restGetStruct.cbFunc)
          restGetStruct.cbFunc(restGetStruct.totalData);
        RestGetAsyncCleanUp();
      break;
  }
}

int RestGet(const char * fileName, uint32_t flashAddr, int32_t flashSizeMax, RestGetAsyncCB_t cb)
{
  TRACE_VA(TRC_TA_CIF, TRC_TL_4, "RestGet(\"%s\", 0x%08X, 0x%X)", fileName, flashAddr, flashSizeMax);

  memset(&restGetStruct, 0, sizeof(RestGetStruct_t));
  PlfTimeMsGet(&restGetStruct.startTime);

  if (DFLASH_OK != HalDFlashEraseAddrRange(flashAddr, flashAddr+flashSizeMax-1)) {
    TRACE(TRC_TA_CIF, TRC_TL_ERROR, "Could not erase Data Flash Region");
    EVENT_LOG_ADD_S("Could not erase Data Flash Region");
    RestGetAsyncCleanUp();
    return R_ERR_FLASH_ERASE;
  }
  xModemTcpContinue = RestGetAsyncTcpContinue;
  restGetStruct.cbFunc = cb;

  restGetStruct.fAddr = flashAddr;
  restGetStruct.fSize = flashSizeMax;

  strncpy(restGetStruct.fileName, fileName, sizeof(restGetStruct.fileName));
  restGetStruct.maxLen = flashSizeMax;
  
  TRACE_VA(TRC_TA_CIF, TRC_TL_4, "Connecting to " HTTP_HOST ":%d", GET_POST_HTTP_PORT);
  if (!ModemTcpOpen(HTTP_HOST, GET_POST_HTTP_PORT)) {
    RestGetAsyncCleanUp();
    return -1;
  }
  
  return 0;
}


/****************************************************************************/

#ifdef CLI_ENABLE 

static void RestGetAsyncCB(int len)
{
  PlfTime_t slutTime;
  char xx[40];
  PlfTimeMsGet(&slutTime);
  CliPrintf("RestGetAsync: %d byte, time:%s\n", len, PlfTime_DHMSM_Str(xx, slutTime - restGetStruct.startTime));
  CliCompleted(len == 0 ? CLI_RESULT_ERROR_UNDEFINED : CLI_RESULT_OK);  
}

int_fast16_t TestHGet(CliParam_t name, CliParam_t addr, CliParam_t maxLen)
{  
  if (addr + maxLen > DFLASH_SIZE-1) { 
    CliPrintf("Data Flash Range[0 ... 0x%08X]\n", DFLASH_SIZE-1);
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  int len = RestGet((const char *)name, addr, maxLen, RestGetAsyncCB);
  if (len < 0) {
    CliPrintf("Get failed: %d\n", len);
    return CLI_RESULT_ERROR_UNDEFINED;
  }

  return CLI_RESULT_EXECUTING;
}

#endif

/****************************************************************************/
