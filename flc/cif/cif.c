/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

// CIF - Connectivity Interface

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>
#include <plf/evos/evos.h>
#include <plf/fifo/fifo.h>

#include <hal/uart/uart.h>

// net
#include <modem.h>

#include "cloud_flc.h"
#include "cif.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define RX_BUF_SIZE       (64u)
#define TX_BUF_SIZE       (512u)

#define PRINT_BUF_SIZE    (132u)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef FifoT(TX_BUF_SIZE) TxBuf_t;
typedef FifoT(RX_BUF_SIZE) RxBuf_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

/*******************************************************************************
 * Global data variables
 ******************************************************************************/

// FIX ME
// Added so the project can link without cloud.c and cloud_cmd.c present in the project
#include <modem.h>
XModemTcpContinue_t xModemTcpContinue = NULL;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static TxBuf_t gsmTxBuf;
static RxBuf_t gsmRxBuf;
static EvosEventHandle_t gsmRxHandle = EVOS_UNINITIALIZED_HANDLE;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// GSM UART
static void GsmRxEvent(EvosEventParam_t param);
static bool GsmTxCallback(uint8_t * ch);
static void GsmRxCallback(uint8_t ch);
static void GsmCheckTxEmpty(void);
static void GsmPutChar(uint_fast8_t ch);
static void GsmPutString(const char * sz);
static void GsmPutArray(const uint8_t * array, uint16_t length);

// Other
static void Log(TraceLevel_t tl, const char * info, va_list ap);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void CIF_Init(void)
{
  TRACE(TRC_TA_CIF, TRC_TL_COMPONENT, "CIF_Init()");
  
  gsmRxHandle = EvosEventRegister(GsmRxEvent, "cld rx");
  FIFO_INIT(&gsmRxBuf, RX_BUF_SIZE);
  FIFO_INIT(&gsmTxBuf, TX_BUF_SIZE);  
  HalUartInit(HAL_UART_GSM, GsmRxCallback, GsmTxCallback, MCUPIN_NA);     
  
  ModemInit();
  CloudFlcInit();
}

void XModemAtSendText(const char * const txt)
{
  GsmPutString(txt);
}

void XModemAtSendData(const uint8_t * const data, const uint16_t length)
{
  GsmPutArray(data, length);
}

void XModemLogFunc(const char * info, ...)
{
  va_list ap;
  va_start(ap, info);  
  Log(TRC_TL_3, info, ap);
  va_end(ap);  
}

bool XModemLogShowFunc(void)
{
  return true;
}

void XModemLogState(const char * const info, ...)
{
  va_list ap;
  va_start(ap, info);  
  Log(TRC_TL_4, info, ap);
  va_end(ap);
}

bool XModemLogShowState(void)
{
  return true;
}

void XModemLogTraffic(const char * const info, ...)
{
  va_list ap;
  va_start(ap, info);  
  Log(TRC_TL_5, info, ap);
  va_end(ap);
}

bool XModemLogShowAtTraffic(void)
{
  return true;
}

bool XModemLogShowDataTraffic(void)
{
  return true;
}

/*******************************************************************************
 * Local functions, gsm uart
 ******************************************************************************/

static void GsmRxEvent(const EvosEventParam_t param)
{
  uint8_t ch;
  while (FifoGet(&gsmRxBuf, &ch)) {
    ModemAtDataCallback(ch);
  }
}

static bool GsmTxCallback(uint8_t * ch)
{
  return FifoGet(&gsmTxBuf, ch);
}

static void GsmRxCallback(uint8_t ch)
{
  FifoPut(&gsmRxBuf, ch);
  EvosEventSetNow(gsmRxHandle, 0);
}

static void GsmCheckTxEmpty(void)
{
  if (HalUartTxEmpty(HAL_UART_GSM)) {
    uint8_t xx;
    if (FifoGet(&gsmTxBuf, &xx)) {
      HalUartTxFirst(HAL_UART_GSM, xx);
    }
  }
}
static void GsmPutChar(uint_fast8_t ch)
{
  while (!FifoPut(&gsmTxBuf, ch)) {
    GsmCheckTxEmpty();  
  }
  GsmCheckTxEmpty();
}

static void GsmPutString(const char * sz)
{
  while (*sz) {
    GsmPutChar(*sz++);
  }
}

static void GsmPutArray(const uint8_t * const array, const uint16_t length)
{
  for (uint_fast16_t index = 0; index < length; index++) {
    GsmPutChar(array[index]);
  }
}

/*******************************************************************************
 * Local functions, other
 ******************************************************************************/

static void Log(const TraceLevel_t tl, const char * const info, va_list ap)
{
  static char buffer[PRINT_BUF_SIZE];
  vsnprintf(buffer, PRINT_BUF_SIZE, info, ap);
  
  TRACE_VA(TRC_TA_CIF, tl, buffer);
}

/******************************************************************************/
