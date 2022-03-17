/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/plf.h"
#include "plf/evos/evos.h"
#include "plf/sio/sio.h"
#include "plf/fifo/fifo.h"
#include "hal/uart/uart.h"
#include "plf/cfg/nvm_cfg.h"

#include "bapp/bapp.h"

/******************************************************************************/

#define TX_BUF_SIZE (1024)
typedef FifoT(TX_BUF_SIZE)    TxBuf_t;
static  TxBuf_t               txBuf;

#define RX_BUF_SIZE (16)
typedef FifoT(RX_BUF_SIZE)    RxBuf_t;
static  RxBuf_t               rxBuf;

static EvosEventHandle_t sioTask;

/******************************************************************************/

static void UartRxCbFunc(uint8_t ch)
{
	FifoPut(&rxBuf, ch);
  EvosEventSetNow(sioTask, 0);
}

static bool UartTxCbFunc(uint8_t * ch) 
{
	return FifoGet(&txBuf, ch);
}

static char cmd[80];
static uint_fast8_t cmdIx = 0;
static void SioTask(EvosEventParam_t p)
{
	uint8_t ch;
  while (FifoGet(&rxBuf, &ch)) {
    if ((ch == '\n') || (ch == '\r')) {
      if (cmdIx > 0) {
        cmd[cmdIx] = 0;
        BAppGotSioCommand(cmd);
        cmdIx = 0;
      }
    } else if (ch >= ' ') {
      if (cmdIx < sizeof(cmd)) {
        cmd[cmdIx++] = ch;
      }
    }      
  }			
}

/******************************************************************************/

bool SioTxBusy(void)
{
  return (!HalUartTxEmpty(HAL_UART_SIF));
}


int_fast16_t SioGetChar(void)
{
  uint8_t ui8;
  if (FifoGet(&rxBuf, &ui8))
    return ui8;
  else
    return -1;
}

static void CheckNotEmpty(void)
{
  if (HalUartTxEmpty(HAL_UART_SIF)) {
    uint8_t ch;
    if (FifoGet(&txBuf, (uint8_t *)&ch)) {
      HalUartTxFirst(HAL_UART_SIF, ch); 
    } 
  }
}

void SifWriteCh(char ch)
{
  uint32_t cnt = 0;
  while (!FifoPut(&txBuf, ch)) {
    CheckNotEmpty();
    if (++cnt > 100000) break;
  }
  CheckNotEmpty();
  
}

void SioPrintLn(void)
{
  SifWrite("\r\n");
}

void SifWrite(const char * BufferPtr)
{
	while (*BufferPtr) {
		SifWriteCh(*BufferPtr++);
	}
}

void SifWriteLn(const char * BufferPtr)
{
  SifWrite(BufferPtr);
  SioPrintLn();
}

void SioInit(void)
{
  sioTask = EvosEventRegister(SioTask, "sio");
  FIFO_INIT(&txBuf, TX_BUF_SIZE);  
  FIFO_INIT(&rxBuf, RX_BUF_SIZE);
  HalUartInit( HAL_UART_SIF, UartRxCbFunc, UartTxCbFunc, MCUPIN_NA);
  EvosEventSet(sioTask, 5000, 0);
//  SifWriteLn("\r\nUART RUNNING...");
}


