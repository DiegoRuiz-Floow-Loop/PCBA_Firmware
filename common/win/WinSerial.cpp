/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <process.h>
#include <stdio.h>

#include "win/WinSerial.h"
#include "win/widechar.h"


typedef unsigned(WINAPI * PBEGINTHREADEX_THREADFUNC)(LPVOID lpThreadParameter);
typedef unsigned *PBEGINTHREADEX_THREADID;

/* ---------------------------------------------------------------------- */
/* This function is not part of the TWinSerial object. It is simply used
 to start the thread from an external point of the object. */
//void WinSerialEventThreadStart(void *arg)
unsigned int __stdcall WinSerialEventThreadStart(void *arg)
{
  class WinSerial *serialUnit;
  serialUnit = (WinSerial*)arg;
  if (serialUnit != 0)
    serialUnit->Run();
  return 0;
}

/* -------------------------------------------------------------------- */
#define SIG_POWER_DOWN     0
#define SIG_READER         1
#define SIG_READ_DONE      2    // data received has been read
#define SIG_WRITER         3
#define SIG_DATA_TO_TX     4    // data waiting to be sent
#define SIG_MODEM_EVENTS   5
#define SIG_MODEM_CHECKED  6

WinSerial::WinSerial() {
  int i;
  ready = false;
  parityMode = WIN_SERIAL_PARITY_NONE;
  port = 0;
  rate = 0;
  threadid = 0;
  serialHandle = INVALID_HANDLE_VALUE;
  threadHandle = 0;
  owner = 0;
  txInProgress = 0;
  rxInProgress = 0;
  maxRxSize = 1;
  txSize = 0;
  receivedSize = 0;
  checkModem = false;
  serialCallBack = NULL;

  // creating Events for the different sources
  for (i = 0; i < WIN_SERIAL_SIGNAL_NBR; i++) {
    if ((i == SIG_READER) || (i == SIG_WRITER) || (i == SIG_MODEM_EVENTS))
      serialEvents[i] = CreateEvent(NULL, TRUE, FALSE, NULL); // Manual Reset
    else
      serialEvents[i] = CreateEvent(NULL, FALSE, FALSE, NULL); // Auto reset
  }
}

/* -------------------------------------------------------------------- */
WinSerial::~WinSerial() {
  int i;
  if (threadHandle != 0)
    WaitForSingleObject((HANDLE)threadHandle, 2000);
  threadHandle = 0;
  for (i = 0; i < WIN_SERIAL_SIGNAL_NBR; i++) { // deleting the events
    if (serialEvents[i] != INVALID_HANDLE_VALUE)
      CloseHandle(serialEvents[i]);
    serialEvents[i] = INVALID_HANDLE_VALUE;
  }
  if (serialHandle != INVALID_HANDLE_VALUE)
    CloseHandle(serialHandle);
  serialHandle = INVALID_HANDLE_VALUE;
}

/* -------------------------------------------------------------------- */
void WinSerial::Disconnect(void) {
  ready = false;
  SetEvent(serialEvents[SIG_POWER_DOWN]);
  if (threadHandle != 0)
    WaitForSingleObject((HANDLE)threadHandle, 2000);
  threadHandle = 0;
}


void WinSerial::LineStatusGet(LineStatus_t & lineStatus)
{
  DCB dcb;
  GetCommState(serialHandle,&dcb);
}
void WinSerial::LineStatusSet(LineStatus_t lineStatus)
{
  DCB dcb;
  GetCommState(serialHandle,&dcb);
  dcb.fDtrControl   = lineStatus.fDtrControl;
  dcb.fRtsControl   = lineStatus.fRtsControl;
  SetCommState(serialHandle,&dcb);
}

/* -------------------------------------------------------------------- */
/* Serial port, file and overlapped structures initialization */
int8_t WinSerial::Connect(
	uint8_t portArg,
	uint32_t rateArg,
	int32_t parityArg,
	uint8_t byteSize,
	bool modemEvents)

{
  int erreur;
  DCB dcb;
  int i;
  //bool hardHandshake;

  COMMTIMEOUTS cto = {0, 0, 0, 0, 0};

  /* --------------------------------------------- */
  if (serialHandle != INVALID_HANDLE_VALUE)
    CloseHandle(serialHandle);
  serialHandle = INVALID_HANDLE_VALUE;

  if (portArg > 0) {
    port = portArg;
    rate = rateArg;
    parityMode = parityArg;
    checkModem = modemEvents;
    erreur = 0;
    ZeroMemory(&ovReader, sizeof(ovReader)); // clearing the overlapped
    ZeroMemory(&ovWriter, sizeof(ovWriter));
    ZeroMemory(&ovWaitEvent, sizeof(ovWaitEvent));

    /* -------------------------------------------------------------------- */
    char sz[1024];
    sprintf(sz, "\\\\.\\COM%d", port);
    serialHandle = ::CreateFile(
      sz,
      GENERIC_READ | GENERIC_WRITE,
      0,
      NULL,
      OPEN_EXISTING,
      FILE_FLAG_OVERLAPPED,
      0);


    /* -------------------------------------------------------------------- */
    memset(&dcb, 0, sizeof(dcb));
    dcb.DCBlength = sizeof(dcb);
    dcb.BaudRate = rate;
    switch (parityMode) {
    case WIN_SERIAL_PARITY_NONE:
      dcb.Parity = NOPARITY;
      dcb.fParity = 0;
      break;
    case WIN_SERIAL_PARITY_EVEN:
      dcb.Parity = EVENPARITY;
      dcb.fParity = 1;
      break;
    case WIN_SERIAL_PARITY_ODD:
      dcb.Parity = ODDPARITY;
      dcb.fParity = 1;
      break;
    }
	dcb.StopBits = ONESTOPBIT;
    dcb.ByteSize = (BYTE) byteSize;
    dcb.fInX = FALSE;
	dcb.fOutX = FALSE;

#if 0
	hardHandshake = false;

	if (hardHandshake) {
	  dcb.fOutxDsrFlow = TRUE;
	  dcb.fOutxCtsFlow = TRUE;
	  dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
	  dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;
	}  else {
	  dcb.fOutxDsrFlow = FALSE;
	  dcb.fOutxCtsFlow = FALSE;
	  dcb.fRtsControl = RTS_CONTROL_ENABLE;
	  dcb.fDtrControl = DTR_CONTROL_ENABLE;
	}
#endif

	dcb.fOutxDsrFlow = FALSE;
	dcb.fOutxCtsFlow = FALSE;
	dcb.fDtrControl = DTR_CONTROL_ENABLE;
	dcb.fRtsControl = RTS_CONTROL_TOGGLE;  // TX ENABLE

    dcb.fErrorChar = 0;
    dcb.fBinary = 1;
    dcb.fNull = 0;
    dcb.fAbortOnError = 0;
    dcb.wReserved = 0;
    dcb.XonLim = 2;
    dcb.XoffLim = 4;
    dcb.XonChar = 0x13;
    dcb.XoffChar = 0x19;
    dcb.EvtChar = 0;

    ovReader.hEvent = serialEvents[SIG_READER];
    ovWriter.hEvent = serialEvents[SIG_WRITER];
    ovWaitEvent.hEvent = serialEvents[SIG_MODEM_EVENTS];

    if (serialHandle != INVALID_HANDLE_VALUE) {
      if (checkModem) {
        if (!SetCommMask(serialHandle, EV_RING | EV_RLSD))
          erreur = 1;
      }
      else {
        if (!SetCommMask(serialHandle, 0))
          erreur = 1;
      }
      if (!SetCommTimeouts(serialHandle, &cto))
        erreur = 2;

      if (!SetCommState(serialHandle, &dcb))
        erreur = 4;
    }
    else
      erreur = 8;
  }
  else
    erreur = 16;

  for (i = 0; i < WIN_SERIAL_SIGNAL_NBR; i++) {
    if (serialEvents[i] == INVALID_HANDLE_VALUE)
      erreur = 32;
  }

  if (erreur != 0) {
    CloseHandle(serialHandle);
    serialHandle = INVALID_HANDLE_VALUE;
  }
  else {
    SetupComm(serialHandle, 0x2000, 0x2000);
    // start thread
    threadHandle = _beginthreadex(
      NULL,
      0,
//      (PBEGINTHREADEX_THREADFUNC) WinSerialEventThreadStart,
	  WinSerialEventThreadStart,
      this,
      0,
      &threadid);
  }

  return (erreur);
}

/* -------------------------------------------------------------------- */
void WinSerial::SetManager(WinSerialCallBack_t manager_arg)
{
  serialCallBack = manager_arg;
}

/* -------------------------------------------------------------------- */
void WinSerial::SetRxSize(int16_t size) {
  maxRxSize = size;
  if (maxRxSize > WIN_SERIAL_MAX_RX)
    maxRxSize = WIN_SERIAL_MAX_RX;
}

/* -------------------------------------------------------------------- */
char * WinSerial::GetDataInBuffer(void) {
  return (char *)rxBuffer;
}

/* -------------------------------------------------------------------- */
int16_t WinSerial::GetDataInSize(void) {
  return receivedSize;
}

/* -------------------------------------------------------------------- */
void WinSerial::DataHasBeenRead(void) {
  SetEvent(serialEvents[SIG_READ_DONE]);
}

/* -------------------------------------------------------------------- */
int16_t WinSerial::GetNbrOfBytes(void) {
  struct _COMSTAT status;
  int n;
  unsigned long etat;
  n = 0;
  if (serialHandle != INVALID_HANDLE_VALUE) {
    ClearCommError(serialHandle, &etat, &status);
    n = status.cbInQue;
  }
  return (n);
}

/* -------------------------------------------------------------------- */
int WinSerial::SendData(void * buffer, int16_t size)
{
	if (!serialHandle) return -1;

 if (serialHandle == INVALID_HANDLE_VALUE) return -2;
 if (txInProgress) return -3;
 if (size >= WIN_SERIAL_MAX_TX) return -4;
 if (buffer == 0) return -5;

  txInProgress = 1;
  memcpy(txBuffer, buffer, size);
  txSize = size;

  return SetEvent(serialEvents[SIG_DATA_TO_TX]) ? 0 : -6;

 }

int WinSerial::SendText(
	char * txt)
{
	return SendData((void *)txt, strlen(txt));
}


/* -------------------------------------------------------------------- */
void WinSerial::OnEvent(uint32_t events)
{
  unsigned long ModemStat;
  GetCommModemStatus(serialHandle, &ModemStat);
  if ((events & EV_RING) != 0) {
    if ((ModemStat & MS_RING_ON) != 0) {
	  if (serialCallBack)
		serialCallBack((uint32_t) this, WIN_SERIAL_RING);
	}
  }
  if ((events & EV_RLSD) != 0) {
	if ((ModemStat & MS_RLSD_ON) != 0) {
	  if (serialCallBack)
		serialCallBack((uint32_t)this, WIN_SERIAL_CD_ON);
	}
	else {
	  if (serialCallBack)
		serialCallBack((uint32_t)this, WIN_SERIAL_CD_OFF);
    }
  }
}

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

// #define DEBUG_EVENTS

/* this function is the main loop of the Tserial object. There is a
 do while() loop executed until either an error or a PowerDown is
 received.
 this is not a busy wait since we use the WaitForMultipleObject function
 */
void WinSerial::Run(void)
{
  bool done;
  long status;
  unsigned long read_nbr, result_nbr;
  char success;

  ready = true;
  done = false;
  txInProgress = 0;
  rxInProgress = 0;
  WaitCommEventInProgress = 0;

  if (serialCallBack)
	serialCallBack((uint32_t) this, WIN_SERIAL_CONNECTED);

  GetLastError(); // just to clear any pending error
  SetEvent(serialEvents[SIG_READ_DONE]);
  if (checkModem)
    SetEvent(serialEvents[SIG_MODEM_CHECKED]);

  /* ----------------------------------------------------------- */
  while (!done) {
    /* ------------------------------------------------------------------ */
    /* Waiting  for signals */
    /* ------------------------------------------------------------------ */
    // Main wait function. Waiting for something to happen.
    // This may be either the completion of a Read or a Write or
    // the reception of modem events, Power Down, new Tx
    //
	status = WaitForMultipleObjects(WIN_SERIAL_SIGNAL_NBR, serialEvents, FALSE,
        INFINITE);

    // processing answer to filter other failures
    status = status - WAIT_OBJECT_0;
    if ((status < 0) || (status >= WIN_SERIAL_SIGNAL_NBR))
      done = true; // error

    else {
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      /* ++++++++++++++++++++ EVENT DISPATCHER ++++++++++++++++++ */
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      switch (status) {
      case SIG_POWER_DOWN:
        // receiving a POWER down signal. Stopping the thread
        done = true;
        break;
      case SIG_READ_DONE:
        // previous reading is finished I start a new one here
        if (!rxInProgress) {
          // locking reading
          rxInProgress = 1;
          // starting a new read
          success = (char) ReadFile(serialHandle, &rxBuffer, maxRxSize,
              &read_nbr, &ovReader);
          if (!success) {
            // failure
            uint16_t err = GetLastError();
            if (err != ERROR_IO_PENDING) {
              // real failure => quiting
              done = true;
#ifdef DEBUG_EVENTS
              printf("Readfile error (not pending)\n");
#endif
            }
#ifdef DEBUG_EVENTS
            else
              printf("ReadFile pending\n");
#endif
          }
#ifdef DEBUG_EVENTS
          else {
            // I make nothing here since the overlapped
            // will be signaled anyway, so I'll make
            // the processing there
            printf("ReadFile immediate success\n");
          }
#endif
        }
        break;
      case SIG_READER:
        // reading the result of the terminated read
        // BOOL GetOverlappedResult(
        // HANDLE hFile,	// handle of file, pipe, or communications device
        // LPOVERLAPPED lpOverlapped,	// address of overlapped structure
        // LPDWORD lpNumberOfBytesTransferred,	// address of actual bytes count
        // BOOL bWait 	// wait flag
        // );
        //
        if (GetOverlappedResult(serialHandle, &ovReader, &result_nbr, FALSE)) {
#ifdef DEBUG_EVENTS
          printf("ReadFile => GetOverlappedResult done\n");
#endif
          // no error => OK
          // Read operation completed successfully
          ResetEvent(serialEvents[SIG_READER]);
          // Write operation completed successfully
          receivedSize = result_nbr;
          rxInProgress = 0; // read has ended
          // if incoming data, I process them
		  if ((result_nbr != 0) && (serialCallBack))
			serialCallBack((uint32_t) this, WIN_SERIAL_DATA_ARRIVAL);
          // I automatically restart a new read once the
          // previous is completed.
          // SetEvent(serialEvents[SIG_READ_DONE]);
          // BUG CORRECTION 02.06.22
        }
        else {
          // GetOverlapped didn't succeed !
          // What's the reason ?
          if (GetLastError() != ERROR_IO_PENDING)
            done = 1; // failure
        }
        break;
        /* ######################################################## */
        /* #                                                      # */
        /* #                                                      # */
        /* #                       TX                             # */
        /* #                                                      # */
        /* #                                                      # */
        /* ######################################################## */
      case SIG_DATA_TO_TX:
        // Signal asserted that there is a new valid message
        // in the "txBuffer" variable
        // sending data to the port
        success = (char) WriteFile(serialHandle, txBuffer, txSize, &result_nbr,
            &ovWriter);
        if (!success) {
          // ouups, failure
          if (GetLastError() != ERROR_IO_PENDING) {
            // real failure => quiting
            done = true;
#ifdef DEBUG_EVENTS
            printf("WriteFile error (not pending)\n");
#endif
          }
#ifdef DEBUG_EVENTS
          else
            printf("WriteFile pending\n");
#endif
        }
#ifdef DEBUG_EVENTS
        else {
          // I make nothing here since the overlapped
          // will be signaled anyway, so I'll make
          // the processing there
          printf("WriteFile immediate success\n");
        }
#endif
        break;
        /* ######################################################## */
      case SIG_WRITER:
        // WriteFile has terminated
        // checking the result of the operation
        if (GetOverlappedResult(serialHandle, &ovWriter, &result_nbr, FALSE)) {
          // Write operation completed successfully
          ResetEvent(serialEvents[SIG_WRITER]);
          // further write are now allowed
          txInProgress = 0;
          // telling it to the manager
		  if (serialCallBack)
			serialCallBack((uint32_t) this, WIN_SERIAL_DATA_SENT);
        }
        else {
          // GetOverlapped didn't succeed !
          // What's the reason ?
          if (GetLastError() != ERROR_IO_PENDING)
            done = 1; // failure
        }
        break;
        /* ######################################################## */
        /* #                                                      # */
        /* #                                                      # */
        /* #                    MODEM_EVENTS EVENTS                      # */
        /* #                                                      # */
        /* #                                                      # */
        /* ######################################################## */
      case SIG_MODEM_CHECKED:
        if ((!WaitCommEventInProgress) && checkModem)
            // if no wait is in progress I start a new one
        {
          WaitCommEventInProgress = 1;
          success = (char) WaitCommEvent(serialHandle, &dwCommEvent,
              &ovWaitEvent);
          // reading one byte only to have immediate answer on each byte
          if (!success) {
            // ouups, failure
            if (GetLastError() != ERROR_IO_PENDING) {
              // real failure => quiting
              done = true;
#ifdef DEBUG_EVENTS
              printf("WaitCommEvent error (not pending)\n");
#endif
            }
#ifdef DEBUG_EVENTS
            else
              printf("WaitCommEvent pending\n");
#endif
          }
#ifdef DEBUG_EVENTS
          else {
            // I make nothing here since the overlapped
            // will be signaled anyway, so I'll make
            // the processing there
            printf("WaitCommEvent immediate success\n");
          }
#endif
        }
        break;
        /* ######################################################## */
      case SIG_MODEM_EVENTS:
        // reading the result of the terminated wait
        if (GetOverlappedResult(serialHandle, &ovWaitEvent, &result_nbr, FALSE))
        {
          // Wait operation completed successfully
          ResetEvent(serialEvents[SIG_MODEM_EVENTS]);
          WaitCommEventInProgress = 0;
          // if incoming data, I process them
          OnEvent(dwCommEvent);
          // automatically starting a new check
          SetEvent(serialEvents[SIG_MODEM_CHECKED]);
        }
        else {
          // GetOverlapped didn't succeed !
          // What's the reason ?
          if (GetLastError() != ERROR_IO_PENDING)
            done = 1; // failure
        }
        break;
      }
    }
  }

  // --------------------- Disconnecting ----------------
  ready = false;
  if (serialHandle != INVALID_HANDLE_VALUE)
    CloseHandle(serialHandle);
  serialHandle = INVALID_HANDLE_VALUE;

  if (serialCallBack)
    serialCallBack((uint32_t) this, WIN_SERIAL_DISCONNECTED);
}

/* -------------------------------------------------------------------- */
int8_t GetComStatus(WinSerialComAvailableArray_t list)
{
  int com;
  int8_t first = -1;
  for (com = 0; com < WIN_SERIAL_MAX_COM_ITEMS; com++) {
	char sz[1024];
	sprintf(sz, "\\\\.\\COM%d", com + 1);
	// Try to open the port
	HANDLE hPort = ::CreateFile(sz, GENERIC_READ | GENERIC_WRITE, 0, 0,
		OPEN_EXISTING, 0, 0);

	if (hPort == INVALID_HANDLE_VALUE) {
	  DWORD dwError = GetLastError();
	  // Check to see if the error was because some other app had the port open
	  if (dwError == ERROR_ACCESS_DENIED) {
		list[com] = WIN_SERIAL_IN_USE;
	  }
	  else {
		list[com] = WIN_SERIAL_NOT_AVAILABLE;
	  }
	}
	else {
	  list[com] = WIN_SERIAL_FREE;
	  if (first < 0) {
		first = com;
	  }
	  CloseHandle(hPort);
	}
  }
  return first;
}
