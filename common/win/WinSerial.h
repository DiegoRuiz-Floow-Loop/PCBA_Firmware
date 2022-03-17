/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef WIN_SERIAL_H
#define WIN_SERIAL_H

#ifdef __cplusplus
//extern "C" {
#endif /* __cplusplus */

/* ------------------------------------------------------------------------- */

#include <windows.h>
#include <Classes.hpp>
#include <stdint.h>

/* ------------------------------------------------------------------------- */

// Callback Event
#define WIN_SERIAL_CONNECTED         0
#define WIN_SERIAL_DISCONNECTED      1
#define WIN_SERIAL_DATA_SENT         2
#define WIN_SERIAL_DATA_ARRIVAL      3
#define WIN_SERIAL_RING              4
#define WIN_SERIAL_CD_ON             5
#define WIN_SERIAL_CD_OFF            6
#define WIN_SERIAL_SIGNAL_NBR        7         // number of events in the thread

// Callback Event Manager
typedef void (*WinSerialCallBack_t) (uint32_t object, uint32_t event);

// Parity
#define WIN_SERIAL_PARITY_NONE       0
#define WIN_SERIAL_PARITY_ODD        1
#define WIN_SERIAL_PARITY_EVEN       2

#define WIN_SERIAL_MAX_RX            2*1024       // Input buffer max size
#define WIN_SERIAL_MAX_TX            2*1024       // output buffer max size

#define WIN_SERIAL_MAX_COM_ITEMS     (128)
typedef enum {
  WIN_SERIAL_FREE,
  WIN_SERIAL_IN_USE,
  WIN_SERIAL_NOT_AVAILABLE
  }                                   WinSerialComAvailable_t;
typedef WinSerialComAvailable_t       WinSerialComAvailableArray_t[WIN_SERIAL_MAX_COM_ITEMS];

#if 0
fDtrControl:
  DTR_CONTROL_DISABLE   0x00 Disables the DTR line when the device is opened and leaves it disabled.
  DTR_CONTROL_ENABLE    0x01 Enables the DTR line when the device is opened and leaves it on.
  DTR_CONTROL_HANDSHAKE 0x02 Enables DTR handshaking. If handshaking is enabled, it is an error for the application to adjust the line by using the EscapeCommFunction function.

fRtsControl:
  RTS_CONTROL_DISABLE   0x00 Disables the RTS line when the device is opened and leaves it disabled.
  RTS_CONTROL_ENABLE    0x01 Enables the RTS line when the device is opened and leaves it on.
  RTS_CONTROL_HANDSHAKE 0x02 Enables RTS handshaking. The driver raises the RTS line when the "type-ahead" (input) buffer is less than one-half full and lowers the RTS line when the buffer is more than three-quarters full. If handshaking is enabled, it is an error for the application to adjust the line by using the EscapeCommFunction function.
  RTS_CONTROL_TOGGLE    0x03 Specifies that the RTS line will be high if bytes are available for transmission. After all buffered bytes have been sent, the RTS line will be low. Windows Me/98/95:  This value is not supported.
#endif

typedef struct {
  unsigned int          fDtrControl : 2;
  unsigned int          fRtsControl : 2;
}                       LineStatus_t;

/* ------------------------------------------------------------------------- */

class WinSerial {
protected:
  bool                  ready;
  bool                  checkModem;
  uint8_t               port;                   // port: 1=="com1",...
  int32_t               rate;                   // baudrate
  int32_t               parityMode;
  HANDLE                serialEvents[WIN_SERIAL_SIGNAL_NBR]; // events to wait on
  uint32_t              threadid;               // ...
  HANDLE                serialHandle;           // ...
  unsigned long         threadHandle;           // ...
  OVERLAPPED            ovReader;               // Overlapped structure for ReadFile
  OVERLAPPED            ovWriter;               // Overlapped structure for WriteFile
  OVERLAPPED            ovWaitEvent;            // Overlapped structure for WaitCommEvent
  bool                  rxInProgress;           // BOOL indicating if a ReadFile is in progress
  bool                  WaitCommEventInProgress;
  uint8_t               rxBuffer[WIN_SERIAL_MAX_RX];
  int16_t               maxRxSize;
  int16_t               receivedSize;
  uint8_t               txBuffer[WIN_SERIAL_MAX_TX];
  int16_t               txSize;
  DWORD                 dwCommEvent;            // to store the result of the wait
  WinSerialCallBack_t   serialCallBack;

  void OnCharArrival(char c);
  void OnEvent(uint32_t events);

public:
  void                  *owner;                 // do what you want with this

  bool                  txInProgress;           // BOOL indicating if a WriteFile is in progress

  WinSerial();
  ~WinSerial();
  void Run(void);

  int8_t Connect(
    uint8_t             port,   // 1=COM1, 2=COM2
    uint32_t            rate,
    int32_t             parity,
    uint8_t             byteSize,
    bool                modemEvents);

  void SetManager(
    WinSerialCallBack_t manager);

  void SetRxSize(
    int16_t             size);
  int SendData(
    void               *buffer,
		int16_t             size);
	int SendText(
		char * txt);

  int16_t GetNbrOfBytes(void);
  int16_t GetDataInSize(void);
  char *GetDataInBuffer(void);
  void DataHasBeenRead(void);
  void Disconnect(void);

  void LineStatusGet(LineStatus_t & lineStatus);
  void LineStatusSet(LineStatus_t lineStatus);

};

  // Return first available free port
extern int8_t GetComStatus(
	WinSerialComAvailableArray_t list);


/* ------------------------------------------------------------------------- */
#ifdef __cplusplus
//}
#endif /* _cplusplus */

#endif /* WIN_SERIAL_H */


