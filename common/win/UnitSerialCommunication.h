/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef UnitSerialCommunicationH
#define UnitSerialCommunicationH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include <Vcl.ExtCtrls.hpp>

#include "WinSerial.h"


//---------------------------------------------------------------------------
class TFormSerialCommunication : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TListBox *ListBoxSerial;
  TLabel *Label1;
  TButton *Button1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ListBoxSerialDblClick(TObject *Sender);
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
	WinSerialComAvailableArray_t allComPorts;

public:		// User declarations
	__fastcall TFormSerialCommunication(TComponent* Owner);
  uint8_t   comNumArr[WIN_SERIAL_MAX_COM_ITEMS];
	void SerialPortsUpdate(void);


};
//---------------------------------------------------------------------------
extern PACKAGE TFormSerialCommunication *FormSerialCommunication;

extern int				      newComPort;
extern int				      comPort;
extern WinSerial 				* winSerial;

extern void SendCommand(char * msg);


typedef void (* SerialDataCallBack_t) (uint8_t ch);

extern int OpenPort(SerialDataCallBack_t serialDataCallBack, uint32_t baudRate);

extern void ClosePort(void);


//---------------------------------------------------------------------------

extern void GotCommand(char * msg); /* outside this file */


//---------------------------------------------------------------------------
#endif
