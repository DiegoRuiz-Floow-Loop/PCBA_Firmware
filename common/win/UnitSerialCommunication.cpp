/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "UnitMain.h"

#include <vcl.h>

#include "UnitSerialCommunication.h"
#include "TextIniFile.h"

#pragma hdrstop

//#include "wchar.h"



//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"


TFormSerialCommunication *FormSerialCommunication;

static UnicodeString IniFileSection = u"Form Com";

int						newComPort;
int						comPort = -1;
WinSerial 				* winSerial = NULL;


//---------------------------------------------------------------------------
__fastcall TFormSerialCommunication::TFormSerialCommunication(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSerialCommunication::FormCreate(TObject *Sender)
{
	Left = GetTextIniFileInteger(iniFile, IniFileSection, "Left", Left);
	Top = GetTextIniFileInteger(iniFile, IniFileSection, "Top", Top);
	Width = GetTextIniFileInteger(iniFile, IniFileSection, "Width", Width);
	Height = GetTextIniFileInteger(iniFile, IniFileSection, "Height", Height);
}
//---------------------------------------------------------------------------
void __fastcall TFormSerialCommunication::FormDestroy(TObject *Sender)
{
	SetTextIniFileInteger(iniFile, IniFileSection, "Left", Left);
	SetTextIniFileInteger(iniFile, IniFileSection, "Top", Top);
	SetTextIniFileInteger(iniFile, IniFileSection, "Width", Width);
	SetTextIniFileInteger(iniFile, IniFileSection, "Height", Height);
}

//---------------------------------------------------------------------------


static SerialDataCallBack_t serialCallBack;

static void SerialEventManager(uint32_t object, uint32_t event)
{

	int i;
	char * inBuffer;
	int inSize;
	WinSerial *com;
	char sz[80];

	com = (WinSerial*)object;
	if (com != NULL) {
		switch (event) {
		case WIN_SERIAL_DATA_ARRIVAL:
      //MAIN_LOG("WIN_SERIAL_DATA_ARRIVAL");
			inSize = com->GetDataInSize();
			inBuffer = com->GetDataInBuffer();
			for (i = 0; i < inSize; i++) {
        serialCallBack(*inBuffer++);
			}
			com->DataHasBeenRead();
			break;

		case WIN_SERIAL_CONNECTED:
      //MAIN_LOG("WIN_SERIAL_CONNECTED");
			break;
		case WIN_SERIAL_DISCONNECTED:
      //MAIN_LOG("WIN_SERIAL_DISCONNECTED");
			break;
		case WIN_SERIAL_DATA_SENT: // OnDataSent();
      //MAIN_LOG("WIN_SERIAL_DATA_SENT");
			break;
		case WIN_SERIAL_RING:
      //MAIN_LOG("WIN_SERIAL_RING");
			break;
		case WIN_SERIAL_CD_ON:
      //MAIN_LOG("WIN_SERIAL_CD_ON");
			break;
		case WIN_SERIAL_CD_OFF:
      //MAIN_LOG("WIN_SERIAL_CD_OFF");
			break;
		default:
      //MAIN_LOG("WIN default");
			break;
		}
	}
}

int OpenPort(SerialDataCallBack_t serialDataCallBack, uint32_t baudRate)
{
  serialCallBack = serialDataCallBack;
	comPort = GetTextIniFileInteger(iniFile, "COM", "com port", 1);
	winSerial = new WinSerial;
	winSerial->SetManager(SerialEventManager);
	return winSerial->Connect(comPort, baudRate, WIN_SERIAL_PARITY_NONE,   8, false);
}

void ClosePort(void)
{
  if (comPort > -1) {
	  winSerial->Disconnect();
	delete winSerial;
	winSerial = NULL;
	comPort =-1;
  }
}

//---------------------------------------------------------------------------

void TFormSerialCommunication::SerialPortsUpdate(void)
{
  int com;
  int comNum = 0;
  bool used;
  (void)GetComStatus(allComPorts);
  ListBoxSerial->Items->Clear();

  for (com = 0; com < WIN_SERIAL_MAX_COM_ITEMS; com++) {
	// Add the port number to the array which will be returned
	switch (allComPorts[com]) {
	  case WIN_SERIAL_FREE:
		ListBoxSerial->Items->Add((AnsiString)"COM" + IntToStr(com + 1) + " (free)");
		comNumArr[comNum++]  = com;
        break;
      case WIN_SERIAL_IN_USE:
        ListBoxSerial->Items->Add((AnsiString)"COM" + IntToStr(com + 1) + " (in use)");
		comNumArr[comNum++]  = com;
        break;
      case WIN_SERIAL_NOT_AVAILABLE:
        //ListBoxSerial->Items->Add((AnsiString)"COM" + IntToStr(com + 1) + " (absent)");
        break;
    }
  }

}
//---------------------------------------------------------------------------

void __fastcall TFormSerialCommunication::ListBoxSerialDblClick(TObject *Sender)
{
  int com = comNumArr[ListBoxSerial->ItemIndex];
	if (allComPorts[com] == WIN_SERIAL_FREE) {
		newComPort  = com + 1;
		Close();
	} else if (allComPorts[com] ==   WIN_SERIAL_IN_USE) {
		LOG_INFO("Com Port Alleady In Use");
	} else {
		LOG_INFO("Com Port not available");
	}
	SerialPortsUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TFormSerialCommunication::Button1Click(TObject *Sender)
{
	ClosePort();
	SerialPortsUpdate();
}
//---------------------------------------------------------------------------


