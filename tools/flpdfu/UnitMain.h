//---------------------------------------------------------------------------

#ifndef UnitMainH
#define UnitMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <stdio.h>

#include <Vcl.Dialogs.hpp>

//---------------------------------------------------------------------------

#define DFU_FILE_MAX_SIZE    (1*1024*1024)
typedef enum {
  DFT_NOT_EXSISTING,
  DFT_UNKNOWN,
  DFT_NORMAL,
  DFT_RLE
}               DfuFileType_t;

class TFormMain : public TForm
{
__published:	// IDE-managed Components
  TLabeledEdit *LE_File;
  TLabeledEdit *LE_Devices;
  TButton *B_File;
  TButton *Button1;
  TButton *Button2;
  TMemo *MemoMain;
  TFileOpenDialog *FileOpenDialog1;

  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall B_FileClick(TObject *Sender);
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
	WSADATA              	wsaData;
  char                  data[DFU_FILE_MAX_SIZE];
  void DFU_FileReadAndCheck(char * fileName, DfuFileType_t & dft, int & len, bool showInfo);

public:		// User declarations
  UnicodeString homeDir;
  __fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
