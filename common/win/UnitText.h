/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef FTextH
#define FTextH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TFormText : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel2;
  TMemo *Memo;
  TLabel *Label1;
  TPrintDialog *PrintDialog1;
  TLabel *Label2;
  TSaveTextFileDialog *SaveTextFileDialog1;
  TPopupMenu *PopupMenu1;
  TMenuItem *MarkALl1;
  TMenuItem *SaveandCLose1;
  TMenuItem *SaveAs1;
  TMenuItem *ABortandClose1;
  TMenuItem *N1;
  TMenuItem *N2;
  TMenuItem *Print1;
  TPanel *Panel1;
  TSpeedButton *SpeedButton1;
  TSpeedButton *SpeedButton2;
  TSpeedButton *BSaveClose;
  TButton *Button1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall MemoChange(TObject *Sender);
  void __fastcall BPrintClick(TObject *Sender);
  void __fastcall BSaveAsClick(TObject *Sender);
  void __fastcall BSaveCloseClick(TObject *Sender);
  void __fastcall BAbortCloseClick(TObject *Sender);
  void __fastcall MarkALl1Click(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
private:	// User declarations
  UnicodeString   fileName;
  bool  saved;

public:		// User declarations
	bool TextLoad(UnicodeString text, bool showModal=true, bool saveEnabled=true);
  void TextPrint(UnicodeString text);

  void MemoPrint(TMemo *Memo, UnicodeString name);

  __fastcall TFormText(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormText *FormText;
//---------------------------------------------------------------------------
#endif
