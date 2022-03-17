/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <vcl.h>
#include <Printers.hpp>
#pragma hdrstop

#include "UnitText.h"
#include "UnitMainMon.h"
#include "TextIniFile.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TFormText *FormText;
static UnicodeString registrySection;

static UnicodeString IniFileSection = u"Text Edit";

//---------------------------------------------------------------------------
__fastcall TFormText::TFormText(TComponent* Owner)
  : TForm(Owner)
{
}

void  TFormText::TextPrint(UnicodeString text)
{
  Memo->Lines->Clear();
  Memo->Lines->LoadFromFile(text);
  fileName = text;
  Label1->Caption = "File: \""+text+"\"";
  Label2->Caption = (UnicodeString)"Lines: " + Memo->Lines->Count;

  MemoPrint(Memo, fileName);
}

bool TFormText::TextLoad(UnicodeString text, bool showModal,bool saveEnabled)
{
  Memo->Lines->Clear();
  Memo->Lines->LoadFromFile(text);
  fileName = text;
  Label1->Caption = "File: \""+text+"\"";
  Label2->Caption = (UnicodeString)"Lines: " + Memo->Lines->Count;
  saved = false;
  Hide();
  BSaveClose->Enabled = saveEnabled;
  if (showModal) {
    ShowModal();
  } else {
    Show();
  }
  return saved;
}

void TFormText::MemoPrint(TMemo *Memo, UnicodeString name)
{
  if (PrintDialog1->Execute()) {
    int lines = Memo->Lines->Count;
    if (lines == 0) {
      Memo->Lines->Append("empty");
    }
    int textHeight = Printer()->Canvas->TextHeight(Memo->Lines->Strings[0])+1;
    int pageHeight = Printer()->PageHeight;
    int line = 0;
    int tab = Printer()->PageWidth / 100;
    int margin = tab*6;
    int linesPrPage = (pageHeight - (2*margin)) / (textHeight+1); //78;
    int pages = (lines + linesPrPage - 1)/linesPrPage;
    TRect r;

//    if (FormMain->MI_PrintBoarder->Checked) {
      r = Rect(
        margin,
        margin,
        Printer()->PageWidth - margin,
        Printer()->PageHeight - margin);
//    } else {
//      tab = 0;
//    }

    Printer()->Title = "MT100 print";
    Printer()->BeginDoc();

    for (int i = 0; i <= pages; i++) {
	    Printer()->Canvas->TextOut(
        margin,
	  		margin - ((textHeight * 3 ) / 2),
        (UnicodeString)"XXX | \"" + name + "\" | Page: " + Printer()->PageNumber + " | "+Now());
//      if (FormMain->MI_PrintBoarder->Checked) {
        TColor oc = Printer()->Canvas->Brush->Color;
        Printer()->Canvas->Brush->Color = clBlack;
        Printer()->Canvas->FrameRect(r);
        Printer()->Canvas->Brush->Color = oc;
//      }
      for (int l=0 ; l < linesPrPage; l++) {
        if (lines-- <= 0) {
          break;
        }
    		Printer()->Canvas->TextOut(
          margin + tab,
  	  		margin + (l * textHeight) + (textHeight / 2),
    			Memo->Lines->Strings[line++]);
      }
      if (lines > 0) {
        Printer()->NewPage();
      }
    }
    Printer()->EndDoc();

  }

}

//---------------------------------------------------------------------------
void __fastcall TFormText::FormCreate(TObject *Sender)
{
	Memo->Lines->Clear();
	registrySection = (UnicodeString)"\\TekPartner\\IoT Factory\\" + Application->Title + "\\Form_Text";

	Top 	 = GetTextIniFileInteger(iniFile, IniFileSection, "TOP", Top);
	Left   = GetTextIniFileInteger(iniFile, IniFileSection, "LEFT", Left);
	Width  = GetTextIniFileInteger(iniFile, IniFileSection, "WIDTH", Width);
	Height = GetTextIniFileInteger(iniFile, IniFileSection, "HEIGHT", Height);

	if (Top  > (Screen->Height - Height)) Top  = (Screen->Height - Height);
	if (Top < 0) Top = 0;
	if (Left > (Screen->Width  - Width)) Left = (Screen->Width  - Width);
	if (Left < 0) Left = 0;
}
//---------------------------------------------------------------------------

void __fastcall TFormText::FormDestroy(TObject *Sender)
{
	SetTextIniFileInteger(iniFile, IniFileSection, "TOP", Top);
	SetTextIniFileInteger(iniFile, IniFileSection, "LEFT", Left);
	SetTextIniFileInteger(iniFile, IniFileSection, "WIDTH", Width);
	SetTextIniFileInteger(iniFile, IniFileSection, "HEIGHT", Height);
}
//---------------------------------------------------------------------------


void __fastcall TFormText::MemoChange(TObject *Sender)
{
  Label2->Caption = (UnicodeString)"Lines: " + Memo->Lines->Count;
}
//---------------------------------------------------------------------------


void __fastcall TFormText::BPrintClick(TObject *Sender)
{
  MemoPrint(Memo, fileName);
}
//---------------------------------------------------------------------------

void __fastcall TFormText::BSaveAsClick(TObject *Sender)
{
  SaveTextFileDialog1->FileName = fileName;
  if (SaveTextFileDialog1->Execute()) {
    Memo->Lines->SaveToFile(SaveTextFileDialog1->FileName);
    saved = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TFormText::BSaveCloseClick(TObject *Sender)
{
//  SaveTextFileDialog1->FileName = fileName;
//  if (SaveTextFileDialog1->Execute()) {
  Memo->Lines->SaveToFile(fileName);
  saved = true;
  Close();
//  }
}
//---------------------------------------------------------------------------

void __fastcall TFormText::BAbortCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TFormText::MarkALl1Click(TObject *Sender)
{
  Memo->SelectAll();
}
//---------------------------------------------------------------------------


void __fastcall TFormText::FormResize(TObject *Sender)
{
  if (FormText->Width < 512) {
    FormText->Width =  512;
  }
  if (FormText->Height < 300) {
    FormText->Height =  300;
  }
}
//---------------------------------------------------------------------------


