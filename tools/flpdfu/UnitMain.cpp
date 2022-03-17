//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.UITypes.hpp>
#include <System.IOUtils.hpp>
#include <dir.h>
#pragma hdrstop

#include "UnitMain.h"
#include "win/TextIniFile.h"
#include "win/widechar.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TFormMain *FormMain;

//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FormCreate(TObject *Sender)
{
  TIniFile    * iniFile;
  FormMain->Caption = "Flow Loop DFU Tools";

  MemoMain->Lines->Add((String)"READY");

	WSAStartup(0x0101, &wsaData);


  homeDir = System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetDocumentsPath(), _D("FlowLoop"));
  TDirectory::CreateDirectory(homeDir);

  OpenTextIniFile(&iniFile, homeDir+"//flpdfu.ini");

  //Width = GetTextIniFileInteger(iniFile, "FormMain", "Width", Width);
  //Height =   GetTextIniFileInteger(iniFile, "FormMain", "Height", Height);

  Top = GetTextIniFileInteger(iniFile, "FormMain", "Top", Top);
  Left = GetTextIniFileInteger(iniFile, "FormMain", "Left", Left);
  if (Top  > (Screen->Height - Height)) Top  = (Screen->Height - Height);
  if (Top < 0) Top = 0;
  if (Left > (Screen->Width  - Width)) Left = (Screen->Width  - Width);
  if (Left < 0) Left = 0;

  WindowState = (TWindowState)GetTextIniFileInteger(iniFile, "FormMain", "WindowState", (int)WindowState);

  LE_File->Text = GetTextIniFileString(iniFile, "App", "LE_File", LE_File->Text);

  CloseTextIniFile(iniFile);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::FormDestroy(TObject *Sender)
{
  TIniFile    * iniFile;
  OpenTextIniFile(&iniFile, homeDir+"//flpdfu.ini");

  SetTextIniFileString(iniFile, "App", "LE_File", LE_File->Text);

  SetTextIniFileInteger(iniFile, "FormMain", "WindowState", (int)WindowState);
  if (WindowState == wsNormal) {
    SetTextIniFileInteger(iniFile, "FormMain", "Top", Top);
    SetTextIniFileInteger(iniFile, "FormMain", "Left", Left);
    SetTextIniFileInteger(iniFile, "FormMain", "Width", Width);
    SetTextIniFileInteger(iniFile, "FormMain", "Height", Height);
  }

  CloseTextIniFile(iniFile);
	WSACleanup();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::B_FileClick(TObject *Sender)
{
  char fileName[1024];
  char drive[1024];
  char dir[1024];
  char name[1024];
  char ext[1024];

  wchar2char(fileName, LE_File->Text.c_str(), sizeof(fileName));
  fnsplit(fileName, drive, dir, name, ext);
  strcat(drive, dir);

  FileOpenDialog1->Title = "Choose DFU File";
  FileOpenDialog1->FileName = LE_File->Text;
  FileOpenDialog1->DefaultFolder = drive;
  FileOpenDialog1->DefaultExtension = "*.dfu";
	if (FileOpenDialog1->Execute()) {
	  LE_File->Text = FileOpenDialog1->FileName;
	}

  DfuFileType_t dft;
  int len;
  wchar2char(fileName, LE_File->Text.c_str(), sizeof(fileName));
  DFU_FileReadAndCheck(fileName, dft, len, true);
}
//---------------------------------------------------------------------------

#include "plf/blc/blc_def.h"

#define HTTP_HOST           "iot.flow-loop.com"
#define HTTP_KEY            "5U3UHHgBHtPyaY9h"
#define GET_POST_HTTP_PORT  3002


static const char _h1[] =
  "POST /upload HTTP/1.1\r\n"
  "Host: " HTTP_HOST ":" _STRIZE(GET_POST_HTTP_PORT) "\r\n"
  "User-Agent: TekPartner/1.0.0\r\n"
  "Accept: */*\r\n"
#if _WIN64
  "Content-Length: %llu\r\n"
#else
  "Content-Length: %u\r\n"
#endif
  "Content-Type: multipart/form-data; boundary=------------------------%016llX\r\n"
  "Expect: 100-continue\r\n"
  "\r\n";

static const char _h2[] =
  "--------------------------%016llX\r\n"
  "Content-Disposition: form-data; name=\"key\"\r\n"
  "\r\n"
  "" HTTP_KEY "\r\n"
  "--------------------------%016llX\r\n"
  "Content-Disposition: form-data; name=\"directory\"\r\n"
  "\r\n"
  "%s\r\n"
  "--------------------------%016llX\r\n"
  "Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n"
  "Content-Type: application/octet-stream\r\n"
  "\r\n";

static const char _f1[] =
  "\r\n"
  "--------------------------%016llX--\r\n"
  "";

static void DfuRestSendFile(const char * filePath, const char * fileName, int fileSize, uint8_t * fileData)
{
  int                 soc, len, ret, cix, d, l, ifd;
  struct sockaddr_in  hdSocketAddrIn;
  struct hostent *    ptrh;
  char h1[1024];
  char h2[1024];
  char f1[1024];
  char r[1024];
  char sz[256];
  uint64_t u64 = 0x100000000ULL * (uint64_t)_lrand() + _lrand();

  FormMain->MemoMain->Lines->Add((String)"DfuRestSendFile(" + fileName+", " + fileSize +", ...)");
  memset(&hdSocketAddrIn, 0, sizeof(hdSocketAddrIn));
  hdSocketAddrIn.sin_family = AF_INET;
  hdSocketAddrIn.sin_port = htons(GET_POST_HTTP_PORT);
  ptrh = gethostbyname(HTTP_HOST);
  if (ptrh) {
    memcpy(&hdSocketAddrIn.sin_addr, ptrh->h_addr, (size_t)ptrh->h_length);
  } else {
    return;
  }

  if ((soc = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    return;
  } else {
    if (0 != connect(soc, (struct sockaddr *)&hdSocketAddrIn, sizeof(hdSocketAddrIn))) {
      closesocket(soc);
      return;
    }
  }

  sprintf(f1, _f1, u64);
  sprintf(h2, _h2, u64, u64, filePath, u64, fileName);
  sprintf(h1, _h1, fileSize + strlen(h2) + strlen(f1), u64);

  // SEND HEADER 1
  //LOG_MAIN("--Header 1");
  //LOG_MAIN(h1);
  len = strlen(h1);
  ret = send(soc, h1, len, 0);
  if (ret != len) {
    //DoLog("Could not send");
    closesocket(soc);
    return;
  }

  // READ HEADER RESPONS
  cix = 0;
  d = 0;
  for (;;) {
    len = recv(soc, h1, sizeof(h1), 0);
    if (len <= 0) break;
    char * b = h1;
    do {
        if (*b == '\n') {
          if (cix > 0) {
            r[cix] = 0;
            //sprintf(sz, ">> %s", r);
            //LOG_MAIN(sz);
            cix = 0;
            if (sscanf(r, "HTTP/1.1 %d ", &d) == 1) {
              if (d != 100) {
                closesocket(soc);
                return;
              } else {
                break;
              }
            }
          } else {
            break;
          }
        } else if (*b == '\r') {
          // ignore
        } else {
          r[cix++] = *b;
          if (cix >= (int)sizeof(r))
            cix--;
        }
        b++;
    } while (len-- > 0);
    if (d==100) break;
   }

  // SEND HEADER 2
  //LOG_MAIN("--Header 2");
  //LOG_MAIN(h2);
  len = strlen(h2);
  ret = send(soc, h2, len, 0);
  if (ret != len) {
    //DoLog("Could not send");
    closesocket(soc);
    return;
  }

  // SEND FILE CONTENTS
  //LOG_MAIN("--Data");
  len = fileSize;
  ifd = 0;
  #define CHUNC_SIZE  (0x8000)
  for (;;) {
    int l;
    if (len<CHUNC_SIZE) {
      l = len;
    } else {
      l = CHUNC_SIZE;
    }
    ret = send(soc, (char *)&fileData[ifd], l, 0);
    if (ret != l) {
      FormMain->MemoMain->Lines->Add("Could not send");
      closesocket(soc);
      return;
    }
    len -= ret;
    ifd += ret;
    if (len <= 0)
      break;
  }

  // SEND FOTTER
  //LOG_MAIN("--Foter");
  //LOG_MAIN(f1);
  len = strlen(f1);
  ret = send(soc, f1, len, 0);
  if (ret != len) {
    FormMain->MemoMain->Lines->Add("Could not send");
    closesocket(soc);
    return;
  }

  // READ DATA RESPONS
  cix = 0;
  for (;;) {
    len = recv(soc, h1, sizeof(h1), 0);
    if (len <= 0)
      break;
    char * b = h1;
    do {
      if (*b == '\n') {
        if (cix > 0) {
          //r[cix] = 0;
          //sprintf(sz, ">> %s", r);
          //LOG_MAIN(sz);
          cix = 0;
        } else {
          goto do_exit;
        }
      } else if (*b == '\r') {
        // ignore
      } else {
        r[cix++] = *b;
        if (cix >= (int)sizeof(h2))
          cix--;
      }
      b++;
    } while (len-- > 0);
  }
  do_exit:
  closesocket(soc);
  FormMain->MemoMain->Lines->Add("Send OK");
}



void TFormMain::DFU_FileReadAndCheck(char * fileName, DfuFileType_t & dft, int & len, bool showInfo)
{
  uint32_t        dfuFileSize = DFU_FILE_MAX_SIZE;
  dft = DFT_NOT_EXSISTING;


  // Load File
  FILE * dfuFile = fopen(fileName, "rb");
  if (dfuFile == NULL) {
    if (showInfo)
      MemoMain->Lines->Add("File Not existing");
    return;
  }
  len = fread(data, 1, dfuFileSize, dfuFile);
  fclose(dfuFile);

  // DFU ???
  FWDescriptor_t * pFWDescriptor = (FWDescriptor_t *)&data[len-BLC_FW_DESCIPTOR_SIZE];
  Crc16_t crc = CALC_CRC16(len - BLC_FW_DESCIPTOR_SIZE, data, CRC16_INITIAL_SEED);
  if (pFWDescriptor->crc == crc) {
    dft = DFT_NORMAL;
    if (showInfo) {
      char sz[256];
      sprintf(sz, "DFU file id: \"%s\", ver: %u.%u.%c, crc: 0x%04X",
        pFWDescriptor->id,
        pFWDescriptor->swVerMajor,
        pFWDescriptor->swVerMinor,
        pFWDescriptor->swVerType,
        pFWDescriptor->crc);
      if (!pFWDescriptor->wdtEnable)
        strcat(sz, " - WDT not enabled !!");
      MemoMain->Lines->Add(sz);
    }
  } else {
    dft = DFT_UNKNOWN;
  }

}



void __fastcall TFormMain::Button1Click(TObject *Sender)
{
  uint32_t        dfuFileSize;
  char fileName[1024];
  DfuFileType_t dft;
  int len;

  MemoMain->Lines->Add((String)"Uploading: "+ LE_File->Text);

  wchar2char(fileName, LE_File->Text.c_str(), sizeof(fileName));
  DFU_FileReadAndCheck(fileName, dft, len, true);
  if (dft == DFT_NORMAL) {
    char drive[1024];
    char dir[1024];
    char name[1024];
    char ext[1024];
    fnsplit(fileName, drive, dir, name, ext);

    const FWDescriptor_t * pFWDescriptor = (const FWDescriptor_t *)&data[len-BLC_FW_DESCIPTOR_SIZE];
    snprintf(name, sizeof(name), "flc_v%u_%u.dfu",
                                 pFWDescriptor->swVerMajor,
                                 pFWDescriptor->swVerMinor);
    DfuRestSendFile("fw", name, len, (uint8_t *)data);
  } else if (dft == DFT_RLE) {
    MemoMain->Lines->Add((String)"DFU RLE not supported yet !!");
  } else { // no data
    MemoMain->Lines->Add((String)"DFU File Not Recognised !!");
  }

}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Button2Click(TObject *Sender)
{
  char serial[1024];
  char filename[1024];
  DfuFileType_t dft;
  int len;

  MemoMain->Lines->Add((String)"Making/uploading cmd/"  ".json");

  wchar2char(filename, LE_File->Text.c_str(), sizeof(filename));
  DFU_FileReadAndCheck(filename, dft, len, true);

  if (dft != DFT_NORMAL) {
    MemoMain->Lines->Add((String)"Loading .dfu file failed. Version unknown!");
    return;
  }

  const FWDescriptor_t * pFWDescriptor = (const FWDescriptor_t *)&data[len-BLC_FW_DESCIPTOR_SIZE];
  const unsigned int major = pFWDescriptor->swVerMajor;
  const unsigned int minor = pFWDescriptor->swVerMinor;

  wchar2char(serial, LE_Devices->Text.c_str(), sizeof(serial));
  snprintf(filename, sizeof(filename), "FLP%s.json", serial);

  data[0] = 0;
  snprintf(data, DFU_FILE_MAX_SIZE, "{\"fw_major\":%u,\"fw_minor\":%u}", major, minor);

  DfuRestSendFile("cmd", filename, strlen(data), (uint8_t *)data);

}
//---------------------------------------------------------------------------

