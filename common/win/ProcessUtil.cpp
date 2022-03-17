/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <vcl.h>
#include "ProcessUtil.h"
#include "wchar.h"

bool StartProcess(
  char * processName,
  bool wait,
  PROCESS_INFORMATION * processInformation )
{
  bool ok;
  STARTUPINFO si;
  memset(&si, 0, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.lpTitle = "";
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_MINIMIZE; //SW_SHOWDEFAULT;

  ok = CreateProcess(
    NULL,
	  processName,
    NULL,//security
    NULL,// security
    true,//inherits handles
    0,
    NULL,
    NULL,
    &si,
	  processInformation);

  if (ok & wait) {
    WaitForSingleObject(processInformation->hProcess, INFINITE );
  }
  return ok;
}


