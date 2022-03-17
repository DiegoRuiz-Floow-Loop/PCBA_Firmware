/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <string.h>

#include "hal/dflash/dflash.h"
#include "plf/trc/trc.h"

#include "modem.h"
#include "net/rest/rest.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#if defined(CLI_ENABLE)

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// Command line
//static int_fast16_t CliShow(CliParam_t p1, CliParam_t p2, CliParam_t p3);
static int_fast16_t CliAT(CliParam_t sz, CliParam_t param2, CliParam_t param3);
static int_fast16_t CliPower(CliParam_t on, CliParam_t p2, CliParam_t p3);
static int_fast16_t CliNetConnect(CliParam_t on, CliParam_t p2, CliParam_t p3);
#if defined(MODEM_TCP_SUPPORT)
static int_fast16_t CliTcpOpen(CliParam_t host, CliParam_t port, CliParam_t p3);
static int_fast16_t CliTcpClose(CliParam_t p1, CliParam_t p2, CliParam_t p3);
static int_fast16_t CliTcpSend(CliParam_t data, CliParam_t length, CliParam_t p3);
#endif


/*******************************************************************************
 * Global data variables
 ******************************************************************************/

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Local functions, cli
 ******************************************************************************/
 
//int_fast16_t TestModemShow(CliParam_t param1, CliParam_t param2, CliParam_t param3)
//{
//  char sz[80];
//  int16_t i;

//  if (cliShowHeader) {
//    CliWriteLine();
//    CliWrite("| Modem Status");  CliWriteEol();
//    CliWriteLine();
//  }

//  CliWrite( "| Mobile Network: "); CliWriteEol();
//  CliPrintf("|   IMEI:    %s", imei); CliWriteEol();
//  CliPrintf("|   FW Ver:  %s", cgmr); CliWriteEol();
//  CliPrintf("|   APN:     %s", apn); CliWriteEol();
//  CliPrintf("|   Reg.:    %u sec (time to regsiter on Network)", ((cgregTime + 500) / 1000)); CliWriteEol();
//  CliWriteLine();
//  
//  return CLI_RESULT_OK;
//}

static void PrintPowerAndReady(void)
{
  CliPrintf("| Power  : %s", (xModemPowerOn ? "On" : "Off")); 
  if (xModemPowerLocked) {
    CliWrite(", working");
  }
  if (xModemPowerOn) {
    CliPrintf(", UART %s", (xModemReadyUart ? "ready" : "not ready"));
  }
  CliWriteLn("");
  
  if (!xModemPowerOn) return;
  
  CliWrite("| SIM    : ");
  if (xModemSimNotInserted) {
    CliWriteLn("Not inserted");
  } else if (!xModemReadySim) {
    CliWriteLn("Not ready");
  } else {
    CliPrintf("Ready, IMSI %s" CLI_NL, xModemSimImsi);
  }
}

static void PrintNetwork(void)
{
  CliPrintf("| Tower  : %s, rssi %u" CLI_NL, (xModemNetServiceAttached ? "Attached" : "Detached"), 
                                                                xModemNetRssi);

  CliPrintf("| Info   : %s %s" CLI_NL, xModemNetInfo, xModemNetMode);

  
  CliPrintf("| Network: %s", (xModemNetConnected ? "Connected" : "Disconnected"));
  if (!xModemNetConnected && xModemNetLocked) {
    CliWrite(" (working)");
  }
  CliWriteLn("");  
}

#if defined(MODEM_TCP_SUPPORT)
static void PrintTcp(void)
{
  CliPrintf("| TCP    : Socket %s", (xModemTcpConnected ? "open" : "closed"));
  if (!xModemTcpConnected && xModemTcpLocked) {
    CliWrite(" (working)");
  }
  CliWriteLn("");
  
  if (xModemTcpHost[0] != '\0') {
    CliPrintf("| Host   : %s:%u" CLI_NL, xModemTcpHost, xModemTcpPort);
  } 
}
#endif


int_fast16_t TestModemShow(const CliParam_t p1, const CliParam_t p2, const CliParam_t p3)
{
  if (cliShowHeader) {
    CliWriteLine();
    CliWrite("| Modem Status");  CliWriteEol();
    CliWriteLine();
  }

  CliWrite( "| Mobile Network: "); CliWriteEol();

  CliPrintf("|   Model:   %s" , xModemModel); CliWriteEol();
  CliPrintf("|   IMEI:    %s" , xModemImei); CliWriteEol();
  CliPrintf("|   FW Ver:  %s" , xModemRevision); CliWriteEol();
  CliPrintf("|   APN:     %s" , xModemNetApn); CliWriteEol();

  if (cliShowHeader) {
    CliWriteLine();
    PrintPowerAndReady();
    CliWriteLine();
    PrintNetwork();
    CliWriteLine();
  #if defined(MODEM_TCP_SUPPORT)  
    PrintTcp();
    CliWriteLine();
  #endif  
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliAT(CliParam_t sz, CliParam_t param2, CliParam_t param3)
{
  char txt[100];
  strncpy(txt, (char*)sz, 100);
  for (uint_fast16_t i = 0; i < strlen(txt); i++) {
    if (txt[i]=='#') {
      txt[i] = '?';
    }
  }
  ModemAtSendText(txt);
  return CLI_RESULT_OK;
}

static int_fast16_t CliPower(CliParam_t on, CliParam_t p2, CliParam_t p3)
{
  UNUSED(p2);
  UNUSED(p3);  
    
  bool success;
  if (on) {
    success = ModemPowerOn();
  } else {
    success = ModemPowerOff();
  }
  
  CliWriteLine();
  PrintPowerAndReady();
  CliWriteLine();
  return (success ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);
}

static int_fast16_t CliNetConnect(CliParam_t on, CliParam_t p2, CliParam_t p3)
{
  UNUSED(p2);
  UNUSED(p3);
  
  bool success;
  if (on) {
    success = ModemNetConnect();
  } else {
    success = ModemNetDisconnect();
  }
  
  CliWriteLine();
  PrintNetwork();
  CliWriteLine();
  return (success ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);
}

#if defined(MODEM_TCP_SUPPORT)
static int_fast16_t CliTcpOpen(CliParam_t host, CliParam_t port, CliParam_t p3)
{
  UNUSED(p3);
  
  // Persistent buffer for the tcp host
  static char sz[65];
  strncpy(sz, (char *)host, sizeof(sz));
  
  const bool success = ModemTcpOpen(sz, port);
  CliWriteLine();
  PrintTcp();
  CliWriteLine();  
  return (success ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);
}
#endif

#if defined(MODEM_TCP_SUPPORT)
static int_fast16_t CliTcpClose(CliParam_t p1, CliParam_t p2, CliParam_t p3)
{
  UNUSED(p1);
  UNUSED(p2);
  UNUSED(p3);
  
  const bool success = ModemTcpClose();
  CliWriteLine();
  PrintTcp();
  CliWriteLine();    
  return (success ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);
}
#endif

#if defined(MODEM_TCP_SUPPORT)
static int_fast16_t CliTcpSend(CliParam_t data, CliParam_t length, CliParam_t p3)
{
  UNUSED(p3);
  
  if ((length < 1) || (length > 8)) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  const bool success = ModemTcpSendData((const uint8_t *) data, length);
  return (success ? CLI_RESULT_OK : CLI_RESULT_ERROR_UNDEFINED);
}
#endif

#if defined(MODEM_FOTA_SUPPORT)
static int_fast16_t CliFotaUrl(const CliParam_t url, const CliParam_t p2, const CliParam_t p3)
{
  UNUSED(p2);
  UNUSED(p3);
  
  // Persistent string buffers
  static char szUrl[128];
  strncpy(szUrl, (char *)url, sizeof(szUrl));  

  // Fix problem with cli and '?' character which is reserved for other use
  for (uint_fast16_t idx = 0; idx < strlen(szUrl); idx++) {
    if (szUrl[idx] == '#') {
      szUrl[idx] = '?';
    }
  }
  
  if (!StrGE(szUrl, "http://")) {
    CliWriteLn("Url: Not HTTP protocol (http://)");
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }

  CliPrintf("Url: %s" CLI_NL, szUrl);
  ModemFotaHttpUpdate(szUrl);
  return CLI_RESULT_OK;
}

static int_fast16_t CliFotaDefault(const CliParam_t p1, const CliParam_t p2, const CliParam_t p3)
{
  UNUSED(p1);
  UNUSED(p2);
  UNUSED(p3);  
  
  static const char * const url = MODEM_FOTA_DEFAULT_URL;
  return CliFotaUrl((CliParam_t)url, 0, 0);
}

static int_fast16_t CliFotaStatus(const CliParam_t p1, const CliParam_t p2, const CliParam_t p3)
{
  UNUSED(p1);
  UNUSED(p2);
  UNUSED(p3);  
  
  CliWrite("FOTA: "); 
  if (!xModemFotaLocked) {
    CliWriteLn("Is idle"); 
  } else {
    if (xModemFotaIsDownloading) {
      CliWriteLn("Is downloading");
      ModemFotaStatusCheck();
    } else if (xModemFotaIsUpdating) {
      CliWriteLn("Is updating"); 
    } else if (xModemFotaIsSuccess) {
      CliWriteLn("Update success! (waiting for reset)"); 
    } else {
      CliWriteLn("Update fail! (waiting for reset)"); 
    }
  }
  return CLI_RESULT_OK;
}
#endif

CLI_START_TABLE(modem)
  CLI_ENTRY0( "show",   "Show modem status", TestModemShow)
  CLI_ENTRY1( "at",     "Command to modem (use # as ?)", CliAT, CLI_PARAM_STRING_LAST)
  CLI_ENTRY1( "pwr",    "Set modem power [on/off]", CliPower, CLI_PARAM_ONOFF)
  CLI_ENTRY1( "net",    "Set modem network [on/off]", CliNetConnect, CLI_PARAM_ONOFF)
#if defined(MODEM_TCP_SUPPORT)  
  CLI_ENTRY2( "open",   "Open TCP socket [host:max 64 length, port]", CliTcpOpen, CLI_PARAM_STRING, CLI_PARAM_UINT32)  
  CLI_ENTRY0( "close",  "Close TCP socket", CliTcpClose)
  CLI_ENTRY2( "send",   "Send TCP data [data, length:1..8]", CliTcpSend, CLI_PARAM_P_UINT64, CLI_PARAM_UINT32) 

  CLI_ENTRY3( "get",     "HTTP GET file <name> <address> <max length>", TestHGet, CLI_PARAM_STRING, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
//CLI_ENTRY3( "post",    "HTTP POST file <name> <address> <length>", TestHPost, CLI_PARAM_STRING, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
//CLI_ENTRY3( "dfrd",    "Read from DF <addr> <len> <width: 1, 2, 4, 8>", CliDFlashRead, CLI_PARAM_UINT32, CLI_PARAM_UINT32, CLI_PARAM_UINT32)
#endif
#if defined(MODEM_MQTT_SUPPORT)  

#endif
#if defined(MODEM_FOTA_SUPPORT)
  CLI_ENTRY0( "fota",   "FOTA update from iot-factory.dk via HTTP", CliFotaDefault)
  CLI_ENTRY1( "furl",   "FOTA update from url via HTTP (use # as ?) [url]", CliFotaUrl, CLI_PARAM_STRING)
  CLI_ENTRY0( "fstat",  "FOTA update status", CliFotaStatus)
#endif
  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(modem)


/******************************************************************************/
#endif
