/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "modem.h"

#if defined(MODEM_FOTA_SUPPORT)

#include "plf/evos/evos.h"

#include "modem_fota.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  FOTA_IDLE,
  FOTA_DOWNLOAD,
  FOTA_DOWNLOAD_WAIT,
  FOTA_DOWNLOAD_CHECK,  
  FOTA_UPDATE,
  FOTA_UPDATE_WAIT,
  FOTA_RESET,
  FOTA_RESET_WAIT,
  FOTA_Last
} FotaState_t;

typedef enum {
  HTTP_IDLE   =   0,
  HTTP_BUSY   =   1,
  HTTP_200_OK = 200
} HttpStatus_t;

/*******************************************************************************
 * Function prototypes, state machine on entry/exit
 ******************************************************************************/

static void DoNothing(void);

static void OnEntryIdle(void);
static void OnExitIdle(void);
static void OnEntryDownload(void);
static void OnExitDownload(void);
static void OnEntryDownloadCheck(void);
static void OnEntryUpdate(void);
static void OnExitUpdate(void);
static void OnEntryReset(void);
static void OnEntryResetWait(void);
static void OnExitResetWait(void);

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const ModemSmStateFuncs_t FOTA_FUNCS[FOTA_Last] = {
  // .onEntry,               .onExit
  { OnEntryIdle,             OnExitIdle },              // FOTA_IDLE
  { OnEntryDownload,         OnExitDownload },          // FOTA_DOWNLOAD
  { DoNothing,               DoNothing },               // FOTA_DOWNLOAD_WAIT
  { OnEntryDownloadCheck,    DoNothing },               // FOTA_DOWNLOAD_CHECK
  { OnEntryUpdate,           OnExitUpdate },            // FOTA_UPDATE  
  { DoNothing,               DoNothing },               // FOTA_UPDATE_WAIT
  { OnEntryReset,            DoNothing },               // FOTA_RESET
  { OnEntryResetWait,        OnExitResetWait },         // FOTA_RESET_WAIT  
};

static const char * FOTA_STATE_TEXTS[FOTA_Last] = {
  "fota idle",               // FOTA_IDLE
  "fota download request",   // FOTA_DOWNLOAD
  "fota download wait",      // FOTA_DOWNLOAD_WAIT
  "fota download check",     // FOTA_DOWNLOAD_CHECK
  "fota update request",     // FOTA_UPDATE
  "fota update wait",        // FOTA_UPDATE_WAIT  
  "fota reset",              // FOTA_RESET  
  "fota reset wait",         // FOTA_RESET_WAIT  
};

static const uint32_t RESET_WAIT_MSEC = 60 * 1000;

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

static EvosEventHandle_t timer = EVOS_UNINITIALIZED_HANDLE;

static bool isTimeout;
static bool isDownloadDone;
static bool shouldDownloadCheck;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

// State machine
static bool NextFotaState(uint8_t current, uint8_t * next);

// AT parse
static bool AtParse(const char * sz);

// Other
static void TimerSet(uint32_t msec);
static void TimerClear(void);
static void TimeoutEvent(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void ModemFotaInit(void)
{
  timer = EvosEventRegister(TimeoutEvent, "modem fota timer");
  
  ModemSmRegister(MODEM_SM_FOTA, NextFotaState, FOTA_FUNCS, FOTA_STATE_TEXTS);
  ModemAtRegister(MODEM_SM_FOTA, AtParse, NULL, NULL);
}

bool ModemFotaHttpUpdate(const char * const url)
{
  if (!xModemNetConnected || xModemFotaLocked) return false;
  
  ModemLogFunc("ModemFotaHttpUpdate(...)", NULL, NULL);
  
  xModemFotaLocked = true;
  xModemFotaUrl = url;
  ModemSmUpdate();
  return true;  
}

void ModemFotaStatusCheck(void)
{
  if (xModemFotaIsDownloading) {
    shouldDownloadCheck = true;
  }
  ModemSmUpdate();
}

/*******************************************************************************
 * Local functions, state machine
 ******************************************************************************/

static void DoNothing(void)
{
}

static bool NextFotaState(const uint8_t current, uint8_t * const next)
{
  if (xModemAtBusy) return false;

  *next = FOTA_Last;
  switch (current) {
    case FOTA_IDLE:
      if (xModemFotaLocked) *next = FOTA_DOWNLOAD;
      break;
    
    case FOTA_DOWNLOAD:
      *next = (xModemAtOkReceived ? FOTA_DOWNLOAD_WAIT : FOTA_IDLE );
      break;

    case FOTA_DOWNLOAD_WAIT:
      if      (!xModemFotaIsDownloading && isDownloadDone) *next = FOTA_UPDATE;
      else if (!xModemFotaIsDownloading)                   *next = FOTA_IDLE;
      else if (shouldDownloadCheck)                        *next = FOTA_DOWNLOAD_CHECK;
      break;
    
    case FOTA_DOWNLOAD_CHECK:
      *next = FOTA_DOWNLOAD_WAIT;
      break;
    
    case FOTA_UPDATE:
      *next = (xModemAtOkReceived ? FOTA_UPDATE_WAIT : FOTA_IDLE );
      break;

    case FOTA_UPDATE_WAIT:
      if (!xModemFotaIsUpdating) *next = FOTA_RESET_WAIT;
      break;    
    
    case FOTA_RESET_WAIT:
      if (isTimeout) *next = FOTA_RESET;
      break;    

    case FOTA_RESET:    
    default:
      *next = FOTA_IDLE;
      break;
  }
  return (*next < FOTA_Last);
}

static void OnEntryIdle(void)
{
  xModemFotaLocked = false;
}

static void OnExitIdle(void)
{
  isDownloadDone      = false;
  shouldDownloadCheck = false;
  xModemFotaIsSuccess = false;
}

static void OnEntryDownload(void)
{
  ModemAtSendText("AT+HTTPTOFS=\"%s\",\"/fota/update.zip\"", xModemFotaUrl);
}

static void OnExitDownload(void)
{
  xModemFotaIsDownloading = xModemAtOkReceived;
}

static void OnEntryDownloadCheck(void)
{
  ModemAtSendText("AT+HTTPTOFS?");
  shouldDownloadCheck = false;
}

static void OnEntryUpdate(void)
{
  ModemAtSendText("AT+CFOTA=1");
}

static void OnExitUpdate(void)
{
  xModemFotaIsUpdating = xModemAtOkReceived;
}

static void OnEntryReset(void)
{
  HalReboot();
}

static void OnEntryResetWait(void)
{
  TimerSet(RESET_WAIT_MSEC);
}

static void OnExitResetWait(void)
{
  TimerClear();
}

/*******************************************************************************
 * Local functions, AT parse
 ******************************************************************************/

static bool AtParse(const char * const sz)
{
  int d1;  
  
  if (sscanf(sz, "+HTTPTOFS: %d,", &d1) == 1) {
    switch (d1) {
      case HTTP_IDLE:
        xModemFotaIsDownloading = false;
        break;
      case HTTP_BUSY:
        break;
      case HTTP_200_OK:
        isDownloadDone = true;
        // Fall-through
      default:        
        xModemFotaIsDownloading = false;
        break;
    }
    return true;
  }
  
#if 0
  if (StrGE(sz, "+CFOTA: \"Start to update\"")) {
    return true;
  }
#endif  

#if 0  
  if (sscanf(sz, "+CFOTA: \"Updating\",%d", &d1) == 1) {
    return true;
  }
#endif  
  
  // +CFOTA: "Update successfully, please wait for reset"
  if (StrGE(sz, "+CFOTA: \"Update success")) {
    xModemFotaIsUpdating = false;
    xModemFotaIsSuccess  = true;
    return true;
  }

  // +CFOTA: "Update failed, please wait for reset"
  if (StrGE(sz, "+CFOTA: \"Update fail")) {
    xModemFotaIsUpdating = false;
    return true;
  }  
  
  return false;
}

/*******************************************************************************
 * Local functions, other
 ******************************************************************************/

static void TimerSet(const uint32_t msec)
{
  EvosEventSetDelta2(timer, msec, 0);
  isTimeout = false;
}

static void TimerClear(void)
{
  EvosEventClear(timer);
  isTimeout = false;
}

static void TimeoutEvent(const EvosEventParam_t param)
{
  ModemLogState("fota timeout");
  
  isTimeout = true;
  ModemSmUpdate();
}

/******************************************************************************/
#endif  //#if defined(MODEM_FOTA_SUPPORT)
