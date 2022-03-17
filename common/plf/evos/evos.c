/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stdio.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"
#include "plf/evos/evos.h"

#if !defined(BOOTLOADER)

#include "plf/cli/cli.h"
#include "plf/trc/trc.h"

#if (PLF_OS == PLF_OS_RTOS)
  #include "plf/rtos/rtos.h"
#endif

#if (PLF_OS == PLF_OS_WINDOWS)
#endif


#endif

/*******************************************************************************
 *    Type definitions
 ******************************************************************************/

typedef struct {
  bool                on;    /**< true if the event is signaled. */
  PlfTime_t           time;  /**< Time at which the event will occur. */
  PlfTime_t           reloadTime;
  EvosEventParam_t    param; /**< Parameter value associated with an occurrence of the event. */
  EvosEventFunc_t     func;  /**< Pointer to event handler function. */
  const char          *name; /**< Name of event in string form. Used only for trace purposes. */
}                     EvosEvent_t;

/*******************************************************************************
 *    Local variables
 ******************************************************************************/

static EvosEvent_t      evosEventPool[EVOS_POOL_SIZE];  /**< Global pool of events. */
PlfTime_t               evosCurrentTime;   /**< Current timer tick value as obtained from HAL. */
//PlfTime_t               evosScheduledTime;   /**< Scheduled timer tick value for current event. */
static EvosEventHandle_t currentEvent;


/******************************************************************************/
#ifdef CLI_ENABLE

/******************************************************************************/

typedef struct {
  uint32_t              count;
  PlfTime_t             time;
}                       EvosLog_t;

static EvosLog_t        evosEventLog[EVOS_POOL_SIZE];
//static PlfTime_t        measureStartTime; //, measureStopTime;
static PlfTime_t        startTime, stopTime;


/******************************************************************************/
#endif /*  */
/******************************************************************************/

/* Register event */
EvosEventHandle_t EvosEventRegister(
  const EvosEventFunc_t  pFunc,
  const char * const pName)
{
  EvosEventHandle_t i;
  i=0;
  while ((i<EVOS_POOL_SIZE) && (evosEventPool[i].func != NULL)) {
    i++;
  }
  if (i==EVOS_POOL_SIZE) {
#if !defined(BOOTLOADER)
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "EvOS Pool To Small!!");
#endif
    for (;;) ;
  } else {
    //memset(&evosEventPool[i], 0, sizeof(EvosEvent_t));
    evosEventPool[i].func = pFunc;
    evosEventPool[i].name = pName;
  }
  return i;
}

/* clear event  */
void EvosEventClear(
  const EvosEventHandle_t  event)
{
  if (event < EVOS_POOL_SIZE) {
    HalIrqStat_t istat;
    HalInterruptDisable(&istat);
    evosEventPool[event].on = false;
    evosEventPool[event].time  = 0;
    evosEventPool[event].reloadTime = 0;
    HalInterruptRestore(istat);
  }
}

/* add repeated time event */
void EvosEventSetAndReload(
  const EvosEventHandle_t event,
  const PlfTime_t firstDeltaTime, 
  const PlfTime_t reloadTime, 
  EvosEventParam_t param)
{
  if (event < EVOS_POOL_SIZE) {
    HalIrqStat_t istat;
    HalInterruptDisable(&istat);
    //evosEventPool[event].time  = firstDeltaTime;
    evosEventPool[event].time  = evosCurrentTime+firstDeltaTime;
    evosEventPool[event].reloadTime = reloadTime;
    evosEventPool[event].param = param;
    evosEventPool[event].on = true;
    HalInterruptRestore(istat);
  }
}

/* add one-shoot time event */
volatile int xx = 0;
void EvosEventSet(
  const EvosEventHandle_t event,
  const PlfTime_t time, 
  EvosEventParam_t param)
{
  if (event == 0) {
    xx++;
  }
  if (event < EVOS_POOL_SIZE) {
    HalIrqStat_t istat;
    HalInterruptDisable(&istat);
    evosEventPool[event].time  = time;
    evosEventPool[event].param = param;
    evosEventPool[event].on = true;
    HalInterruptRestore(istat);
  }
}

/* add one-shoot time event, expire after "deltaTime" from now - might drift*/
void EvosEventSetDelta2(
  const EvosEventHandle_t event,
  const PlfTime_t         deltaTime, 
  EvosEventParam_t        param )
{
  PlfTime_t plfTime;
  PlfTimeMsGet(&plfTime);
  EvosEventSet(event, plfTime+deltaTime, param);
}

/* add one-shoot time event, expire after "deltaTime" from evos time - might drift  */
void EvosEventSetDelta(
  const EvosEventHandle_t event,
  const PlfTime_t         deltaTime, 
  EvosEventParam_t        param )
{
  EvosEventSet(event, evosCurrentTime+deltaTime, param);
}

/* add one-shoot time event, expire after "deltaTime" from last scheduled time - NO permanent drift  */
void EvosEventSetDeltaNoDrift(
  const EvosEventHandle_t event,
  const PlfTime_t         deltaTime, 
  EvosEventParam_t        param )
{
  EvosEventSet(event, evosEventPool[event].time+deltaTime, param);
}


/* add one-shoot time event, expire ASAP */
void EvosEventSetNow(
  const EvosEventHandle_t event, 
  EvosEventParam_t param)
{
  EvosEventSet(event, evosCurrentTime, param);
}

/******************************************************************************/

/* Re-Register event */
void EvosEventReregister(
  const EvosEventHandle_t  event,
  const EvosEventFunc_t  pFunc)
{
  //memset(&evosEventPool[event], 0, sizeof(EvosEvent_t));
  evosEventPool[event].func  = pFunc;
  evosEventPool[event].on    = false;
}

/* Re-Register event */
void EvosEventDelete(
  const EvosEventHandle_t  event)
{ 
  if (event == EVOS_CURRENT_HANDLE) {
    memset(&evosEventPool[currentEvent], 0, sizeof(EvosEvent_t));
  } else {
    memset(&evosEventPool[event], 0, sizeof(EvosEvent_t));
  }
}


/******************************************************************************/

const char * EvosEventNameGet(EvosEventHandle_t i)
{
  return evosEventPool[i].name;
}

/******************************************************************************/



static bool _TimeUpAndClearAtomic(void)
{
  bool run = false;
  HAL_START_ATOMIC();
  if (evosEventPool[currentEvent].on) {
    if (TIME_UP(evosCurrentTime, evosEventPool[currentEvent].time)) {
      run = true;
      evosEventPool[currentEvent].on = false;
    }
  }
  HAL_END_ATOMIC();
  return run;
}

void EvosSchedulerCheck(void)
{
  PlfTimeMsGet(&evosCurrentTime);
#ifndef BOARD_TekPartner_TT3
  HalWdtFeed();
#endif
  for (currentEvent=0; currentEvent<EVOS_POOL_SIZE; currentEvent++) {
    if (_TimeUpAndClearAtomic()) {
      #if defined(CLI_ENABLE) 
        evosEventLog[currentEvent].count++;
        PlfTimeMsGet(&startTime);
      #endif
      if (evosEventPool[currentEvent].reloadTime > 0) {
        evosEventPool[currentEvent].time += evosEventPool[currentEvent].reloadTime;
        evosEventPool[currentEvent].on = true;
      }
      evosEventPool[currentEvent].func(
      #ifdef EVOS_PARAM_USED
        evosEventPool[currentEvent].param
      #endif
        );
      #if defined(CLI_ENABLE) 
        PlfTimeMsGet(&stopTime);
        evosEventLog[currentEvent].time += stopTime - startTime;
      #endif
    }
  }
}


/******************************************************************************/

static bool init = false;
void EvosInit(void)
{
  if (!init) {
    memset(&evosEventPool, 0, sizeof(evosEventPool));
    init = true;
  }
}

#if (STACK_SIZE > 0)
  extern uint8_t * pst;
  int li = STACK_SIZE;
#endif

void EvosSchedulerStart(void const * argument)
{
  EvosInit();
  for (;;) {
	  EvosSchedulerCheck();
#if (PLF_OS == PLF_OS_RTOS)
//		RTOS_TASK_DELAY(1);
#endif
  }
}

/******************************************************************************/
#ifdef CLI_ENABLE
/******************************************************************************/

int_fast16_t CliEvosStat(CliParam_t param1, CliParam_t param2, CliParam_t param3)
{
  char buf[80];
  static const char event_list_txt[] =
    "EVOS Events:" CLI_NL
    "  Name             No      Count   Time(ms)";
  static const char  format_txt[] =
    "%4d %10ld %10ld";

  PlfTime_t measureStopTime;
  int i,j;
  int free = 0;

  CliWriteLine();

  PlfTimeMsGet(&measureStopTime);
//  diffTime = (measureStopTime - measureStartTime);

  /* Event info */

  CliWriteLn(event_list_txt);
  for (i=0; i<EVOS_POOL_SIZE; i++) {
	if (evosEventPool[i].func != 0) {
	  buf[17] = 0;
	  strcpy((char *)buf, "  ");
	  strncat((char *)buf, evosEventPool[i].name, 15);
	  for (j=strlen((char *)buf); j<17; j++)
			  buf[j] = ' ';
	  sprintf(
		(char *)(buf+strlen((char *)buf)),
		format_txt,
		i,
		(long int)evosEventLog[i].count,
		(long int)evosEventLog[i].time);

	  CliWriteLn(buf);
	  /* Restart accumulation of processing time. */
	  evosEventLog[i].time = 0;
	} else {
	  free++;
	}
  }
  memset(&evosEventLog, 0, sizeof(evosEventLog));
  CliPrintf("Free handles: %u" CLI_NL, free);
  CliWriteLine();

  return CLI_RESULT_OK;
}

/******************************************************************************/
#endif /*   */
/******************************************************************************/
