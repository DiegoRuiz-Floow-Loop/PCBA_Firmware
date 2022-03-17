/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <stdio.h>

#include <Classes.hpp>

#include "plf/plf.h"
#include "plf/evos/evos.h"

/*******************************************************************************
 *    Type definitions
 ******************************************************************************/

typedef struct {
  bool                on;    /**< TRUE if the event is signaled. */
  PlfTime_t             time;  /**< Time at which the event will occur. */
  PlfTime_t             reloadTime;
  EvosEventParam_t      param; /**< Parameter value associated with an occurrence of the event. */
  EvosEventFunc_t       func;  /**< Pointer to event handler function. */
  const char            *name; /**< Name of event in string form. Used only for trace purposes. */
}                       EvosEvent_t;

/*******************************************************************************
 *    Local variables
 ******************************************************************************/

volatile EvosEvent_t evosEventPool[EVOS_POOL_SIZE];  /**< Global pool of events. */
PlfTime_t      evosCurrentTime;   /**< Current timer tick value as obtained from HAL. */
PlfTime_t      evosScheduledTime;   /**< Scheduled timer tick value for current event. */

/******************************************************************************/

bool EvosEventIsSet(EvosEventHandle_t event)
{
  return evosEventPool[event].on;
}

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
    //HalIrqStat_t istat;
    //HalInterruptDisable(&istat);
    evosEventPool[event].on = FALSE;
    evosEventPool[event].time  = 0;
    evosEventPool[event].reloadTime = 0;
    //HalInterruptRestore(istat);
  }
}

/* add repeated time event */
void EvosEventSetAndReload(
  const EvosEventHandle_t event,
  const PlfTime_t firstTime,
  const PlfTime_t reloadTime,
  EvosEventParam_t param)
{
  if (event < EVOS_POOL_SIZE) {
    //HalIrqStat_t istat;
    //HalInterruptDisable(&istat);
    evosEventPool[event].time  = firstTime;
    evosEventPool[event].reloadTime = reloadTime;
    evosEventPool[event].param = param;
    evosEventPool[event].on = TRUE;
    //HalInterruptRestore(istat);
  }
}

/* add one-shoot time event */
void EvosEventSet(
  const EvosEventHandle_t event,
  const PlfTime_t time,
  EvosEventParam_t param)
{
  if (event < EVOS_POOL_SIZE) {
    //HalIrqStat_t istat;
    //HalInterruptDisable(&istat);
    evosEventPool[event].time  = time;
    //evosEventPool[event].reloadTime = 0;
    evosEventPool[event].param = param;
    evosEventPool[event].on = TRUE;
    //HalInterruptRestore(istat);
  }
}

/* add one-shoot time event, expire after "deltaTime" from now */
void EvosEventSetDelta(
  const EvosEventHandle_t event,
  const PlfTime_t         deltaTime,
  EvosEventParam_t        param )
{
  EvosEventSet(event, evosCurrentTime+deltaTime, param);
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
  evosEventPool[event].on    = FALSE;
}

/* Re-Register event */
void EvosEventDelete(
  const EvosEventHandle_t  event)
{
  memset((void*)&evosEventPool[event], 0, sizeof(EvosEvent_t));
}

bool EvosEventPending(
  const EvosEventHandle_t event)
{
	return evosEventPool[event].on;
}

/******************************************************************************/

const char * EvosEventNameGet(EvosEventHandle_t i)
{
  return evosEventPool[i].name;
}

static void  EvosSchedulerCheck(void)
{
  bool timeUp;
  EvosEventHandle_t currentEvent;

  PlfTimeMsGet((PlfTime_t *)&evosCurrentTime);

  for (currentEvent=0; currentEvent<EVOS_POOL_SIZE; currentEvent++) {
		if (evosEventPool[currentEvent].on) {
      timeUp = TIME_UP(evosCurrentTime, evosEventPool[currentEvent].time);
      if (timeUp) {

        evosEventPool[currentEvent].on = FALSE;
        evosScheduledTime = evosEventPool[currentEvent].time;
        if (evosEventPool[currentEvent].reloadTime > 0) {
          evosEventPool[currentEvent].time += evosEventPool[currentEvent].reloadTime;
          evosEventPool[currentEvent].on = TRUE;
        }
				//TRACE_VA(TRC_TA_DIA, TRC_TL_TIMER, "Event: %u", currentEvent);
				evosEventPool[currentEvent].func(evosEventPool[currentEvent].param);
      }
    }
  }
}

/******************************************************************************/

class ThreadTimer_t : public TThread
{
private:
protected:
   void __fastcall Execute();
public:
   //char     nextString[80];

   __fastcall ThreadTimer_t(bool CreateSuspended);
};

ThreadTimer_t * pTimer;

__fastcall ThreadTimer_t::ThreadTimer_t(bool CreateSuspended) : TThread(CreateSuspended)
{
}

void __fastcall ThreadTimer_t::Execute()
{
  for (;;) {
    EvosSchedulerCheck();
    Sleep(1);
  };
}


static bool init = FALSE;

void EvosInit(void)
{
  if (!init) {
		memset((void *)&evosEventPool, 0, sizeof(evosEventPool));
		init = TRUE;
  }
}

void EvosSchedulerStart(const void * p)
{
  EvosInit();
  ThreadTimer_t * pTimer = new ThreadTimer_t(true);  /* create but don't run */
  pTimer->Priority = tpNormal;        /* set the priority lower than normal */
  pTimer->Resume();                   /* now start the thread running */
}

/******************************************************************************/

