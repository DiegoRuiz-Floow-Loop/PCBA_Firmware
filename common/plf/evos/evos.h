/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef EVOS_H
#define EVOS_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************/

/* configuration of EvOS ... */
#include "plf/plf.h"
#include "plf/cfg/evos_cfg.h"
#include "plf/cli/cli.h"

/******************************************************************************/

#define EVOS_CURRENT_HANDLE         (EvosEventHandle_t)(UINT8_MAX)
#define EVOS_UNINITIALIZED_HANDLE   (EvosEventHandle_t)(UINT8_MAX)

/******************************************************************************/

/* may be read by the event functions etc. */
extern PlfTime_t        evosCurrentTime;      /* Current timer is the ms tick value as obtained from HAL. */
//extern PlfTime_t        evosScheduledTime;    /* Scheduled timer is the ms tick value scheduled for current event. */

/******************************************************************************/

extern EvosEventHandle_t EvosEventRegister(
  const EvosEventFunc_t   pFunc,
  const char * const      pName);

/* add repeated time event */
extern void EvosEventSetAndReload(
  const EvosEventHandle_t event,
  const PlfTime_t firstDeltaTime,
  const PlfTime_t reloadTime,
  EvosEventParam_t param);

/* add one-shoot time event */
extern void EvosEventSet(
  const EvosEventHandle_t event,
  const PlfTime_t         time,
  EvosEventParam_t        param);

/* add one-shoot time event, expire after "deltaTime" from now - might drift*/
extern void EvosEventSetDelta2(
  const EvosEventHandle_t event,
  const PlfTime_t         deltaTime, 
  EvosEventParam_t        param);

/* add one-shoot time event, expire after "deltaTime" from evos time - no drift  */
extern void EvosEventSetDelta(
  const EvosEventHandle_t event,
  const PlfTime_t  deltaTime,
  EvosEventParam_t param);
extern void EvosEventSetDeltaNoDrift(
  const EvosEventHandle_t event,
  const PlfTime_t  deltaTime,
  EvosEventParam_t param);

/* add one-shoot time event, expire ASAP - usefull from IRQ routine */
extern void EvosEventSetNow(
  const EvosEventHandle_t event,
  EvosEventParam_t param);

extern void EvosEventClear(
  const EvosEventHandle_t event);


extern void EvosEventReregister(
  const EvosEventHandle_t event,
  const EvosEventFunc_t   pFunc);

extern void EvosEventDelete(
  const EvosEventHandle_t event);

/******************************************************************************/

#ifdef CLI_ENABLE

extern int_fast16_t CliEvosStat(CliParam_t param1, CliParam_t param2, CliParam_t param3);

#endif

extern const char * EvosEventNameGet(EvosEventHandle_t i);


/******************************************************************************/

extern void EvosInit(void);

extern void EvosSchedulerCheck(void);

extern void EvosSchedulerStart(void const * argument);


/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif //EVOS_H
