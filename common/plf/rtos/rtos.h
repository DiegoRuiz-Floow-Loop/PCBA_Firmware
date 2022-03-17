/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_RTOS_H
#define PLF_RTOS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"
#include "plf/cli/cli.h"
#include "plf/cfg/rtos_cfg.h"


//------------------------------------------------------------------------------
#if (PLF_OS == PLF_OS_RTOS)
//------------------------------------------------------------------------------

/* TARGET  - FreeRTOS is used */

#include <string.h>

#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"
#include "semphr.h"
#include "timers.h"
#include "cmsis_os.h"

//------------------------------------------------------------------------------

extern volatile bool  rtosRunning;

//------------------------------------------------------------------------------

/* RTOS wrapper */

#define RTOS_MALLOC(size) \
  pvPortMalloc(size)
#define RTOS_FREE(ptr) \
  vPortFree(ptr)

#define RtosBinSemaphor_t xSemaphoreHandle

#define RTOS_BIN_SEMAPHOR_CREATE(sem) \
  vSemaphoreCreateBinary(sem)
#define RTOS_BIN_SEMAPHOR_TAKE(sem, timeout) \
  xSemaphoreTake(sem, timeout)
#define RTOS_BIN_SEMAPHOR_GIVE(sem) \
  xSemaphoreGive(sem)
#define RTOS_BIN_SEMAPHOR_GIVE_FROM_ISR(sem, tast_woken) \
  xSemaphoreGiveFromISR( sem, tast_woken)

#define RTOS_SCHEDULER_START() \
  rtosRunning = true; \
  vTaskStartScheduler() 

#define RtosTime_t                        portTickType
#define RTOS_TICK_GET()                   xTaskGetTickCount()

/* ------------------------------------------------------------------------- */

#define RTOS_TRACE                        FatalError
#define RTOS_ASSERT(A)                    if (!(A)) { RTOS_TRACE("Assertion in %s, line %u: %s", __FILE__, __LINE__, #A); for (;;) {} }

#define RTOS_IDLE_PRIORITY                tskIDLE_PRIORITY
#define RTOS_MINIMAL_STACK_SIZE           configMINIMAL_STACK_SIZE
#define RtosBaseType_t                    portBASE_TYPE

/* mem */
#define RTOS_MEM_ALLOCATE(size)           pvPortMalloc(size)
#define RTOS_MEM_FREE(ptr)                vPortFree(ptr)

/* Ticks */
#define RtosTicks_t                       portTickType
#define RTOS_TICKS_FROM_MSEC(X)           (configTICK_RATE_HZ * (X) / 1000)
#define RTOS_TICKS_PER_SEC                configTICK_RATE_HZ
#define RTOS_TICKS_MAX                    portMAX_DELAY
#define RTOS_TICKS_LESS(A, B)             ((portSignedTickType)((A) - (B)) < 0)
#define RTOS_GET_TICK_COUNT()             xTaskGetTickCount()
#define RTOS_GET_TICK_COUNT_FROM_ISR()    xTaskGetTickCountFromISR()

/* Task */
#define RTOS_TASK_DELAY(d)                vTaskDelay(d)
#define RTOS_TASK_DELAY_UNTIL(pwt, inc)   vTaskDelayUntil(pwt, inc)

#define RTOS_TASK_CREATE(task, name, extraStack, prio) { \
  if (pdPASS != xTaskCreate(task, name, configMINIMAL_STACK_SIZE+extraStack, NULL, prio, NULL))  \
    for (;;); \
  }
#define RTOS_CHAR_SIGN  char 
 
#define RTOS_ENTER_CRITICAL()             taskENTER_CRITICAL()
#define RTOS_EXIT_CRITICAL()              taskEXIT_CRITICAL()
#define RTOS_TASK_DELETE(T)               vTaskDelete(T)
#define RTOS_TASK(X, P)                   void X(void * P)


// task==NULL eq current task
#define RTOS_PRIORITY_MAX                 configMAX_PRIORITIES
#define RTOS_PRIORITY_GET(task)           uxTaskPriorityGet(task)
#define RTOS_PRIORITY_SET(task, prio)     vTaskPrioritySet(task, prio)

/* Event */
#define RtosEvent_t                       xSemaphoreHandle
#define RTOS_EVENT_INIT(X)                vSemaphoreCreateBinary(X); RTOS_ASSERT(NULL != X)
#define RTOS_EVENT_SET(X)                 xSemaphoreGive(X)
#define RTOS_EVENT_RESET(X)               (void)xSemaphoreTake(X, 0)
#define RTOS_EVENT_SET_FROM_ISR(X, B)     /*OS_ASSERT*/(void)(xSemaphoreGiveFromISR(X, B))
#define RTOS_EVENT_WAIT(X, T)             xSemaphoreTake(X, T)
#define RTOS_EVENT_TAKE(X)                xSemaphoreTake(X, RTOS_TICKS_MAX)

/* Mutex */
#define RtosMutex_t                       xSemaphoreHandle
#define RTOS_MUTEX_INIT(X)                X = xSemaphoreCreateMutex()
#define RTOS_MUTEX_TAKE(X)                xSemaphoreTake(X, RTOS_TICKS_MAX)
#define RTOS_MUTEX_GIVE(X)                xSemaphoreGive(X)

/* Queue */
#define RtosQueue_t                       xQueueHandle
#define RTOS_QUEUE_INIT(Q, entries, size) Q = xQueueCreate(entries, size)
#define RTOS_QUEUE_RECV(Q, msg, timout)   xQueueReceive(Q, msg, timout)   /* == pdTRUE */
#define RTOS_QUEUE_SEND(Q, msg, timout)   xQueueSend(Q, msg, timout)      /* == pdTRUE */
#define RTOS_QUEUE_RECV_FROM_ISR(Q, E, F) xQueueReceiveFromISR(Q, E, F)
#define RTOS_QUEUE_SEND_FROM_ISR(Q, E, F) xQueueSendFromISR(Q, E, F)

/* TIME */
#define RTOS_WAIT_FOREVER               0xFFFFFFFF  

/* ------------------------------------------------------------------------- */

extern void RtosStart(void);

#ifdef CLI_ENABLE

extern int_fast16_t CliRtosStat(CliParam_t p1, CliParam_t p2, CliParam_t p3);

#endif

//------------------------------------------------------------------------------
#elif (PLF_OS == PLF_OS_WINDOWS)
//------------------------------------------------------------------------------

typedef void *  RtosBinSemaphor_t;

#define RTOS_TASK_DELAY(msec)         (void)0
#define RTOS_TASK_CREATE(task, name, extraStack, prio) (void)0


//------------------------------------------------------------------------------
#else
//------------------------------------------------------------------------------

//#error "HOV"

//------------------------------------------------------------------------------
#endif
/* ------------------------------------------------------------------------- */

#ifdef __cplusplus
}
#endif

#endif /* PLF_RTOS_H */

/* - End of file ----------------------------------------------------------- */
