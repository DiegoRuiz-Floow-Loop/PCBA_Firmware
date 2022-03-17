/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <stdio.h>
#include <string.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"

#include "plf/plf.h"
#include "plf/rtos/rtos.h"
#include "plf/trc/trc.h"

#ifndef UNUSED
#define UNUSED(x) ((void)(x))
#endif

//------------------------------------------------------------------------------

volatile bool rtosRunning = false;

//------------------------------------------------------------------------------

extern void vApplicationMallocFailedHook(void);
void vApplicationMallocFailedHook(void)
{
  for( ;; );
}

extern void vApplicationTickHook( void );
void vApplicationTickHook( void )
{
  // NOP
}

extern void vApplicationStackOverflowHook( xTaskHandle pxTask, char const * pcTaskName );

void vApplicationStackOverflowHook( xTaskHandle pxTask, char const * pcTaskName )
{
  /* This function will get called if a task overflows its stack.   If the
  parameters are corrupt then inspect pxCurrentTCB to find which was the
  offending task. */

  ( void ) pxTask;
  ( void ) pcTaskName;

  for( ;; );
}

extern void vApplicationIdleHook( void );
void vApplicationIdleHook( void )
{
  HalWdtFeed();
}

#if defined(configGENERATE_RUN_TIME_STATS)
  unsigned long ulRunTimeStatsClock = 0UL;
#endif


void RtosStart(void)
{
  rtosRunning = true;
  vTaskStartScheduler();
}

/****************************************************************************/
#ifdef CLI_ENABLE
/****************************************************************************/

#define RTOS_TXT_LEN   (4*1024)
int_fast16_t CliRtosStat(CliParam_t p1, CliParam_t p2, CliParam_t p3)
{
  UNUSED(p1); UNUSED(p2); UNUSED(p3);
  int i, j, k;
	int h;

  h = (int)xPortGetFreeHeapSize();
  if (h<RTOS_TXT_LEN) {
    CliWriteLn("No room for text buffer");
    return CLI_RESULT_ERROR_UNDEFINED;
  }
  RTOS_CHAR_SIGN * dbgBuffer = RTOS_MEM_ALLOCATE(RTOS_TXT_LEN);

  CliWriteLine();

  CliWrite(
    "Task statistics:\n"
    "Task          state   prio.    stack    #\n");
  
//#warning "..."  
  vTaskList(dbgBuffer);
//  dbgBuffer[0] = 0;
  j = 0;
  for (i=0; i<strlen(dbgBuffer); i++) {
    if (dbgBuffer[i] != '\r') {
      dbgBuffer[j++] = dbgBuffer[i];      
    }
  }
  dbgBuffer[j] = dbgBuffer[i];      
  CliWrite((const char *)dbgBuffer);
  // Remove \n
  k = strlen(dbgBuffer);
  for (i=0, j=0; i<k; i++) {
    if (dbgBuffer[i] != '\r')
      dbgBuffer[j++] = dbgBuffer[i];
  }

#if ( configGENERATE_RUN_TIME_STATS == 1 )
  CliWrite(
    "\n"
    "Run-time statistics:\n"
    "  Task        abs. time       pct. time");
  vTaskGetRunTimeStats(dbgBuffer);
  CliWrite(dbgBuffer);
#endif
  CliPrintf("\nFree Heap: %d byte\n", h);
  CliWriteLine();

  RTOS_MEM_FREE(dbgBuffer);
  return CLI_RESULT_OK;
}

#endif  /*  */
