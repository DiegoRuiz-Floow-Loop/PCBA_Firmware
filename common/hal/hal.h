/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_HAL_H
#define PLF_HAL_H

#ifdef __cplusplus
extern "C" {
#endif

#if defined(STM32L010xB) || defined(STM32L071xx)
  #include "stm32l0xx_hal.h"
#elif defined(STM32G070xx)
  #include "stm32g0xx_hal.h"
#elif defined(STM32L475xx) || defined(STM32L431xx) || defined(STM32L471xx)    
  #include "stm32l4xx_hal.h"
#elif defined(STM32F767xx)
  #include "stm32f7xx_hal.h"
#elif defined(STM32WL55xx)
  #include "stm32wlxx_hal.h"
#endif


#include "plf/plf.h"

#if !defined(__SES_ARM)
#include "plf/cli/cli.h"
#endif

/******************************************************************************/

extern volatile PlfTime_t halTime0;
extern void HalTimeMsGet(PlfTime_t * pTime);

/******************************************************************************/

typedef union {
  uint32_t              id[3];
  uint8_t               idb[12];
}                       Uid_t;

extern void HalUidGet(Uid_t * uid);

typedef char            UidText_t[27];
extern UidText_t        uidText;
extern char * HalUidTextGet(UidText_t uidText);



/******************************************************************************/
/* INTERRUPT */

typedef uint32_t        HalIrqStat_t;

extern void HalInterruptEnable(HalIrqStat_t * oldState);
extern void HalInterruptDisable(HalIrqStat_t * oldState);
extern void HalInterruptRestore(HalIrqStat_t oldState);
  
#define HAL_START_ATOMIC() \
  HalIrqStat_t oldState;\
  HalInterruptDisable(&oldState)
#define HAL_END_ATOMIC()   \
  HalInterruptRestore(oldState)

#define IS_RUNNING_MAIN()     ((SCB->ICSR & SCB_ICSR_VECTACTIVE_Msk)==0)
#define IS_RUNNING_ISR()      ((SCB->ICSR & SCB_ICSR_VECTACTIVE_Msk)!=0)


extern void HalExintHandler(uint_fast16_t xi);

extern void HalReboot(void);


/******************************************************************************/
/* KEY+SW */

extern void HalCfgGet(uint8_t * rev);

extern int HalHwRevGet(void);

extern void HalReadButtons(uint8_t * keys);

/******************************************************************************/

extern void HalCounterGet(uint32_t * ui32, int ci);
extern void HalCounterGetAndClear(uint32_t * u32, int ci) ;

/******************************************************************************/
/* Common */

extern void HalDelayUs(uint32_t n);

#ifdef CLI_ENABLE
  extern int_fast16_t CliReboot(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

extern uint32_t  HAL_GetTick(void); /* In Cube */

extern void HalReboot(void);

extern void HalInit(void);

/******************************************************************************/

#if (PLF_OS == PLF_OS_WINDOWS) /* see project file */

//extern uint16_t HalReadButtons(void);

typedef void * xHWND;
extern void HalSetWndHandle(xHWND handle);
extern xHWND HalGetWndHandle(void);

#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif
#endif // HAL_H
