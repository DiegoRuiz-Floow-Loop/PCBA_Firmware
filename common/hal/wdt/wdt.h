/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef WDT_H
#define WDT_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "plf/plf.h"

/****************************************************************************/

// define in project file !!
//#define HAL_WDT_ENABLE
  
extern void HalWdtFeed(void);


// Default: 
extern uint32_t HalWdtReActivate(uint32_t prescaler);

extern void HalWdtActivate(void);

/****************************************************************************/


#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* WDT_H */
