/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_KEY_CFG_H
#define PLF_KEY_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include "plf/plf.h"

/*******************************************************************************/
  
typedef enum {
  KEY_UI_BTN_1,
#if !defined(EVK)  
  KEY_UI_BTN_2,
#endif  

  KEY_Last
} 				        Key_t;

typedef uint32_t   KeySet_t;


/*******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* PLF_KEY_H */
