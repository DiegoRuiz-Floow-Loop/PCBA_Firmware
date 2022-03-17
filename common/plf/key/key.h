/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_KEY_H
#define PLF_KEY_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*******************************************************************************/

#include "plf/plf.h"
#include "plf/cfg/key_cfg.h"


  /*******************************************************************************/

/* Required call-back Interface */
extern void XKeyChanged(Key_t key, bool level);


  
/* Enable/Disable Key Scanner */
extern void KeyScanner(bool enabled);

/* Update key-state for mask. */
extern void KeyUpdate(KeySet_t mask);

/* Enable/Disable Key Scanner */
extern void KeyInit(void);


/*******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* PLF_KEY_H */
