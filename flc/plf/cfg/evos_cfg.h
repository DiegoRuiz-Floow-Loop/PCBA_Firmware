/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef EVOS_CFG_H
#define EVOS_CFG_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"

/******************************************************************************/

#define EVOS_POOL_SIZE  (40)        /* Number of event instances in the global event pool. */

/******************************************************************************/

typedef uint8_t         EvosEventHandle_t;

#define EVOS_PARAM_USED  
typedef uint32_t        EvosEventParam_t;

typedef void (*EvosEventFunc_t)(EvosEventParam_t param);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif //EVOS_CFG_H
