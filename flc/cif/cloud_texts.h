/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef CLOUD_TEXT_H
#define CLOUD_TEXT_H

#ifdef __cplusplus
extern "C" {
#endif


#include "cif/cloud_def.h"

/******************************************************************************/

extern const char *  lstItfNwm[ITF_Last];

extern const char *  lstSW_App[SA_Last];

typedef char ItemName_t[15];
typedef char ItemDimension_t[8];

typedef struct {
  ItemName_t          name;
  ItemDimension_t     dim;
}                     ItemTypeText_t;

extern const ItemTypeText_t itemTypeTexts[IT_Last + 1];

typedef char WindVaneDirText_t[10];
extern const WindVaneDirText_t windVaneDirText[WWD_Last];

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // CLOUD_DEF_H
