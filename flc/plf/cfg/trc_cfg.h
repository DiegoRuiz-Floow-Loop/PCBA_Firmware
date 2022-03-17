/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef TRC_CFG_H
#define TRC_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

	
#include "plf/plf.h"
	
/******************************************************************************/

/* define/undefine as needed  */
#if !defined(BOOTLOADER)
#define TRC_ENABLE
#endif


/* application specific costumation */

/**
 * Enumeration of the individual trace areas, which are corresponding to
 * whatever abstraction level is appropriate (Components/Domain/Module).
 */
typedef enum {
  TRC_TA_First,
  TRC_TA_APP = TRC_TA_First,
  TRC_TA_SHC,
  TRC_TA_CIF,
  TRC_TA_SUP,  
  TRC_TA_SIF,
  TRC_TA_MWR,
  TRC_TA_PLF,
  TRC_TA_HAL,
  TRC_TA_Last
}                       TraceArea_t;

typedef enum {
  TRC_TL_First,
  TRC_TL_COMPONENT = TRC_TL_First,     /* top level component interfaces - XXX_Init... */
  TRC_TL_2,
  TRC_TL_3,
  TRC_TL_4,
  TRC_TL_5,
  TRC_TL_ERROR,
  TRC_TL_BLOCK,
  TRC_TL_FATAL,
  TRC_TL_Last
}                       TraceLevel_t;

/******************************************************************************/
#ifdef TRC_ENABLE
/******************************************************************************/

#define TRC_AREA_TEXT_0     "app"
#define TRC_AREA_TEXT_1     "shc"
#define TRC_AREA_TEXT_2     "cif"
#define TRC_AREA_TEXT_3     "sup"
#define TRC_AREA_TEXT_4     "sif"
#define TRC_AREA_TEXT_5     "mwr"
#define TRC_AREA_TEXT_6     "plf"
#define TRC_AREA_TEXT_7     "hal"

#define TRC_LEVEL_TEXT_0    "cmp"
#define TRC_LEVEL_TEXT_1    "tl2"
#define TRC_LEVEL_TEXT_2    "tl3"
#define TRC_LEVEL_TEXT_3    "tl4"
#define TRC_LEVEL_TEXT_4    "tl5"
#define TRC_LEVEL_TEXT_5    "err"
#define TRC_LEVEL_TEXT_6    "blk"
#define TRC_LEVEL_TEXT_7    "fat"

#define TRC_CFG_LINE_LENGTH  (80)   /**< Maximum length of a trace output line. */

/******************************************************************************/
#endif
/******************************************************************************/

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* #ifndef TRC_CFG_H */
