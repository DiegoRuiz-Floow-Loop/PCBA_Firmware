/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_CLI_CFG_H
#define PLF_CLI_CFG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "plf/plf.h"
  
/*******************************************************************************/

#if (PLF_OS != PLF_OS_WINDOWS) /* see project file */
#if !defined(BOOTLOADER)

#define CLI_ENABLE

#endif
#endif

/*******************************************************************************/
  
/* Enable sw test/debug commands */
#define CLI_DEBUG_COMMANDS          /**< Persistent trace config between reboots. */
  
#define CLI_CMD_MAX_LENGTH    (6)s
#define CLI_MAX_PARAM_LENGTH  (120)
#define CLI_CFG_LEVEL         (5)   /**< Number of levels in debugging command hierarchy. */
#define CLI_CFG_HIST_SIZE     (3)   /**< History depth of previous commands issued from the console. */
#define CLI_CFG_LINE_LENGTH   (200) /**< Maximum length of an input line from the debug console. */

/******************************************************************************/

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* CLI_CFG_H */
