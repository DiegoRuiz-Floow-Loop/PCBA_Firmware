/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef PLF_COMMON_H
#define PLF_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif


#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <time.h>
#include <stdlib.h.>
#include <assert.h>

/*******************************************************************************
 *    OS Definitions
 ******************************************************************************/

#define PLF_OS_ANY                  1
#define PLF_OS_EVOS                 2
#define PLF_OS_RTOS                 3
#define PLF_OS_WINDOWS              4

// define PLF_OS=xxx in project
  
/*******************************************************************************
 *    Application Plf Definitions
 ******************************************************************************/

#define PLF_ANY                     1   /* Non IoT demo */
#define PLF_ROWLEY_STM32F10x_THING  2   /* Ga-Jol Device */
#define PLF_KEIL_GJ_GUI_THING       3   /* Ga-Jol GUI */
#define PLF_KEIL_GJ_ROBERT_THING    4   /* Ga-Jol Robert */
#define PLF_KEIL_LORA_GW            5   /* Lora GW */
#define PLF_KEIL_LORA_THING         6   /* Lora Thing */
#define PLF_KEIL_BLE_GW             7   /* nRF52 GW */
#define PLF_KEIL_BLE_THING          8   /* nRF52 Thing */
#define PLF_KEIL_ZIGBE_GW           9   /* Lora GW */
#define PLF_KEIL_ZIGBE_THING        10  /* Lora Thing */
#define PLF_BCPP_THING              99  /* Windows Bcc Thing */

// define PLF=xxx in project - if needed


/*******************************************************************************
 * Integer types.
 * These should _always_ be used instead of the built-in C types.
 ******************************************************************************/

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * UYtility Macrose
 ******************************************************************************/

#define _STRIZE(x)        _VAL(x)
#define _VAL(x)            #x

//#ifndef UNUSED
////#define UNUSED(x) ((void)(x))
//#define UNUSED(a)       (void)a          /**< Use to prevent compiler warnings about unused parameters. */
//#endif

#ifndef NULL
#define NULL            (void *)0         /**< General NULL pointer. */
#endif

#define MIN_VAL(a,b)    ((a) < (b) ? (a) : (b)) /**< Returns the smallest value of a and b. */
#define MAX_VAL(a,b)    ((a) > (b) ? (a) : (b)) /**< Returns the highest value of a and b. */

// Inversion ensures that zero is always 1 and non-zero is always 0
#define BOOL_EQUAL(a, b)      (!(a) == !(b))
#define BOOL_NOT_EQUAL(a, b)  (!(a) != !(b))

#define SIZEOF8(t)    ((uint8_t)sizeof(t))  /**< Returns the size of a variable as an unsigned 8-bit integer. */
#define SIZEOF16(t)   ((uint16_t)sizeof(t)) /**< Returns the size of a variable as an unsigned 16-bit integer. */
#define SIZEOF32(t)   ((uint32_t)sizeof(t)) /**< Returns the size of a variable as an unsigned 32-bit integer. */

#define SIZEOF_ARRAY(array) (sizeof(array) / sizeof((array)[0]))

/******************************************************************************/


#ifdef __cplusplus
}
#endif
#endif

