/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef HTTP_REST_H
#define HTTP_REST_H

#include <stdint.h>
#include <stdbool.h>
#include "plf/trc/trc.h"

#define HTTP_HOST   "iot.flow-loop.com"
#define HTTP_KEY    "5U3UHHgBHtPyaY9h"

/****************************************************************************/

#define TIME_SOURCE_HTTP_PORT   80

#define TD_OK             0
#define TD_ERR_CONNECT   -1   /**< Could not connect to the network and join the web server. */
#define TD_ERR_HTTP      -2   /**< Could not parse the time and date from the web server response. */
#define TD_ERR_RTC       -3   /**< Could not set the RTC. */
#define TD_ERR_TLS_CERT  -4   /**< The server certificate verification failed. Applicable only when force_apply is false. */

extern int RestSetRTCTimeDateFromNetwork(void);

/****************************************************************************/

#define GET_POST_HTTP_PORT   3002

#define R_OK             0
#define R_ERR_CONNECT   -1   /**< Could not connect to the network and join the web server. */
#define R_ERR_DNS       -2  /* other error...? */
#define R_ERR_SOCKET    -3  /* other error...? */
#define R_ERR_DATA      -4  /* other error...? */
#define R_ERR_SEND      -5
#define R_ERR_SEND_DATA -6
#define R_ERR_RECV      -7
#define R_ERR_HEAP      -8
#define R_ERR_FLASH_ERASE  -9
#define R_ERR_FLASH_WRITE  -10
#define R_ERR_FLASH_READ    -11


// If res > 0 res == receivede data length
// else error code

typedef void (*RestGetAsyncCB_t)(int size);
extern int RestGet(const char * fileName, uint32_t flashAddr, int32_t flashSizeMax, RestGetAsyncCB_t cb);

extern int RestPost(const char * fileName, uint32_t fileAddr, int32_t fileSize);

/****************************************************************************/

#ifdef TRC_ENABLE

extern int_fast16_t TestHGet(CliParam_t name, CliParam_t addr, CliParam_t maxLen);
extern int_fast16_t TestHPost(CliParam_t name, CliParam_t addr, CliParam_t len);

#endif

/****************************************************************************/


#endif /* HTTP_REST_H */
