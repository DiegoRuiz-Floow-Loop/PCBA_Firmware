/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef XMODEM_H
#define XMODEM_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/******************************** INCLUDE FILES *******************************/
#include "plf/plf.h"


/*********************************** DEFINES **********************************/
#define XMODEM_BUF_SIZE   (1024)

/************************** INTERFACE DATA DEFINITIONS ************************/
typedef int (*XmodemCallback_t)(unsigned char *buf, int cnt);


/************************ INTERFACE FUNCTION PROTOTYPES **********************/

//int XmodemReceive(unsigned char *dest, int destsz);
int32_t XmodemReceive(XmodemCallback_t cb);
int32_t XmodemTransmit(unsigned char *src, int32_t srcsz);


/** @} */

#ifdef __cplusplus
}
#endif /* _cplusplus */

#endif /* XMODEM_H */
