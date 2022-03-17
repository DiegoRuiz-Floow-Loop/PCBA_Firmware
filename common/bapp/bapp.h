/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef BAPP_H
#define BAPP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plf/plf.h"

/******************************************************************************/

#define FIRMWARE_STATUS_OK 				  (0)
#define FIRMWARE_STATUS_BAD_ID      (-1)
#define FIRMWARE_STATUS_BAD_CRC 	  (-2)

extern void BAppExecuteFirmware(void);
extern bool DownloadAndActivate(void);
extern bool Download(void);
extern bool DoPFlashProgram(void);

/******************************************************************************/

extern int8_t FirmwareCheck(void);

/******************************************************************************/

extern void BAppGotSioCommand(char * cmd);

extern void BAppMenu(void);
  
extern void BAPP_Init(void);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif

