/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef CLOUD_H
#define CLOUD_H

#ifdef __cplusplus
extern "C" {
#endif


#include "plf/plf.h"
#include "plf/nvm/nvm.h"
#include "plf/evos/evos.h"
#include "plf/cli/cli.h"

#include "cif/cloud_common.h"

  
/******************************************************************************/

typedef enum {
  CTS_CONNECT,
  CTS_CLOUD_READY,
} CT_StepEvent_t;

extern EvosEventHandle_t  cloudTask;

extern uint8_t            cloudStep;

extern void CloudeConnect(void);


extern void CloudGotMqttData(
  MqttTopic_t     topic, 
  MqttMessage_t   msg);

extern void CLOUD_Init(void);

#ifdef CLI_ENABLE 
CLI_DECLARE_SUBTEST(mqtt)
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif
#endif
