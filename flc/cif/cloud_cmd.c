/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <string.h>
#include <ctype.h>

#include "hal/hal.h"
#include "hal/wdt/wdt.h"
#include "hal/dflash/dflash.h"

#include "plf/plf.h"
#include "plf/json/jsonx.h"
#include "plf/blc/blc.h"

#include "net/rest/rest.h"
#include "cif/cloud.h"
#include "cif/cloud_common.h"

#include "modem.h"
#include "modem_mqtt.h"

#include "sif/sif.h"
#include "app/app.h"

/******************************************************************************/

static void AppGotJsonControl(int things, json_t * root)
{
  bool ok = true;
  json_t * tag;
  ErrorText_t error = "";
  json_int_t item;
  json_int_t data;

  //TRACE(TRC_TA_CLD, TRC_TL_3, "Got JSON CONTROL cmd");
  
  tag = json_object_get(root, ITF_CMD_EVE_ITEM);
  if (tag) {
    if (json_is_integer(tag)) {
      item = json_integer_value(tag);
    } else {
      ok = false;
    }
  } else {
    ok = false;
  }
    
  if ((item <= THINGS_INPUT_ITEMS) || (item > THINGS_ITEMS)) {
    ok = false;
  }
  
  tag = json_object_get(root, ITF_CMD_EVE_DATA);
  if (tag) {
    if (json_is_integer(tag)) {
      data = json_integer_value(tag);
    } else {
      ok = false;
    }
  } else {
    ok = false;
  }
  if (ok) {
    if (item <= THINGS_ITEMS-THINGS_APP_ITEMS) {
      ItemControl(things, item-1, data, true, error);
    } else {
      AppItemChanged(things, item-1, data);
    }
  }
}

/****************************************************************************/

static bool CfgAckSend(int things, int item)
{
  bool res = true;
	json_t * root = json_object();
  
	json_object_set_new(root, ITF_CMD, json_string(jsonCmdTexts[JC_CONFIG]));
  
  AddConfigItem(things, root, item);

  char * jsz = json_dumps(root, JSON_COMPACT);
  json_delete(root);

  MqttTopicMake(app.no, things, TOPIC_SUBJECT_BCA);
  strcpy(message, jsz);
	sprintf(&message[strlen(message)], "%04X", CalcCrc16(strlen(message), (uint8_t *)message, CRC16_INITIAL_SEED));
	if (!ModemMqttPublish(things, topic, message)) {
    res = false;
	}
  HeapxFree(jsz);
  return res;
}

static void AppGotJsonConfigSetup(int things, json_t * root)
{
  json_t * tag;
  json_t * item;

  json_int_t no;
  const char * szType = NULL;
  bool ena;
  ItemType_t typ;
  bool ok;
  char sz[20];
  int i;

  ok = true;
  
  // GW Config ...  
  tag = json_object_get(root, ITF_CMD_CFG_GW_NO);
  if (tag) {
    if (json_is_integer(tag)) {
      no = json_integer_value(tag);
      (void)AppThingsNoSet(no);       
    }
  }

  tag = json_object_get(root, ITF_CMD_CFG_THINGS_NAME);
  if (tag) {
    if (json_is_array(tag)) {
      int s = MIN_VAL(json_array_size(tag), ITF_THINGS_NAME_LENGTH);
      for(i = 0; i < s; i++)
        app.name[i] = json_integer_value(json_array_get(tag, i));
      app.name[i] = 0;
    
    }
  }

  // Item Config ...
  no = 0;
  for (i=1; i<=THINGS_INPUT_ITEMS; i++) {
    sprintf(sz, ITF_CMD_CFG_ITEM_NO, i);
    item = json_object_get(root, sz);
    if (item) {
      no = i;
      break;
    }
  }
 
  if ((no < 1) || (no > THINGS_ITEMS)) {
    // return;
  } else {

    tag = json_object_get(item, ITF_CMD_CFG_TYP);
    if (tag) {
      if (json_is_string(tag)) {
        szType = json_string_value(tag);
      } else {
        ok = false;
      }
    } else {
      ok = false;
    }
    if (ok) {
      typ = ItemsTextToItemType(szType);    
      if (no > THINGS_INPUT_ITEMS) {
        if (typ != IT_RELAY) {
          return;
        }
      }
    } else {
      return;
    }
      
    tag = json_object_get(item, ITF_CMD_CFG_ON);
    if (tag) {
      if (json_is_boolean(tag)) {
        ena = json_boolean_value(tag);
      } else {
        ok = false;
      }
    } 
    if (!ok) {
      return;
    }
    
    int32_t hysteresis = -1;
    tag = json_object_get(item, ITF_CMD_CFG_HYS);
    if (tag) {
      if (json_is_integer(tag)) {
        hysteresis = json_integer_value(tag);
      } else {
        return;
      }
    }     
     
    int32_t samplingTime = -1;
    tag = json_object_get(item, ITF_CMD_CFG_ST);
    if (tag) {
      if (json_is_integer(tag)) {
        samplingTime = json_integer_value(tag);
      } else {
        return;
      }
    }     

    ItemDefine(things, no-1, typ,  ena, hysteresis, samplingTime);
    CfgAckSend(things, no-1);
    app.crc = CalcCrc16(sizeof(app)-sizeof(Crc16_t), (uint8_t*)&app, CRC16_INITIAL_SEED);
    #if (NVM_ACCESS==NVM_ACCESS_ADDRESS)
      NvmWrite(offsetof(NvmStruct_t, appl.app), (uint8_t *)&app, sizeof(app));
    #else
      NvmWrite(NVM_ID_APP, app);
    #endif

  }
  
}

/****************************************************************************/

#pragma pack(push, 1)
typedef struct {
	unsigned char         head;
	unsigned char         chLen[2];
	unsigned char         chAddr[4];
	unsigned char         chRecId[2];
	union {
		struct {
			unsigned char     chOffset[4];
		}                   ihr_04;
		struct {
			unsigned char     chData[2*256];
		}                   ihr_00;
	}                     u;
}                       ih_t;
#pragma pack(pop)


#ifdef BLC_H

static bool dfuRunning = false;

static void RestGetAsyncCB(int len)
{
//  if (len != APP_PROGRAM_FLASH_SIZE) {
//    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Get DFU file failed");
//  } else {
    TRACE(TRC_TA_CLD, TRC_TL_4, "Checking DFU file");
    FWDescriptor_t fwd;
    #if defined(USE_FWD_BC_MAKE_DFX_NEW)
      FwdBootCommand_t fbc = BlcCheckNewDfuDfxImage(DFLASH_DFU_START, &fwd);
    #else
      FwdBootCommand_t fbc = BlcCheckDataFlashFirmware(DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, &fwd);
    #endif
    if (fbc != FWD_BC_UNKNOWN) {
      TRACE(TRC_TA_CLD, TRC_TL_4, "DFU Image OK - activating");
      SifTxFlush();
      BlcActivateNewImage(DFLASH_DFU_START, fbc);    
    } else {
      TRACE(TRC_TA_CLD, TRC_TL_ERROR, "DFU file not OK");
    }
//  }
  xModemMqttLocked = false;
  if (!ModemMqttBrokerConnect(MQTT_URL, MQTT_PORT, mqttClientId, MQTT_USER, MQTT_PW)) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Rejected: ModemMqttBrokerConnect()");
  }
  AppTaskSuspend(0);  
  dfuRunning = false;
}

#endif


#ifdef BLC_H

static void AppGotJsonDfuFile(const char * fname)
{
  if (!dfuRunning) {
    dfuRunning = true;
    TRACE_VA(TRC_TA_CLD, TRC_TL_FATAL, "Executing DFU (%s)", fname);
    TstIOStop(0,0,0);
    
    static int things = 0;
    AppTaskSuspend(10*1000) ;
    if (!ModemMqttBrokerDisconnect()) {
      TRACE(TRC_TA_CLD, TRC_TL_FATAL, "Rejected: ModemMqttBrokerDisconnect()");
      AppTaskSuspend(0) ;
      dfuRunning = false;
      return ;
    }  
    ats[things].appState = AS_DO_REGISTER;
    
    if (0 != RestGet(fname, DFLASH_DFU_START, APP_PROGRAM_FLASH_SIZE, RestGetAsyncCB)) {
      TRACE_VA(TRC_TA_CLD, TRC_TL_FATAL,  "DFU Download File \"%s\" failed", fname);
      xModemMqttLocked = false;
      if (!ModemMqttBrokerConnect(MQTT_URL, MQTT_PORT, mqttClientId, MQTT_USER, MQTT_PW)) {
        TRACE(TRC_TA_CLD, TRC_TL_FATAL, "Rejected: ModemMqttBrokerConnect()");
        AppTaskSuspend(0) ;
        dfuRunning = false;
      }
    } else {
      TRACE_VA(TRC_TA_CLD, TRC_TL_4,  "DFU Download File: \"%s\" started ...", fname);
    }
  }
}

#endif

/****************************************************************************/

JsonCmd_t TextToCmd(const char * cmd) 
{
  int i;
  for (i=0; i<JC_Last; i++) {
    if (strcmp(jsonCmdTexts[i], cmd)==0) {
      return (JsonCmd_t)i;
    }
  }
  return JC_Last; // Not Found
}


/****************************************************************************/

void CloudGotMqttData(
  MqttTopic_t   topic, 
  MqttMessage_t msg)
{  
  int gw, things;
  json_t * root;
  json_t * tag;
  json_error_t error;
//  yourSeq = -1;

  JsonCmd_t cmd = JC_Last;

  HeapxClean();
  char * s1;
  char * s2;
  char * s3;
//  char * s4;
  //char * s5;
  
  s1 = &topic[8];     // xTA
  s2 = &s1[4];        // 
  gw = 100*(s2[0] - '0') + 10*(s2[1] - '0') + 1*(s2[2] - '0');
  if (gw == 999) 
    gw = app.no;
  if (gw != app.no)
    return;

  s3 = &s2[4];
  things = 100*(s3[0] - '0') + 10*(s3[1] - '0') + 1*(s3[2] - '0');

  if (things == 999)
    things = 0;
  
  // "ITF/D20/MTA/..."
  if (StrGE(s1, "MTA/")) { // check/remove CRC
    uint32_t crc, c2;
    int i = strlen(msg)-4;
    sscanf(&msg[i], "%04X", &crc);
    msg[i] = 0;
    c2 = CalcCrc16(i, (uint8_t *)msg, CRC16_INITIAL_SEED);
    if (crc != c2) {
      TRACE_VA(TRC_TA_CLD, TRC_TL_ERROR, "MTA CRC error, l:%u,cm:%04X, cc:%04X", i, crc, c2);
      return;
    }
  }
  
  TRACE_VA(TRC_TA_CLD, TRC_TL_4, "RSV: %s | %s", topic, msg);

  // Convert string to JSON object
  root = json_loads(msg, 0, &error);
  if(!root) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "Not possible to load JSON string");
    return;
  }

  tag = json_object_get(root, ITF_CMD);
  if (tag) {
    if (json_is_string(tag)) {
      cmd = TextToCmd(json_string_value(tag));
    }
  }
  if (cmd == JC_Last) {
    TRACE(TRC_TA_CLD, TRC_TL_ERROR, "cmd wrong");
    #if !defined(NTA) && !defined(LTA)
      json_delete(root);
    #endif  
    return;
  }
    
  /* COMMAND(seq) or RESPONS(ack) */
//  tag = json_object_get(root, ITF_SEQ);
//  if (tag) {
//    if (json_is_integer(tag)) {
//      yourSeq = json_integer_value(tag);
//    } else {
//      TRACE(TRC_TA_CLD, TRC_TL_ERROR, "seq wrong");
//      #if !defined(NTA)  && !defined(LTA)
//        json_delete(root);
//      #endif  
//      return;
//    }
//  } else {
//  }

  switch (cmd) {
    case JC_CONFIG:
     AppGotJsonConfigSetup(things, root);
      break;    
    case JC_CONTROL:
      AppGotJsonControl(things, root);
      break;
    case JC_DFU: 
      // check command, do we need to update ??
      tag = json_object_get(root, ITF_CMD_DFU_FEX);
      if (json_is_string(tag)) {
#ifdef BLC_H
        AppGotJsonDfuFile(json_string_value(tag));
#else
        return;
#endif
      } else {
        return;
      }
      break;
    case JC_REREGISTER:
      AppStartRegistration(things);    
      break;
    case JC_RESTART:
      if (gw == app.no) {
#ifdef BLC_H
        BlcReboot();
#else
        HalReboot();
#endif
      }
      break;
    case JC_STATUS:
      break;
    default:
      break;    
  }
//  AppCloudGotSubCommand(cmd);
}

