/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef CLOUD_DEF_H
#define CLOUD_DEF_H

#ifdef __cplusplus
extern "C" {
#endif


#include <stdlib.h.>

#include "plf/plf.h"

/******************************************************************************/
/* SYSTEM - APPLICATION */

typedef enum {
  ITF_ETA,
  ITF_LTA,
  ITF_MTA,
  ITF_NTA,
  ITF_TTA,
  ITF_WTA,
  ITF_Last
}                       IoT_FactoryNetwork_t;

typedef char            ItfText_t[4];

/* MTA */
#define NWK_MTA                       "MTA"
#define MTA_GATEWAY_MAX               (5)
#define MTA_THINGS_MAX                (0) // except GW's own!
// ADC, CNT and TTL shares same input !!!
#define MTA_THINGS_INPUT_COUNTER      (5) 
#define MTA_THINGS_INPUT_ADCS         (5)
#define MTA_THINGS_INPUT_ITEMS        (MTA_THINGS_INPUT_ADCS) 
#define MTA_THINGS_OUTPUT_TTL_ITEMS   (2)
#define MTA_THINGS_OUTPUT_RELAY_ITEMS (3)
#define MTA_THINGS_OUTPUT_PWM_ITEMS   (2)
#define MTA_THINGS_OUTPUT_DAC_ITEMS   (0)
#define MTA_THINGS_OUTPUT_ITEMS       (MTA_THINGS_OUTPUT_TTL_ITEMS + MTA_THINGS_OUTPUT_RELAY_ITEMS + MTA_THINGS_OUTPUT_PWM_ITEMS + MTA_THINGS_OUTPUT_DAC_ITEMS)
#define MTA_THINGS_APP_ITEMS        2
#define MTA_ITEMS                     (MTA_THINGS_INPUT_ITEMS + MTA_THINGS_OUTPUT_ITEMS + MTA_THINGS_APP_ITEMS)

#define NWK                       "/" NWK_MTA
#define TP_DEVICE_MODEL           "MT1"
#define GATEWAY_MAX               MTA_GATEWAY_MAX
#define THINGS_MAX                MTA_THINGS_MAX
#define THINGS_INPUT_COUNTER      MTA_THINGS_INPUT_COUNTER
#define THINGS_INPUT_ITEMS        MTA_THINGS_INPUT_ITEMS
#define THINGS_OUTPUT_TTL_ITEMS   MTA_THINGS_OUTPUT_TTL_ITEMS
#define THINGS_OUTPUT_RELAY_ITEMS MTA_THINGS_OUTPUT_RELAY_ITEMS
#define THINGS_OUTPUT_PWM_ITEMS   MTA_THINGS_OUTPUT_PWM_ITEMS
#define THINGS_OUTPUT_DAC_ITEMS   MTA_THINGS_OUTPUT_DAC_ITEMS
#define THINGS_APP_ITEMS          MTA_THINGS_APP_ITEMS

#define THINGS_OUTPUT_TTL_FIRST   (THINGS_INPUT_ITEMS)
#define THINGS_OUTPUT_RELAY_FIRST (THINGS_OUTPUT_TTL_FIRST + THINGS_OUTPUT_TTL_ITEMS)
#define THINGS_OUTPUT_PWM_FIRST   (THINGS_OUTPUT_RELAY_FIRST + THINGS_OUTPUT_RELAY_ITEMS)
#define THINGS_OUTPUT_DAC_FIRST   (THINGS_OUTPUT_PWM_FIRST + THINGS_OUTPUT_PWM_ITEMS)
#define THINGS_OUTPUT_ITEMS       (THINGS_OUTPUT_TTL_ITEMS + THINGS_OUTPUT_RELAY_ITEMS + THINGS_OUTPUT_PWM_ITEMS + THINGS_OUTPUT_DAC_ITEMS)

#define THINGS_APP_FIRST          (THINGS_INPUT_ITEMS+THINGS_OUTPUT_ITEMS)
#define THINGS_ITEMS              (THINGS_APP_FIRST+THINGS_APP_ITEMS)

/******************************************************************************/
/* MQTT */

// logon-defauelt
#define MQTT_URL        "iot.flow-loop.com"
#define MQTT_PORT       1883
#define MQTT_USER       "fliot"
#define MQTT_PW         "U7Z8Snj7PacSnQQw"

// Topic
#define CLOUD_ROOT      "FLP"
#define CLOUD_APPL      "/TST"

#define CLOUD_GW_MTA    "/MTA" //  LTE Cat-M Things App

typedef char MqttUrl_t[64 + 1];

typedef char MqttUser_t[32 + 1];
typedef char MqttPassword_t[32 + 1];

typedef char MqttClientId_t[30 + 1];

typedef enum {
  TC_BCA,
  TC_CTL,
  TC_DFU,
  TC_Last
} TopicCmd_t;

#define TOPIC_SUBJECT_BCA "BCA" //  Broadcast
#define TOPIC_SUBJECT_DFU "DFU" //  DFU
#define TOPIC_SUBJECT_CTL "CTL" //  Control Request

typedef char MqttTopic_t[60 + 1];

typedef char MqttMessage_t[1000 + 1];

/******************************************************************************/
/* JSON */

#define JSON_STRING_LENGTH_MAX (1000)
typedef char JsonString_t[JSON_STRING_LENGTH_MAX];


// Commands...
typedef enum {
  JC_NONE,
  JC_REGISTRATION, //
  JC_CONFIG,       //
  JC_STATUS,       //

  JC_EVENT,        //
  JC_CONTROL,      //

  JC_DFU,          //
  JC_RESTART,      //
  JC_REREGISTER,   // Request Things to publish registration and configutation

  JC_Last
} JsonCmd_t;

extern const char * jsonCmdTexts[JC_Last];
extern JsonCmd_t TextToCmd(const char * cmd);

#define ITF_CMD_REG    "reg"    // jsonCmdTexts[JC_REGISTRATION]
#define ITF_CMD_CFG    "cfg"    // jsonCmdTexts[JC_CONFIG]
#define ITF_CMD_STA    "sta"    // jsonCmdTexts[JC_STATUS]

#define ITF_CMD_EVE    "eve"    // jsonCmdTexts[JC_EVENT]
#define ITF_CMD_CTL    "ctl"

#define ITF_CMD_DFU    "dfu"
#define ITF_CMD_RST    "rst"
#define ITF_CMD_RRG    "rrg"


// Standard Tokens
#define ITF_CMD                   "cmd"  // == jsonCmdText[...]
#define ITF_UPTIME                "upt"

// CMD == REG
#define ITF_CMD_REG_THINGS_NAME   "tna"
#define ITF_THINGS_NAME_LENGTH    (32)
typedef wchar_t                   ItfThingsName_t[ITF_THINGS_NAME_LENGTH+1];

#define ITF_CMD_REG_MODEL         "mdl"
typedef char                      MdlText_t[31];

typedef enum {
  SA_InOut,
  SA_WeatherStation,
  SA_AirQuality,
  SA_Last
}                                 SoftwareApplication_t;
typedef char                      SwaText_t[16];
#define ITF_CMD_REG_SW_APP        "swa"

#define ITF_CMD_REG_MCUID         "mid"   // hex string 96 bit Unigue MCU ID
typedef char                      MidText_t[8+1+8+1+8+1];

#define ITF_CMD_REG_SW_REV        "swr"   // (maj << 16) | (min << 8) | rev
#define ITF_CMD_REG_HW_REV        "hwr"   // 1..N

#define ITF_CMD_REG_EUI           "eui"   // local network ID
#define EUI64_SIZE                ( 8 )
#define EUI64_STRING_SIZE         ( EUI64_SIZE * 2 +1 )
typedef char                      EuiText_t[EUI64_STRING_SIZE];

// CMD == CFG
#define ITF_CMD_CFG_ITEM_NO       "i%u"
#define ITF_CMD_CFG_GW_NO         "gno"
#define ITF_CMD_CFG_THINGS_NAME   ITF_CMD_REG_THINGS_NAME
#define ITF_CMD_CFG_THINGS_NO     "tno"
#define ITF_CMD_CFG_TYP           "ty"
#define ITF_CMD_CFG_HYS           "hy"
#define ITF_CMD_CFG_ON            "on"
#define ITF_CMD_CFG_ST            "st"    // Sample Time

// CMD = EVE
#define ITF_CMD_EVE_ITEM          "evi"
#define ITF_CMD_EVE_DATA          "dat"

// CMD = STA
#define ITF_CMD_STA_ITEM          "isa"  // Item Static Array

// CMD == DFU
#define ITF_CMD_DFU_FEX           "fex"
#define ITF_CMD_DFU_FEX_ID        "id"
#define ITF_CMD_DFU_FEX_VMA       "vma"
#define ITF_CMD_DFU_FEX_VMI       "vmi"
#define ITF_CMD_DFU_FEX_CRC       "crc"

// -> HTTP GET
//#define ITF_CMD_DFU_IH            "ih"
//#define ITF_CMD_DFU_IH_PREPARE    "prepare"  // _ok, __failed
//#define ITF_CMD_DFU_IH_EXECUTE    "execute"  // _ok, _failed
//#define ITF_CMD_DFU_IH_DATA       ":xxxx"

//extern JsonCmd_t TextToCmd(const char * cmd);

/******************************************************************************/
/* ITEMS */

/* Items with number is application-specific, and more may come... */
typedef enum {
  IT_NONE,

  // INPUT
  IT_TEMPERATURE, //  
  IT_PRESSURE,    
  IT_FLOW,          
  
  // https://www.sparkfun.com/products/15901
  IT_RAIN_GAUGE_WS,  // Measures rainfall. Tipping Bucket Rain Gauge - for each 0.011" (0.2794 mm) of rain
  IT_ANEMOMETER_WS,  // Wind Speed
  IT_WIND_VANE_WS,   // Wind Vane  

  // Atmusphere
  IT_ATM_GAS,
  IT_ATM_PRES,
  IT_ATM_TEMP,
  IT_ATM_HUMIDITY,
  
  IT_SWITCH,      // Input Level
  IT_COUNTER,     // Counting, pulses/second ??
  IT_VOLTAGE,     // ADC Voltage 0.3v3
  IT_RAW,         //  Raw ADC value
  IT_INPUT_Last = IT_RAW,

  // OUTPUT
  IT_OUTPUT_FIRST,
  IT_RELAY = IT_OUTPUT_FIRST,
  IT_TTL,
  IT_PWM,
  IT_DAC,
  IT_OUTPUT_Last = IT_DAC,

  IT_Last
} ItemType_t;


#define MAX_IO_TYPE_TEXT_SIZE (15)
//typedef  char ItemTypeText_t[MAX_IO_TYPE_TEXT_SIZE + 1];
extern ItemType_t ItemsTextToItemType(const char * type);


/******************************************************************************/
/* Weather Station */

#define WS_IT_TEMPERATURE   0
#define WS_IT_RAIN_GAUGE    1
#define WS_IT_ANEMOMETER    2
#define WS_IT_WIND_VANE     3

typedef enum {
  WWD_N, WWD_NNE, WWD_NE, WWD_ENE, 
  WWD_E, WWD_ESE, WWD_SE, WWD_SSE,
  WWD_S, WWD_SSW, WWD_SW, WWD_WSW,
  WWD_W, WWD_WNW, WWD_NW, WWD_NNW ,
  WWD_Last
}                   WindVaneDir_t;


/******************************************************************************/

#define CLOUD_MAX_ERROR_TEXT_SIZE (32)
typedef char ErrorText_t[CLOUD_MAX_ERROR_TEXT_SIZE + 1];

/******************************************************************************/

#if (PLF_OS == PLF_OS_WINDOWS)

typedef struct {
  // config
  bool          enabled;
  ItemType_t    type;
  uint32_t      hys;
  uint32_t      samplingTime;
  int64_t       value;
}               Item_t;

#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif // CLOUD_DEF_H
