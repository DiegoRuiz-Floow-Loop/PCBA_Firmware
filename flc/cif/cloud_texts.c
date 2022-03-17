/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "cif/cloud_texts.h"

/******************************************************************************/

const char * lstItfNwm[ITF_Last] = {
  NWK_MTA,
  NWK_MTA,  
  NWK_MTA,
  NWK_MTA,
  NWK_MTA,
  NWK_MTA,  
};

/******************************************************************************/

const char * jsonCmdTexts[JC_Last] = {
  "",
  ITF_CMD_REG,
  ITF_CMD_CFG,
  ITF_CMD_STA,
  
  ITF_CMD_EVE,
  ITF_CMD_CTL,
  
  ITF_CMD_DFU,
  ITF_CMD_RST,
  ITF_CMD_RRG,
};

/******************************************************************************/

const char * lstSW_App[SA_Last] = {
  "I/O Things",
  "Weather Things",
  "Air Quality Things",
};

/******************************************************************************/

const ItemTypeText_t itemTypeTexts[IT_Last + 1] = {
  {"(none)",        ""},
  // INPUT
  // Single-sensors
  {"Temperature",     "C"},         // adc
  {"Pressure",        "Bar"},       // adc
  {"Flow",            "l/min"},     // counter
  // Weather Station
  {"WS Rainfall",     "mm/24h"},    // counter
  {"WS Wind Speed",   "m/s"},       // counter
  {"WS Wind Vane",    ""},          // adc
  // Environment Station - BME680 IS, gas, pressure, temperature & humidity sensor
  {"ES Gas",          "Ohm"},         // i2c
  {"ES Pressure",     "hPa"},       // i2c
  {"ES Temp",         "C"},         // i2c
  {"ES Humidity",     "%"},         // i2c
  // I/O 
  {"Switch",          "on/off"},    // 0|1    (di)
  {"Counter",         "cnt"},       // 0..N   (irq)
  {"Voltage",         "V"},         // 0..3.3 (adc)
  {"Raw",             ""},          // 0..4095 (adc)
  // OUTPUT
  {"Relay",           "on/off"},    //
  {"TTL",             "on/off"},    //
  {"PWM",             "permill"},   //
  {"DAC",             "permill"},   //
  // end...
  {"", ""},
};

/******************************************************************************/

const WindVaneDirText_t windVaneDirText[WWD_Last] = {
  "N/0",
  "NNW/337.5",
  "NW/315",
  "WNW/292.5",
  "W/270",
  "WSW/247.5",
  "SW/225",
  "SSW/202.5",
  "S/180",
  "SSE/157.5",
  "SE/135",
  "ESE/112.5",
  "E/90",
  "ENE/67.5",
  "NE/45",
  "NNE/22.5",
};

/******************************************************************************/

