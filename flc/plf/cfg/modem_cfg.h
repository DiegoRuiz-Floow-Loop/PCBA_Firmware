/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef MODEM_CFG_H
#define MODEM_CFG_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************/

#define MODEM_AT_TX_BUFFER_SIZE   (2048)  // Doubled size to accommodate big JSON publish of raw data
#define MODEM_AT_RX_BUFFER_SIZE   (1024)

// Modem feature support
#define MODEM_NTP_SYNCHRONIZE

#define MODEM_FOTA_SUPPORT
#define MODEM_FOTA_DEFAULT_URL    "http://portal.iot-factory.dk:3002/download?file=update.zip&key=TekPartner"

// Modem protocol support
#define MODEM_TCP_SUPPORT

#define MODEM_MQTT_SUPPORT
#define MODEM_MQTT_CONNECTED_POLL (5 * 60 * 1000)

// Modem settings
#define MODEM_POWER_START_DELAY   (1000)

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
