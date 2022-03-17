/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef ADC_MUX_H
#define ADC_MUX_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>

/*******************************************************************************
 * Defines
 ******************************************************************************/

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
#if defined(EVK)
// Normaly open         // The EVK ADC DMA changes the order of inputs. Not fixed
  ADC_MUX_ITEM_VBAT_1,  // Placement is temporary fixed
  ADC_MUX_ITEM_3_1,     // Placement is temporary fixed
  ADC_MUX_ITEM_2_1,     // Placement is temporary fixed
  ADC_MUX_ITEM_1_1,     // Placement is temporary fixed
  ADC_MUX_ITEM_4_1,     // Placement is temporary fixed
  ADC_MUX_ITEM_5_1,     // Not temporary fixed
  ADC_MUX_ITEM_VEXT_1,  // Not temporary fixed
  ADC_MUX_ITEM_TEMP_1,  // Not temporary fixed
  
// Normaly closed       // The EVK ADC DMA changes the order of inputs. Not fixed
  ADC_MUX_ITEM_1_2,
  ADC_MUX_ITEM_2_2,
  ADC_MUX_ITEM_3_2,
  ADC_MUX_ITEM_4_2,
  ADC_MUX_ITEM_5_2,
  ADC_MUX_ITEM_VBAT_2,
  ADC_MUX_ITEM_VEXT_2,
  ADC_MUX_ITEM_TEMP_2,
#else
// Normaly open
  ADC_MUX_VOLT_24V,
  ADC_MUX_VOLT_5V0,
  ADC_MUX_PUMP_CUR1,
  ADC_MUX_PUMP_CUR2,
  ADC_MUX_PUMP_CUR3,
  ADC_MUX_SENSOR1,
  ADC_MUX_SENSOR2,
  ADC_MUX_SENSOR3,
  ADC_MUX_SENSOR4,
  ADC_MUX_SENSOR5,
  ADC_MUX_SENSOR6,
  ADC_MUX_SENSOR7,
  ADC_MUX_SENSOR15,
  ADC_MUX_SENSOR17A,
  ADC_MUX_TEMP_PWR,
  
// Normaly closed
  ADC_MUX_CUR_SYS,
  ADC_MUX_VOLT_5V0A,
  ADC_MUX_PUMP_VOLT1,
  ADC_MUX_PUMP_VOLT2,
  ADC_MUX_PUMP_VOLT3,
  ADC_MUX_SENSOR8,
  ADC_MUX_SENSOR9,
  ADC_MUX_SENSOR10,
  ADC_MUX_SENSOR11,
  ADC_MUX_SENSOR12,
  ADC_MUX_SENSOR13,
  ADC_MUX_SENSOR14,
  ADC_MUX_SENSOR16A,
  ADC_MUX_SENSOR18A,
  ADC_MUX_TEMP_AMB,
#endif
  ADC_MUX_Last
} AdcMuxChannels_t;

/*******************************************************************************
 * Public const variables
 ******************************************************************************/

extern const char * const ADC_MUX_NAMES[ADC_MUX_Last];

/*******************************************************************************
 * Functions
 ******************************************************************************/

void AdcMuxInit(void);

float AdcMuxGetVolt(AdcMuxChannels_t muxChannel);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
