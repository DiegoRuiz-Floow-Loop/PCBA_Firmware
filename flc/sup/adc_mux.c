/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

// ADC MUX

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <hal/dio/dio.h>
#include <hal/adc/adc.h>

#include "plf/evos/evos.h"
#include "adc_mux.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define ADC_MUX_CHANNELS ADC_MUX_Last / 2

#if defined(EVK)
#define MUX_SEL_NC()    HAL_DIO_PIN_CLR(MCUPIN_NA)
#define MUX_SEL_NO()    HAL_DIO_PIN_SET(MCUPIN_NA)
#else
#define MUX_SEL_NC()    HAL_DIO_PIN_CLR(MP_A_MUX_SEL)
#define MUX_SEL_NO()    HAL_DIO_PIN_SET(MP_A_MUX_SEL)
#endif


/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef enum {
  ADC_MUX_STATE_NO,
  ADC_MUX_STATE_NC,
  ADC_MUX_STATE_Last
} AdcMuxState_t;

/*******************************************************************************
 * Public const variables
 ******************************************************************************/

const char * const ADC_MUX_NAMES[] = {
#if defined(EVK)
// Normaly open
  "ADC_MUX_ITEM_1_1",
  "ADC_MUX_ITEM_2_1",
  "ADC_MUX_ITEM_3_1",
  "ADC_MUX_ITEM_4_1",
  "ADC_MUX_ITEM_5_1",
  "ADC_MUX_ITEM_VBAT_1",
  "ADC_MUX_ITEM_VEXT_1",
  "ADC_MUX_ITEM_TEMP_1",
  
// Normaly closed
  "ADC_MUX_ITEM_1_2",
  "ADC_MUX_ITEM_2_2",
  "ADC_MUX_ITEM_3_2",
  "ADC_MUX_ITEM_4_2",
  "ADC_MUX_ITEM_5_2",
  "ADC_MUX_ITEM_VBAT_2",
  "ADC_MUX_ITEM_VEXT_2",
  "ADC_MUX_ITEM_TEMP_2",
#else
// Normally open
  "ADC_MUX_VOLT_24V",
  "ADC_MUX_VOLT_5V0",
  "ADC_MUX_PUMP_CUR1",
  "ADC_MUX_PUMP_CUR2",
  "ADC_MUX_PUMP_CUR3",
  "ADC_MUX_SENSOR1",
  "ADC_MUX_SENSOR2",
  "ADC_MUX_SENSOR3",
  "ADC_MUX_SENSOR4",
  "ADC_MUX_SENSOR5",
  "ADC_MUX_SENSOR6",
  "ADC_MUX_SENSOR7",
  "ADC_MUX_SENSOR15",
  "ADC_MUX_SENSOR17A",
  "ADC_MUX_TEMP_PWR",
  
// Normally closed
  "ADC_MUX_CUR_SYS",
  "ADC_MUX_VOLT_5V0A",
  "ADC_MUX_PUMP_VOLT1",
  "ADC_MUX_PUMP_VOLT2",
  "ADC_MUX_PUMP_VOLT3",
  "ADC_MUX_SENSOR8",
  "ADC_MUX_SENSOR9",
  "ADC_MUX_SENSOR10",
  "ADC_MUX_SENSOR11",
  "ADC_MUX_SENSOR12",
  "ADC_MUX_SENSOR13",
  "ADC_MUX_SENSOR14",
  "ADC_MUX_SENSOR16A",
  "ADC_MUX_SENSOR18A",
  "ADC_MUX_TEMP_AMB",
#endif
};
static_assert(SIZEOF_ARRAY(ADC_MUX_NAMES) == ADC_MUX_Last, "ADC_MUX_NAMES unexpected size");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

extern ADC_HandleTypeDef hadc1;

static AdcMuxState_t muxState = ADC_MUX_STATE_NC;
static uint32_t conversionsNo[ADC_MUX_CHANNELS];
static uint32_t conversionsNc[ADC_MUX_CHANNELS];

static EvosEventHandle_t MuxUpdateHandle = EVOS_UNINITIALIZED_HANDLE;

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

static float AdcValueToVolt(uint16_t adcValue);
static void UpdateMux(EvosEventParam_t param);

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void AdcMuxInit(void)
{
  TRACE(TRC_TA_SUP, TRC_TL_COMPONENT, "ADC_MUX_Init()");
  MuxUpdateHandle = EvosEventRegister(UpdateMux, "ADC Mux update timer");  
  EvosEventSetAndReload(MuxUpdateHandle, 0, 50, 0);
}

float AdcMuxGetVolt(const AdcMuxChannels_t muxChannel) {
  if (muxChannel < ADC_MUX_CHANNELS) {
    return AdcValueToVolt(conversionsNo[muxChannel]);
  } else if (muxChannel < ADC_MUX_Last) {
    return AdcValueToVolt(conversionsNc[muxChannel - ADC_MUX_CHANNELS]);
  }
  
  return 0;
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

static float AdcValueToVolt(const uint16_t adcValue)
{  
  if(adcValue == 0) {
    return 0.0f;
  }
  
  //      ADCin
  // U = -------- * Uref
  //      ADCmax
  
  const float adcMax  = (float)(ADC_RESOLUTION - 1);
  const float voltRef = ((float)(vdda_mv)) / 1000.0f;
  const float volt    = (adcValue / adcMax) * voltRef;
  
  return volt;
}

static void UpdateMux(const EvosEventParam_t param) {
  HAL_ADC_Stop_DMA(&hadc1); //DMA needs to be stopped before data ptr. can be changed.
  
  if(muxState == ADC_MUX_STATE_NC) {
    MUX_SEL_NO();
    muxState = ADC_MUX_STATE_NO;
    HAL_ADC_Start_DMA(&hadc1, conversionsNo, ADC_MUX_CHANNELS);
  } else if (muxState == ADC_MUX_STATE_NO) {
    MUX_SEL_NC();
    muxState = ADC_MUX_STATE_NC;
    HAL_ADC_Start_DMA(&hadc1, conversionsNc, ADC_MUX_CHANNELS);
  }
}

/******************************************************************************/

/*******************************************************************************
 * Local functions
 ******************************************************************************/


#ifdef CLI_ENABLE

static int_fast16_t CliShowAllChannels(CliParam_t sz, CliParam_t param2, CliParam_t param3) {
  for(size_t i = 0; i < ADC_MUX_Last; i++) {
    CliPrintf("%s: %i\r\n", ADC_MUX_NAMES[i], i + 1);
  }
  
  return CLI_RESULT_OK;
}

static int_fast16_t CliShowChannel(CliParam_t param1, CliParam_t param2, CliParam_t param3) {
  if(param1 > ADC_MUX_Last || param1 <= 0) {
    return CLI_RESULT_ERROR_PARAMETER_VALUE;
  }
  
  CliPrintf("%s: %.2fV\r\n", ADC_MUX_NAMES[param1 - 1], (double)AdcMuxGetVolt(param1 - 1));  
  return CLI_RESULT_OK;
}

CLI_START_TABLE(adc_mux)
  CLI_ENTRY0( "show",   "Show all channels", CliShowAllChannels)
  CLI_ENTRY1( "ch",     "Show value for channel #", CliShowChannel, CLI_PARAM_INT)

  CLI_SUBTEST("trc",    "Trace system", trc)
  CLI_ENTRY0( "reboot", "Re-boot System", CliReboot)  
CLI_END_TABLE(adc_mux)

#endif

/******************************************************************************/
