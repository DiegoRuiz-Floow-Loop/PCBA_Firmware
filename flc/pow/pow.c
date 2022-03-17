/*******************************************************************************
 * TekPartner A/S
 * $Header: https://svn.tekpartner.dk/svn/FLP_P01/sw/trunk/flc/shc/valve.c 142 2021-07-09 09:43:49Z ett17934 $
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include "plf/plf.h"
#include "plf/trc/trc.h"

#include "hal/dio/dio.h"

#include "pow.h"

/*******************************************************************************
 * Defines
 ******************************************************************************/

#define TA_POWER  (TRC_TA_APP)

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  McuPin_t pinEnable;
  McuPin_t pinFault;
} PowerHwCfg_t;

/*******************************************************************************
 * Local constant variables
 ******************************************************************************/

static const PowerHwCfg_t HW_CFG[] = {
  // Enable pin      // Fault pin
  { MP_PWR_EN_VALVE, MP_PWR_FAULT_24V },   // POWER_VALVES
  { MP_PWR_EN_UV,    MP_PWR_FAULT_24V },   // POWER_UVLAMP
  { MP_PWR_EN_MMI,   MP_PWR_FAULT_24V },   // POWER_MMI
};
static_assert(SIZEOF_ARRAY(HW_CFG) == POWER_Last, "Wrong table row count!");

/*******************************************************************************
 * Local data variables
 ******************************************************************************/

/*******************************************************************************
 * Macros
 ******************************************************************************/

/*******************************************************************************
 * Function prototypes
 ******************************************************************************/

/*******************************************************************************
 * Public functions
 ******************************************************************************/

void POW_Init(void)
{
  TRACE(TA_POWER, TRC_TL_COMPONENT, "POW_Init()");
  
  // Power sources not controlled by external drivers
  PowerSet(POWER_MMI, true);
}

void PowerSet(const Power_t idx, const bool enable)
{
  if (idx >= POWER_Last) return;

  HAL_DIO_PIN_WRITE(HW_CFG[idx].pinEnable, enable);
}

void PowerXi24VFaultCb(void)
{
  // Power fault not supported
}

void PowerXiSensorFaultCb(void)
{
  // Power fault not supported
}

/*******************************************************************************
 * Local functions
 ******************************************************************************/

/******************************************************************************/
