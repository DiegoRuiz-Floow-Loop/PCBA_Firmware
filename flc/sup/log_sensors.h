/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef LOG_SENSORS_H
#define LOG_SENSORS_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>
#include <plf/trc/trc.h>

#include <app/flc_def.h>

#include "adc_mux.h"
 
/*******************************************************************************
 * Type definitions
 ******************************************************************************/

/*******************************************************************************
 * Functions
 ******************************************************************************/

bool LogSensorsInit(const FlcComponentMapping_t * cfg);

bool LogSensorGetValue(FlcHwComponent_t sensor, float * value);

#ifdef CLI_ENABLE
int_fast16_t CliLogSensorShowAll(CliParam_t param1, CliParam_t param2, CliParam_t param3);
#endif

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
