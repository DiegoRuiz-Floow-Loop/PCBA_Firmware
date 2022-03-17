/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

#ifndef LINEAR_INTERPOLATION_H
#define LINEAR_INTERPOLATION_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/plf.h>

/*******************************************************************************
 * Type definitions
 ******************************************************************************/

typedef struct {
  const float in;
  const float out;
} LookupTablePoints_t;

typedef struct {
  uint32_t size;
  const LookupTablePoints_t * data;
} LookupTable_t;

/*******************************************************************************
 * Functions
 ******************************************************************************/

bool LinearInterpolation(const LookupTable_t *table, const float in, float * out);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
