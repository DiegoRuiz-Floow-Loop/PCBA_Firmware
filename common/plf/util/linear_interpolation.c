/*******************************************************************************
 * TekPartner A/S
 * $Header$
 ******************************************************************************/

/*******************************************************************************
 * Includes
 ******************************************************************************/

#include <plf/trc/trc.h>
#include <hal/dio/dio.h>

#include "linear_interpolation.h"

/*******************************************************************************
 * Public functions
 ******************************************************************************/

bool LinearInterpolation(const LookupTable_t * const table, const float in, float * const out) {   
  float x1 = 0.0f;
  float y1 = 0.0f;
  float x2 = 0.0f;
  float y2 = 0.0f;
  bool foundParameters = false;
  
  if(table->size < 2) {
    TRACE(TRC_TA_PLF, TRC_TL_FATAL, "Table size too small!");
    return false;
  }
  
  for(uint32_t idx = 0; idx < table->size; idx++) {
    
    if (in < table->data[idx].in) {
      
      // In is less than the min table value
      // Interpolate between first two points in table
      if (idx == 0) {        
        x1 = table->data[idx].out;
        y1 = table->data[idx].in;
        x2 = table->data[idx + 1].out;
        y2 = table->data[idx + 1].in;
      } else {
        x1 = table->data[idx - 1].out;
        y1 = table->data[idx - 1].in;
        x2 = table->data[idx].out;
        y2 = table->data[idx].in;
      }
      
      foundParameters = true;
      break;
    }
    
    // In is larger than last point
    // Interpolate between 2nd last and last point
    if (idx == table->size - 1) {
      x1 = table->data[idx - 1].out;
      y1 = table->data[idx - 1].in;
      x2 = table->data[idx].out;
      y2 = table->data[idx].in;
      foundParameters = true;
      break;
    }
  }
  
  if(foundParameters == false) {
    return false;
  }
  
  //                  OUT_MAX - OUT_MIN
  // OUT = OUT_MIN + ------------------- * (IN - IN_MIN)
  //                   IN_MAX - IN_MIN  
  const float a = (x2 - x1) / (y2 - y1);  
  *out = x1 + a * (in - y1);
  return true;
}
