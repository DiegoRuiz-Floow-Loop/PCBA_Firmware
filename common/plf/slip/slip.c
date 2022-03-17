/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include "plf/slip/slip.h"

/******************************************************************************/

void Slip_RawToSlipSendFirst(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc)
{
  sendFunc(SLIP_END);
  for (int i = 0; i < rawLen; i++) {
    if (raw[i] == SLIP_END) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_END);
    } else if (raw[i] == SLIP_ESC) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_ESC);
    } else {
      sendFunc(raw[i]);
    }
  }
}

void Slip_RawToSlipSendLast(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc)
{
  for (int i = 0; i < rawLen; i++) {
    if (raw[i] == SLIP_END) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_END);
    } else if (raw[i] == SLIP_ESC) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_ESC);
    } else {
      sendFunc(raw[i]);
    }
  }
  sendFunc(SLIP_END);
}

void Slip_RawToSlipSend(
	const uint8_t         * raw,        /* raw data to be coded */
	uint_fast16_t					rawLen,     /* number of raw data */
  SlipSendFunc_t        sendFunc)
{
	uint_fast16_t i;
  sendFunc(SLIP_END);
  for (i = 0; i < rawLen; i++) {
    if (raw[i] == SLIP_END) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_END);
    } else if (raw[i] == SLIP_ESC) {
      sendFunc(SLIP_ESC);
      sendFunc(SLIP_ESC_ESC);
    } else {
      sendFunc(raw[i]);
    }
  }
  sendFunc(SLIP_END);
}

/******************************************************************************/

bool Slip_SlipCharToRawChars(
  uint8_t ch,
  uint8_t * raw,
	uint16_t * rawLength,
  bool * slipNormal, 
  const uint16_t msgSizeMax)
{
  uint16_t rawLen = *rawLength;
  if (*slipNormal == true) {
    if (ch == SLIP_END) {
      if (rawLen > 0) {
        *slipNormal = true;
        *rawLength = rawLen;
        return true;
      }
    } else if (ch == SLIP_ESC) {
      *slipNormal = false;
    } else {
			if (rawLen < msgSizeMax) { // Space for data ???
        *raw = ch;
        rawLen++;
			} else {
				*slipNormal = true;
				rawLen = 0;
			}
    }
  } else { // stuffing
  	if (rawLen < msgSizeMax) {  // Space for data ???
			if (ch == SLIP_ESC_END)
				*raw = SLIP_END;
			else
				*raw = SLIP_ESC;
			rawLen++;
			*slipNormal = true;
  	} else {
			*slipNormal = true;
			rawLen = 0;
		}
  }
  *rawLength = rawLen;
  return false;
}

/******************************************************************************/

//void Slip_RawToSlip(
//  const uint8_t * raw,
//  uint16_t rawLen,
//  uint8_t * slip,
//	uint16_t * slipLength)
//{
//	uint_fast16_t slipLen = 0;
//  uint_fast16_t i;
//  slip[slipLen++] = SLIP_END;
//  for (i = 0; i < rawLen; i++) {
//    if (raw[i] == SLIP_END) {
//      slip[slipLen++] = SLIP_ESC;
//      slip[slipLen++] = SLIP_ESC_END;
//    } else if (raw[i] == SLIP_ESC) {
//      slip[slipLen++] = SLIP_ESC;
//      slip[slipLen++] = SLIP_ESC_ESC;
//    } else {
//      slip[slipLen++] = raw[i];
//    }
//  }
//  slip[slipLen++] = SLIP_END;
//  *slipLength = slipLen;
//}





///******************************************************************************/

//bool Slip_SlipToRaw(
//  const uint8_t * slip,
//	const uint16_t slipLen,
//  uint8_t * raw,
//	uint16_t * rawLen)
//{
//  uint16_t i;
//  bool slipNormal = true;
//  *rawLen = 0;

//  for (i=0; i<slipLen; i++) {
//    if (Slip_SlipCharToRawChars(slip[i], &raw[*rawLen], rawLen, &slipNormal, slipLen)) {
//      return true;
//    }
//  }
//  return false;
//}


///******************************************************************************/

//bool Slip_SlipToRawConv(
//  const uint8_t * slip,
//	const uint16_t slipLen,
//	uint16_t *slipConvLen,
//  uint8_t * raw,
//	uint16_t * rawLen)
//{
//  uint16_t i;
//  bool slipNormal = true;
//  *rawLen = 0;

//  for (i=0; i<slipLen; i++) {
//    if (Slip_SlipCharToRawChars(slip[i], &raw[*rawLen], rawLen, &slipNormal, slipLen)) {
//			*slipConvLen = i;
//      return true;
//    }
//  }
//	*slipConvLen = 0;
//  return false;
//}

/******************************************************************************/

