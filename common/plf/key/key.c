/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

//#include "i2c.h"

#include "hal/hal.h"
#include "hal/dio/dio.h"
#include "hal/mcupin/mcupin.h"

#include "plf/key/key.h"
#include "plf/evos/evos.h"
#include "plf/trc/trc.h"

/*******************************************************************************/

//static EvosEventHandle_t       keyEvent = EVOS_UNINITIALIZED_HANDLE;

//static KeySet_t          changeOld;    // one bit/key
//static KeySet_t          oldKeyState;  // -//-
//static KeySet_t          keyState;
//static KeySet_t          keyStateSample;

//static bool justSample = true;

//static void KeyTask(EvosEventParam_t mask)
//{
//  bool   level;
//  int_fast8_t   k;
//  KeySet_t      changeNew;
//  
//  if (justSample) {
//    HalReadButtons(&keyStateSample);
//  } else {
//    HalReadButtons(&keyState);
//    keyState &= keyStateSample;
//    
//    changeNew = (keyState ^ oldKeyState);
//    
//    if (0 != mask) {
//      // Force a "change". 
//      changeNew |= mask;
//      // Make sure the change happens immediately (i.e., circumvent debounce).
//      changeOld = changeNew;
//    }
//      
//    
//    if (changeNew != 0) {
//      if (changeNew != changeOld) {
//        changeOld = changeNew;
//      } else {
//        for (k = 0; k < KEY_Last; k++) {
//          if ((changeNew & (1 << k)) != 0) {  /* pick changes, one at a time */
//            level = ((keyState & (1 << k)) != 0);
//            XKeyChanged((Key_t)k, level);
//          }
//        }
//        oldKeyState = keyState;
//      }
//    }
//  }
//  justSample = !justSample;
//  EvosEventSetDelta(keyEvent, 15, 0);
//}

static EvosEventHandle_t       keyEvent = EVOS_UNINITIALIZED_HANDLE;

static volatile KeySet_t    servicedKeyState;  // -//-

bool KeyIsActive(Key_t key)
{
  return ((servicedKeyState & (1 << key)) != 0);
}


#define KEY_CHECK_DELAY         (10)   // sample inputs every 10 ms
#define KEY_CHECK_MASK          (0x07) // Mask to test the last n samples
                                       // e.g. 0x07 (0000111b) 3 last three sampled bits must all be '1' before key state is '1'
typedef struct {
  bool    state;
  uint8_t value;
} KeyState_t;


static void KeyTask(EvosEventParam_t p)
{
  static KeyState_t key[KEY_Last]; // To save the last key states
  uint8_t  currKeys; // Current key states. One byte for all keys

  HalReadButtons(&currKeys);

  for (Key_t k = (Key_t)0; k < KEY_Last; k++) {
    uint8_t keyLevel = currKeys & (1 << k) ? 1 : 0;
    key[k].value = (key[k].value << 1) | keyLevel;

    // Debounce keys
    bool state = key[k].value & KEY_CHECK_MASK ? true : false;
    if (state != key[k].state) {
      key[k].state = state;
      // Input has changed
      if ((currKeys & (1 << k)) != 0) {
        servicedKeyState |= (1 << k);
        XKeyChanged(k, true);
      } else {
        servicedKeyState &= ~(1 << k);
        XKeyChanged(k, false);
      }
    }
  }
  EvosEventSetDelta(keyEvent, KEY_CHECK_DELAY, 0);
}



//void KeyScanner(bool enabled)
//{
//  if (keyEvent == EVOS_UNINITIALIZED_HANDLE) {
//    keyEvent = EvosEventRegister(KeyTask, "KeyTask");
//  }
//  if (enabled) {
//    changeOld    = 0;
//    oldKeyState  = 0;
//    //(void)memset(keyFlags, 0, sizeof(keyFlags));
//    EvosEventSetDelta(keyEvent, 500, 0);
//  } else {
//    EvosEventClear(keyEvent);
//  }

//}

void KeyScanner(bool enabled)
{
  if (keyEvent == EVOS_UNINITIALIZED_HANDLE) {
    keyEvent = EvosEventRegister(&KeyTask, "Kbd");
  }
  if (enabled) {
    servicedKeyState  = 0;
    EvosEventSetDelta(keyEvent, 100, 0);
  } else {
    EvosEventClear(keyEvent);
  }

}

void KeyUpdate(KeySet_t mask)
{
  if (keyEvent == EVOS_UNINITIALIZED_HANDLE) {
    keyEvent = EvosEventRegister(KeyTask, "KeyTask");
  }
  EvosEventSetNow(keyEvent, mask);
}


__weak void XKeyChanged(Key_t key, bool level) 
{
}


void KeyInit(void)
{
  //oldKeyState = 0;
 
}
