/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <windows.h>
#include <iostream>
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <string>
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <string>
#pragma hdrstop

#include "LanUtil.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


bool GetAdapterInfo(int adapter_num, uint8_t * pMa)
{
    // Reset the LAN adapter so that we can begin querying it
  NCB Ncb;
  memset(&Ncb, 0, sizeof(Ncb));
  Ncb.ncb_command = NCBRESET;
  Ncb.ncb_lana_num = adapter_num;
  if (Netbios(&Ncb) != NRC_GOODRET) {
    //mac_addr = "bad (NCBRESET): ";
    //mac_addr += string(Ncb.ncb_retcode);
    return false;
  }

  // Prepare to get the adapter status block
  memset(&Ncb,0,sizeof(Ncb));
  Ncb.ncb_command = NCBASTAT;
  Ncb.ncb_lana_num = adapter_num;
  strcpy((char *) Ncb.ncb_callname, "*");
  struct ASTAT
  {
    ADAPTER_STATUS adapt;
    NAME_BUFFER NameBuff[30];
  } Adapter;
  memset(&Adapter,0,sizeof(Adapter));
  Ncb.ncb_buffer = (unsigned char *)&Adapter;
  Ncb.ncb_length = sizeof(Adapter);

  // Get the adapter's info and, if this works, return it in standard,
  // colon-delimited form.
  if (Netbios(&Ncb) == 0)
  {
    *pMa++ = Adapter.adapt.adapter_address[5];
    *pMa++ = Adapter.adapt.adapter_address[4];
    *pMa++ = Adapter.adapt.adapter_address[3];
    *pMa++ = Adapter.adapt.adapter_address[2];
    *pMa++ = Adapter.adapt.adapter_address[1];
    *pMa = Adapter.adapt.adapter_address[0];
    return true;
  }
  else
  {
    //mac_addr = "bad (NCBASTAT): ";
    //mac_addr += string(Ncb.ncb_retcode);
    return false;
  }
}

void LanMacGet(uint8_t  * pMa)
{
  // Get adapter list
  LANA_ENUM AdapterList;
  NCB Ncb;
  memset(&Ncb, 0, sizeof(NCB));
  Ncb.ncb_command = NCBENUM;
  Ncb.ncb_buffer = (unsigned char *)&AdapterList;
  Ncb.ncb_length = sizeof(AdapterList);
  Netbios(&Ncb);

  // Get all of the local ethernet addresses
  for (int i = 0; i < AdapterList.length; i++)  {
    if (GetAdapterInfo(AdapterList.lana[i], pMa))  {
      return;
    }
  }
  memset(pMa, 0, 6);
}


//---------------------------------------------------------------------------
