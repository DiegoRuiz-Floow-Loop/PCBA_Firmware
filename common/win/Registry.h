/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef RegistryH
#define RegistryH
//---------------------------------------------------------------------------

extern UnicodeString GetRegistryString(
	UnicodeString key,
	UnicodeString item,
	UnicodeString defVal);

extern void SetRegistryString(
	UnicodeString key,
	UnicodeString item,
	UnicodeString val);

extern int GetRegistryInteger(
	UnicodeString key,
	UnicodeString item,
  int defVal);

extern void SetRegistryInteger(
	UnicodeString key,
	UnicodeString item,
  int val);

extern void GetRegistryBin(
	UnicodeString key,
	UnicodeString item,
  void * val,
  void * defVal,
  int size);

extern void SetRegistryBin(
	UnicodeString key,
  UnicodeString item,
  void * val,
	int size);

extern void CloseRegistry(void);

#endif
