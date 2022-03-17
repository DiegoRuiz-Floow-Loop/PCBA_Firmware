/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#include <vcl.h>
#include <registry.hpp>

#include "win/registry.h"

TRegistry   * reg = NULL;


UnicodeString GetRegistryString(
	UnicodeString key,
	UnicodeString item,
	UnicodeString defVal)
{
	UnicodeString as;
	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;
	try {
		if (reg->OpenKey(key, false)) {
			try {
				as = reg->ReadString(item);
			}
			catch (...) {
				as = defVal;
			}
		} else {
			as = defVal;
		}
	}
	catch (...) {
		as = defVal;
	}
	return as;
}
void SetRegistryString(
	UnicodeString key,
	UnicodeString item,
	UnicodeString val)
{
	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;

	if (val=="") {
		if (reg->OpenKey(key, TRUE)) {
			reg->DeleteKey(key);
		}
	} else {
		if (reg->OpenKey(key, TRUE)) {
			reg->WriteString(item, val);
		}
	}
}

int GetRegistryInteger(
	UnicodeString key,
	UnicodeString item,
	int defVal)
{
	int lo;
	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;
	try {
		if (reg->OpenKey(key, false)) {
			try {
				lo = reg->ReadInteger(item);
			}
			catch (...) {
				lo = defVal;
			}
		} else {
			lo = defVal;
		}
	}
	catch (...) {
		lo = defVal;
	}
	return lo;

}
void SetRegistryInteger(
	UnicodeString key,
	UnicodeString item,
	int val)
{
	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;
	if (reg->OpenKey(key, TRUE))
		reg->WriteInteger(item, val);
}

void GetRegistryBin(
	UnicodeString key,
	UnicodeString item,
	void * val,
	void * defVal,
	int size)
{
	int no;

	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;
	try {
		if (reg->OpenKey(key, false)) {
			try {
				no = reg->ReadBinaryData(item, val, size);
				if (no != size)
					memcpy(val, defVal, size);
			}
			catch (...) {
				memcpy(val, defVal, size);
			}
		} else {
			memcpy(val, defVal, size);
		}
	}
	catch (...) {
		memcpy(val, defVal, size);
	}
}

void SetRegistryBin(
	UnicodeString key,
	UnicodeString item,
	void * val,
	int size)
{
	if (!reg) {
		reg = new TRegistry();
	}
	reg->RootKey = HKEY_CURRENT_USER;
	if (reg->OpenKey(key, TRUE))
		reg->WriteBinaryData(item, val, size);
}

void CloseRegistry(void)
{
	delete(reg);
	reg = NULL;
}

