/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/


#include "win/TextIniFile.h"

//static TIniFile * ini = NULL;

void OpenTextIniFile(
	TIniFile ** ini,
	UnicodeString fileName)
{
	*ini = new TIniFile(fileName);
}

void CloseTextIniFile(
	TIniFile * ini)
{
	delete (ini);
	//*ini = NULL;
}

void CleanTextIniFileSection(
	TIniFile * ini,
	UnicodeString section)
{
	TStringList * string = new TStringList;
	ini->ReadSection(section, string);
	for (int i = 0; i < string->Count; i++)
		ini->DeleteKey(section, string->Strings[i]);

	delete(string);
}

UnicodeString GetTextIniFileString(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	UnicodeString value,
	bool saveDefault)
{
	if (saveDefault) {
		UnicodeString val = ini->ReadString(section, ident, "");
		if (val=="") {
			 ini->WriteString(section, ident, value);
			 return value;
		} else {
			return val;
		}
	} else {
		return ini->ReadString(section, ident, value);
	}
}

void SetTextIniFileString(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	UnicodeString value)
{
	ini->WriteString(section, ident, value);
}

int GetTextIniFileInteger(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	int32_t value,
	bool saveDefault)
{
	if (saveDefault) {
		int32_t val = ini->ReadInteger(section, ident, 0x7FFFFFFF);
		if (val==0x7FFFFFFF) {
			 ini->WriteInteger(section, ident, value);
			 return value;
		} else {
			return val;
		}
	} else {
		return ini->ReadInteger(section, ident, value);
	}
}

void SetTextIniFileInteger(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	int value)
{
	ini->WriteInteger(section, ident, value);
}



