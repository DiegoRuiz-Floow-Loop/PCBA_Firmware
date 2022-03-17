/*******************************************************************************
 * TekPartner A/S
 * $Header$
 *******************************************************************************/

#ifndef TEXT_INI_FILE_H
#define TEXT_INI_FILE_H

#include <inifiles.hpp>
#include "stdint.h"

extern void OpenTextIniFile(
	TIniFile ** ini,
	UnicodeString fileName = ".\\INI.TXT");

extern void CloseTextIniFile(
	TIniFile * ini);

extern void CleanTextIniFileSection(
	TIniFile * ini,
	UnicodeString section);

extern UnicodeString GetTextIniFileString(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	UnicodeString value,
	bool saveDefault = true);

extern void SetTextIniFileString(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	UnicodeString value);

extern int GetTextIniFileInteger(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	int32_t value,
	bool saveDefault = true); // 0x7FFFFFFF is magic integer to determin not-defined tag

extern void SetTextIniFileInteger(
	TIniFile * ini,
	UnicodeString section,
	UnicodeString ident,
	int32_t value);


#endif  // TEXT_INI_FILE_H
