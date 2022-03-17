@echo off

del mcupin*.inc

copy "..\..\flc\Core\Inc\main.h">nul
powershell.exe -executionpolicy bypass -File ..\..\..\common\hal\mcupin\mcupin.ps1 > mcupin.inc
if exist main.h del main.h

copy "..\..\flc_bootloader\Core\Inc\main.h">nul
powershell.exe -executionpolicy bypass -File ..\..\..\common\hal\mcupin\mcupin.ps1 > mcupin_bl.inc
if exist main.h del main.h

copy "..\..\..\flc_evk\STM32L431\Inc\main.h">nul
powershell.exe -executionpolicy bypass -File ..\..\..\common\hal\mcupin\mcupin.ps1 > mcupin_evk.inc
if exist main.h del main.h

copy "..\..\..\flc_evk\STM32L431_bootloader\Inc\main.h">nul
powershell.exe -executionpolicy bypass -File ..\..\..\common\hal\mcupin\mcupin.ps1 > mcupin_evk_bl.inc
if exist main.h del main.h

