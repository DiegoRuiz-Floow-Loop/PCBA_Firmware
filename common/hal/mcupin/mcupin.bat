@echo off

del mcupin.inc

copy "..\..\project\Core\Inc\main.h">nul

powershell.exe -executionpolicy bypass -File ..\..\..\common\hal\mcupin\mcupin.ps1 > mcupin.inc

if exist main.h del main.h
