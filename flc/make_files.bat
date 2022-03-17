@echo off
echo.
echo.

del *.dfu
del *.dfx
del *.hex

::: TOOLS
set h2b="..\tools\hex2bin\hex2bin.exe"
set srec_cat="..\tools\srec_cat.exe"

::: SRC
set APP=flc
set APP_PATH=.\flc\MDK-ARM\flc
set BL=flc_bootloader
set BL_PATH=.\flc_bootloader\MDK-ARM\flc_bootloader

:<<< NO CHANGES NEEDED BELOW THIS LINE >>>

::: FILES
xcopy "%APP_PATH%\%APP%.hex" *.* /y
xcopy "%BL_PATH%\%BL%.hex" *.* /y

::: Patch Main File
%h2b% %APP%.hex out=.\%APP%.dfu fill=255 type=release dfu dfx 
if errorlevel 7 goto ok_1
if not errorlevel 0 goto error
:ok_1

::: Patched file to dfu hex file
@echo on
%srec_cat% %APP%.dfu -Binary -offset 0x08004000 -o %APP%_dfu.hex -Intel -Output_Block_Size=128
REM %srec_cat% %APP%.dfu -Binary -offset 0x08004000 -unfill 0xFF 8 -o %APP%_dfu_optimized.hex -Intel -Output_Block_Size=128
@echo off
if errorlevel 1 goto error

::: combine hex files
@echo on
%srec_cat% %APP%_dfu.hex -Intel %BL%.hex -Intel -o %APP%_all.hex -Intel 
@echo off
if errorlevel 1 goto error

echo OK
goto exit

:error
echo ERROR
pause

:exit
set h2b=
set srec_cat=
:del *.dfu
del *.dfx
del %BL%.hex
del %APP%.hex
del %APP%_dfu.hex



:pause
