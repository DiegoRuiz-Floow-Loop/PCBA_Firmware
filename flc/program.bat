@echo off

::REM Install Segger J-Link 64-bit version
set jl="%ProgramFiles%\SEGGER\JLink\jlink.exe"
set jlstm32="%ProgramFiles%\SEGGER\JLink\jlinkstm32.exe"

::REM Setup STM32 family for J-Link STM32 unlock
::[13] STM32L4xxxx
set family=13

@echo on
%jlstm32% -SetDeviceFamily %family% -Speed 5000 -Exit 1
@echo off


::REM Setup STM32 MCU for J-Link
::set mcu=STM32L471ZG
set mcu=STM32L471ZE

echo si swd>program.txt
echo speed 5000>>program.txt
echo device %mcu%>>program.txt
echo erase>>program.txt
echo loadfile .\flc_all.hex>>program.txt
echo r>>program.txt
echo g>>program.txt
echo q>>program.txt
echo .>>program.txt

@echo on
%jl% program.txt
@echo off

erase program.txt


set jl=
set jlstm32=
set family=
set mcu=


echo Press a key to terminate...
pause>nul