@echo off


:: COMMON ===============================================================

del *.dfu /s
del *.dfx /s

:keil
del *.kgo /s

:Embarcadero
del *.groupproj.local /s
del *.cbproj.local /s
del *.res /s
del *.tds /s
del *.map /s
del *.#* /s
del *.~* /s
:del *.stat /s
:del *.local /s
:del *.$$$ /s
:del *.vlb /s
:del *.tvsconfig /s
:del *.cache /s
:del *.obj /s
del *.ilc /s
del *.ild /s
del *.ils /s
del *.ilf /s
del *.pch /s
:del *.pdi /s

:Common
del .\*.bak /s
del .\*.tmp /s
del .\*.lst /s


:: SW  ===============================================================

rd ".\tools\hex2bin\__history" /s /q
rd ".\tools\hex2bin\hex2bin" /s /q
rd ".\tools\flpdfu\__astcache" /s /q
rd ".\tools\flpdfu\__history" /s /q
rd ".\tools\flpdfu\Win32" /s /q
rd ".\tools\flpdfu\Win64" /s /q

:: OTHER SW  ===============================================================



:: ============================================================================

goto exit
:error
echo ERROR...
pause
:exit
cd ..
