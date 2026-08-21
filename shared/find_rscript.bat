@echo off
rem find_rscript.bat - locate Rscript.exe and leave it in RS for the caller.
rem
rem CALLED, not run: `call find_rscript.bat` then use %RS%. Deliberately has no
rem setlocal, because the whole point is to leave RS set in the caller's scope.
rem Sets RS empty and exits 1 if nothing is found.
rem
rem WHY DISCOVERY RATHER THAN A PINNED VERSION. The VM has only R-4.3.1 while
rem this dev machine has 4.5.2, so any pinned path is wrong on one of them -
rem which forced a local edit to launch_console_ui.bat on the VM, and that edit
rem was silently reverted by every pull. One shared lookup, no pin, no twin.
rem
rem PATH is checked LAST and only as a courtesy: qlik-cli's VM has no R on PATH
rem and the account cannot add one, so the directory scan is the real path.

set "RS="
set "PF86=%ProgramFiles(x86)%"

rem Standard install roots. Within each, R-* folders are scanned in ascending
rem name order and the last match wins, so the newest version is preferred.
rem GOTCHA: %ProgramFiles(x86)% is copied to PF86 above and never used inline -
rem the parentheses in its NAME break cmd's parser inside a parenthesised block.
for %%D in ("%ProgramFiles%\R" "%PF86%\R" "C:\R" "D:\R") do call :scan %%D

if not defined RS for %%X in (Rscript.exe) do if not "%%~$PATH:X"=="" set "RS=%%~$PATH:X"

if not defined RS exit /b 1
exit /b 0

:scan
if "%~1"=="" goto :eof
if not exist "%~1\" goto :eof
for /f "delims=" %%V in ('dir /b /on "%~1\R-*" 2^>nul') do (
  if exist "%~1\%%V\bin\Rscript.exe" set "RS=%~1\%%V\bin\Rscript.exe"
)
goto :eof
