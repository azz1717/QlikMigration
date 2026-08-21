@echo off
cd /d "%~dp0"
call "%~dp0shared\find_rscript.bat"
if errorlevel 1 (
  echo Could not find Rscript.exe.
  echo Looked under Program Files\R\R-*, C:\R\R-*, D:\R\R-*, and PATH.
  echo Edit shared\find_rscript.bat if R lives somewhere else on this machine.
  pause
  exit /b 1
)
"%RS%" diagnostics\qlik_cli_probe.R %*
rem pause is load-bearing: the window must stay open to be photographed.
pause
