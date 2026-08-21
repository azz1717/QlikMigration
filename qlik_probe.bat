@echo off
cd /d "%~dp0"
set "RS=C:\Program Files\R\R-4.3.1\bin\Rscript.exe"
if not exist "%RS%" (
  echo Rscript not found at:
  echo   %RS%
  echo Edit this file and set RS to this machine's Rscript.exe path.
  pause
  exit /b 1
)
"%RS%" qlik_cli_probe.R %*
pause
