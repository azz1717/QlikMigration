@echo off
cd /d "%~dp0"
set /p RS=<rscript_path.txt
if not exist "%RS%" (
  echo Rscript.exe not found at: %RS%
  echo Put the right path in rscript_path.txt
  pause
  exit /b 1
)
"%RS%" migrate.R
pause
