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
"%RS%" ui/console_ui.R
pause
