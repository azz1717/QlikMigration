@echo off
cd /d "%~dp0"
call "%~dp0shared\find_rscript.bat"
if errorlevel 1 (
  echo Could not find Rscript.exe.
  echo Looked under C:\ and D:\Program Files\R, C:\R, D:\R, and PATH.
  echo Copy rscript_path.txt.example to rscript_path.txt and put the
  echo full path to Rscript.exe on its one line.
  pause
  exit /b 1
)
"%RS%" fleet/console_ui.R
pause
