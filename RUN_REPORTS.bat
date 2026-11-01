@echo off
rem RUN_REPORTS.bat - the per-app review + tech-debt reports, kept OUT of the
rem migration walk-through (launch_console_ui.bat). Runs on the apps last
rem picked there (fleet/selection.csv); nothing in the cloud changes.
cd /d "%~dp0"
call "%~dp0shared\find_rscript.bat"
if errorlevel 1 (
  echo Could not find Rscript.exe.
  echo Edit shared\find_rscript.bat if R lives somewhere else on this machine.
  pause
  exit /b 1
)
"%RS%" fleet/fleet.R report --selected
echo.
echo Each app's report: fleet\apps\(app name)\report.html
pause
