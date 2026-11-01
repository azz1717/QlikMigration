@echo off
rem RUN_REPORTS.bat - the per-app review + tech-debt reports, kept OUT of the
rem migration (RUN_MIGRATION.bat). Asks which space and which apps every
rem time, downloads them (a read - nothing in the cloud changes) and writes
rem fleet\apps\(app name)\report.html for each.
cd /d "%~dp0"
call "%~dp0shared\find_rscript.bat"
if errorlevel 1 (
  echo Could not find Rscript.exe.
  echo Edit shared\find_rscript.bat if R lives somewhere else on this machine.
  pause
  exit /b 1
)
"%RS%" fleet/console_ui.R --reports
pause
