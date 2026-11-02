@echo off
rem RUN_REPORTS.bat - the per-app review + tech-debt reports, kept OUT of the
rem migration (RUN_MIGRATION.bat). Asks which space and which apps every
rem time, downloads them (a read - nothing in the cloud changes) and writes
rem fleet\apps\(app name)\report.html for each.
cd /d "%~dp0"
set /p RS=<rscript_path.txt
if not exist "%RS%" (
  echo Rscript.exe not found at: %RS%
  echo Put the right path in rscript_path.txt
  pause
  exit /b 1
)
"%RS%" fleet/console_ui.R --reports
pause
