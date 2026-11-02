@echo off
rem RUN_REPORTS.bat - the per-app review + tech-debt reports, kept OUT of the
rem migration (RUN_MIGRATION.bat). Asks which space and which apps every
rem time, downloads them (a read - nothing in the cloud changes) and writes
rem fleet\apps\(app name)\report.html for each.
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
"%RS%" fleet/console_ui.R --reports
pause
