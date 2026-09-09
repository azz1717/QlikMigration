@echo off
rem mock_qlik.bat - stands in for qlik.exe in offline tests (PLAN-fleet.md M0).
rem
rem Point QLIK_CLI_PATH (or qlik_cli_path.txt) at THIS file and every fleet
rem verb runs unchanged against canned data. It is a .bat rather than an .exe
rem because R's system2() on Windows goes through cmd.exe, so a .bat resolves
rem the same way an .exe does - and fleet/test_fleet.R proves that pairing
rem rather than assuming it.
rem
rem Rscript is found the same way every other launcher here finds it, so no
rem path is pinned: the VM has R-4.3.1 and this machine has 4.5.2.
setlocal
call "%~dp0..\shared\find_rscript.bat"
if not defined RS (
  echo Error: no Rscript.exe found 1>&2
  exit /b 9
)
"%RS%" --vanilla "%~dp0mock_qlik.R" %*
exit /b %ERRORLEVEL%
