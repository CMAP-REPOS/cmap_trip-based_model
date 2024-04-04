@echo off
rem run_cleanup_script.bat
rem Craig Heither, CMAP

cd %~dp0
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
set val=%val:~1,3%

REM -- Get path to INRO Python installation, redirect errors to nul in case file not found, read first path from file --
set infile=path.txt
if exist %infile% (del %infile% /Q)
dir "C:\Program Files\INRO\*python.exe" /s /b >> %infile% 2>nul
set /p empypath=<%infile%
set paren="
set empypath=%paren%%empypath%%paren%
echo Emme pypath = %empypath%
call :CheckEmpty1 %infile%
:pythonpass
if exist %infile% (del %infile% /Q)

REM -- Get name of .emp file --
set infile=empfile.txt
cd ..
if exist %infile% (del %infile% /Q)
dir "*.emp" /b >> %infile% 2>nul
set /p file1=<%infile%
echo file1 = %file1%
call :CheckEmpty %infile%
:filepass
if exist %infile% (del %infile% /Q)
cd Database

rem ============================================================= 
@ECHO   ***  Cleaning up databank.  ***
if exist cleanup.rpt (del cleanup.rpt)
call %empypath% useful_macros\cleanup_for_rerun.py %file1% %val%>> cleanup.rpt
if exist reports (del reports)
@ECHO  Cleanup complete.
goto end
rem ============================================================= 

:CheckEmpty
if %~z1 == 0 (goto badfile)
goto filepass

:badfile
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND .EMP FILE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:CheckEmpty1
if %~z1 == 0 (goto badpython)
goto pythonpass

:badpython
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND EMME PYTHON INSTALLATION.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:end
pause
exit