@echo off
REM Run transit assignment.
REM  Heither, rev. 03-18-2023

cd %~dp0
cd ..
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo.
@echo   BEFORE CONTINUING:
@echo.
@echo   - Please connect to an Emme license.
@echo.
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
rem -- Read model run settings from batch_file.yaml --
for /f "tokens=2 delims==" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=9 tokens=2 delims==" %%b in (batch_file.yaml) do (set transitFilePath=%%b & goto break2)
:break2
set val=%val:~0,3%
@echo.
@echo ========================================
@echo     --- Model Run Settings ---
@echo  Scenario = %val%
@echo ========================================
pause

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
cd Database/transit_asmt_macros
if exist usemacro_* (del usemacro_* /Q)

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

REM -- Submit with name of .emp file 
%empypath% cmap_transit_assignment_runner.py %file1% 1
REM -- Summarize transit boardings
cd ..
set /a val=%val% + 21
call emme -ng 000 -m transit_asmt_macros/summarize_transit_boardings.mac %val%
echo.
REM -- Delete transit assignment matrices
%empypath% transit_asmt_macros/delete_transit_skims.py %file1%
@echo.
@echo MATRICES DELETED.
goto last

REM ======================================================================
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

:last
@ECHO ==================================================================
@ECHO            TRANSIT ASSIGNMENT COMPLETED %date% %time%
@ECHO ==================================================================
@ECHO.

:end
pause
exit



