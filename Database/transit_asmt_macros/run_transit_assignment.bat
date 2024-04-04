@echo off
REM Run transit assignment.
REM  Heither, rev. 01-19-2024

@echo -------------------------------------------------------------------------------------------------
@echo To run a transit assignment, use the following settings in batch_file.yaml:
@echo    - scenario: set to appropriate value
@echo    - runTransitAsmt: set to True
@echo    - transit_file_path: include the file path to transit transaction files
@echo.
@echo To add a select line analysis to the transit assignment:
@echo    - transitSelectFile: transit select line analysis file in Database\Select_Line
@echo                        (provide a file name with "_", like rsp57_line.txt or metra_lines.txt) 
@echo.
@echo To add an analysis of HBW demand (in addition to all demand) to the select line analysis:
@echo    - RSP: set to True [it doesn't matter if it is an actual RSP, this merely sets a flag]
@echo -------------------------------------------------------------------------------------------------

cd %~dp0
cd ..
echo.
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=11 tokens=2 delims=:" %%f in (batch_file.yaml) do (set transitAsmt=%%f & goto break2)
:break2
for /f "eol=# skip=14 tokens=2 delims=:" %%b in (batch_file.yaml) do (set transitFilePath=%%b & goto break3)
:break3
for /f "eol=# skip=16 tokens=2 delims=:" %%h in (batch_file.yaml) do (set selLineFile=%%h & goto break4)
:break4
for /f "eol=# skip=22 tokens=2 delims=:" %%k in (batch_file.yaml) do (set RSPrun=%%k & goto break5)
:break5

set val=%val:~1,3%
set transitAsmt=%transitAsmt:~1,1%
set transitFilePath=%transitFilePath:~1%
set selLineFile=%selLineFile:~1%
set RSPrun=%RSPrun:~1,1%
@echo.
@echo ==============================================================
@echo     --- Model Run Settings ---
@echo  Scenario = %val%
@echo  Run transit assignment = %transitAsmt%
if "%transitAsmt%" EQU "T" (@echo  Location of transit network files = %transitFilePath%)
if "%transitAsmt%" EQU "T" (@echo  Transit assignment select line file = %selLineFile%)
@echo  RSP evaluation run = %RSPrun%
@echo ==============================================================

set /a trnAsmt=0
if "%transitAsmt%" EQU "T" (set /a trnAsmt+=1)
set check2=%selLineFile:~0,4%
REM Remove trailing spaces from transitFilePath
set transitFilePath=%transitFilePath:~0,-1%

if "%check2%" NEQ "None" (
    if not exist Select_Line\%selLineFile% (goto no_select_line_file)
)
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
%empypath% cmap_transit_assignment_runner.py %file1% 1 %val%
REM -- Summarize transit boardings
cd ..
set /a val21=%val%+21
call emme -ng 000 -m transit_asmt_macros/summarize_transit_boardings.mac %val21%
echo.
REM -- Delete transit assignment matrices
%empypath% transit_asmt_macros/delete_transit_skims.py %file1%
@echo.
@echo MATRICES DELETED.

if "%check2%" NEQ "None" (
        REM -- Run select line analysis
        call %empypath% transit_asmt_macros\transit_select_line.py %file1% %val% %selLineFile%
        if %ERRORLEVEL% GTR 0 (goto issue)
        @ECHO -- Completed Select Line Analysis 
        REM -- Summarize select line boardings
        call %empypath% transit_asmt_macros\select_line_boardings.py %file1% %val% %RSPrun% %selLineFile%
        if %ERRORLEVEL% GTR 0 (goto issue)
        @ECHO -- Completed Select Line Boarding Analysis
    )
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

:no_select_line_file
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    SELECT LINE FILE %selLineFile% IS SPECIFIED BUT DOES NOT EXIST.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
