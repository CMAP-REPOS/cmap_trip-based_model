@echo off
REM Prepare data for TOD transit assignments.
REM  Heither, rev. 04-25-2024 (improved logic for correctly reading transitFilePath)
@echo -------------------------------------------------------------------------------------------------
@echo create_transit_demand.bat
@echo  Batch file does the following:
@echo   1) Create TOD transit network scenarios.
@echo   2) Create Emme matrices to hold TOD transit demand by value-of-time user class.
@echo   3) Fill the matrices with the TOD transit demand (where necessary trip origins
@echo      have been moved to boarding zones).
@echo ===================================================================
@echo.
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
@echo.
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=11 tokens=2 delims=:" %%f in (batch_file.yaml) do (set transitAsmt=%%f & goto break2)
:break2
for /f "eol=# skip=14 tokens=2* delims=:" %%b in (batch_file.yaml) do (set transitFilePath1=%%b & set transitFilePath2=%%c & goto break3)
:break3
for /f "eol=# skip=16 tokens=2 delims=:" %%h in (batch_file.yaml) do (set selLineFile=%%h & goto break4)
:break4
for /f "eol=# skip=22 tokens=2 delims=:" %%k in (batch_file.yaml) do (set RSPrun=%%k & goto break5)
:break5

set val=%val:~1,3%
set transitAsmt=%transitAsmt:~1,1%
rem Construct complete transit file path
set transitFilePath1=%transitFilePath1:~1,-1%
set transitFilePath2=%transitFilePath2:~1,-1%
set sep=:\
set transitFilePath=%transitFilePath1%%sep%%transitFilePath2%
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
cd Database

set /p ok="[RUN SETUP FOR SCENARIO %val%? (y/n)] "
@echo.
set ok=%ok:y=Y%
if not "%ok%"=="Y" (goto end)
if exist env (rmdir /S /Q env)
echo.

REM -- Build TOD transit networks
call emme -ng 000 -m transit_asmt_macros\setup_transit_asmt_1_build_transit_asmt_networks.mac %val% %transitFilePath%
if %ERRORLEVEL% NEQ 0 (goto issue)

rem Activate Emme Python env
call %~dp0..\..\Scripts\manage\env\activate_env.cmd emme

REM -- Create matrices to hold TOD transit demand
python transit_asmt_macros/setup_transit_asmt_2_initialize_matrices.py %file1% %RSPrun%
if %ERRORLEVEL% NEQ 0 (goto issue)

rem Activate Python env
call %~dp0..\..\Scripts\manage\env\activate_env.cmd

REM -- Fill matrices with demand
python transit_asmt_macros\setup_transit_asmt_3_TOD_transit_demand.py %RSPrun%
if %ERRORLEVEL% NEQ 0 (goto issue)
@echo.
REM -- Clean up, if needed
set /a strat=%val% + 21
if exist STRATS_s%strat% (rmdir /S /Q STRATS_s%strat%)
set /a strat=%strat% + 2
if exist STRATS_s%strat% (rmdir /S /Q STRATS_s%strat%)
set /a strat=%strat% + 2
if exist STRATS_s%strat% (rmdir /S /Q STRATS_s%strat%)
set /a strat=%strat% + 2
if exist STRATS_s%strat% (rmdir /S /Q STRATS_s%strat%)

rem Activate Emme Python env
call %~dp0..\..\Scripts\manage\env\activate_env.cmd emme

REM -- Adjust emmebank matrices if needed --
set /a trnAsmt=0
if "%transitAsmt%" EQU "T" (set /a trnAsmt+=1)
call python macros\verify_select_link.py %file1% "None" %RSPrun% %trnAsmt%
if %ERRORLEVEL% GTR 0 (goto issue)

@echo ALL TOD TRANSIT ASSIGNMENT SETUP COMPLETED.
goto end

rem ===========================================================================
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

:issue
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO          THE LAST PROCEDURE DID NOT TERMINATE PROPERLY!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

:no_select_line_file
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    SELECT LINE FILE %selLineFile% IS SPECIFIED BUT DOES NOT EXIST.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:end
pause
exit
