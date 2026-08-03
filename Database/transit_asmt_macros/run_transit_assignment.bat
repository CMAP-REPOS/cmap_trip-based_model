@echo off
REM Run transit assignment.
REM  Heither, rev. 10-21-2024 (updated for c24q4)
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

cd ..
@echo %cd%
echo.
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=10 tokens=2 delims=:" %%f in (batch_file.yaml) do (set transitAsmt=%%f & goto break4)
:break4
for /f "eol=# skip=12 tokens=2 delims=:" %%i in (batch_file.yaml) do (set selLineFile=%%i & goto break5)
:break5
for /f "eol=# skip=18 tokens=2 delims=:" %%l in (batch_file.yaml) do (set RSPrun=%%l & goto break8)
:break8

set val=%val:~1,3%
set transitAsmt=%transitAsmt:~1,1%
set selLineFile=%selLineFile:~1%
set RSPrun=%RSPrun:~1,1%
@echo.
@echo ==============================================================
@echo     --- Model Run Settings ---
@echo  Scenario = %val%
@echo  Run transit assignment = %transitAsmt%
if "%transitAsmt%" EQU "T" (@echo  Transit assignment select line file = %selLineFile%)
@echo  RSP evaluation run = %RSPrun%
@echo ==============================================================

set /a trnAsmt=0
if "%transitAsmt%" EQU "T" (set /a trnAsmt+=1)
set check2=%selLineFile:~0,4%

if "%check2%" NEQ "None" (
    if not exist Select_Line\%selLineFile% (goto no_select_line_file)
)

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
if exist usemacro_* (del usemacro_* /Q)

@ECHO Begin Transit Assignment setup: %date% %time% >> model_run_timestamp.txt
REM -- Create matrices to hold TOD transit demand
if "%RSPrun%" EQU "T" (@ECHO -- Creating HBW transit demand matrices >> model_run_timestamp.txt)
uv run transit_asmt_macros/setup_transit_asmt_2_initialize_matrices.py %file1% %RSPrun%
if %ERRORLEVEL% NEQ 0 (goto issue)
REM -- Fill matrices with demand (point to conda environment)
uv run transit_asmt_macros/setup_transit_asmt_3_TOD_transit_demand.py %RSPrun%
if %ERRORLEVEL% NEQ 0 (goto issue)
@ECHO End Transit Assignment setup >> model_run_timestamp.txt
@ECHO Submit Transit Assignment >> model_run_timestamp.txt 
cd transit_asmt_macros
uv run cmap_transit_assignment_runner.py %file1% 1 %val%
if %ERRORLEVEL% GTR 0 (goto issue)
REM -- Summarize transit boardings
cd ..
set /a val21=%val%+21
uv run transit_asmt_macros\summarize_transit_boardings.py %val21%
if %ERRORLEVEL% GTR 0 (goto issue)
@echo.
REM -- Delete transit assignment matrices
uv run transit_asmt_macros\delete_transit_skims.py %file1%
if %ERRORLEVEL% GTR 0 (goto issue)
if "%check2%" NEQ "None" (
        REM -- Run select line analysis
        uv run transit_asmt_macros\transit_select_line.py %file1% %val% %selLineFile%
        if %ERRORLEVEL% GTR 0 (goto issue)
        @ECHO -- Completed Select Line Analysis >> model_run_timestamp.txt
        REM -- Summarize select line boardings
        uv run transit_asmt_macros\select_line_boardings.py %file1% %val% %RSPrun% %selLineFile%
        if %ERRORLEVEL% GTR 0 (goto issue)
        @ECHO -- Completed Select Line Boarding Analysis >> model_run_timestamp.txt
    )
@ECHO End Transit Assignment: %date% %time% >> model_run_timestamp.txt
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
