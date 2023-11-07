@echo off
REM Prepare data for TOD transit assignments.
REM  Heither, rev. 08-16-2023

@echo create_transit_demand.bat
@echo  Batch file does the following:
@echo   1) Create TOD transit network scenarios.
@echo   2) Create Emme matrices to hold TOD transit demand by value-of-time user class.
@echo   3) Fill the matrices with the TOD transit demand (where necessary trip origins
@echo      have been moved to boarding zones).
@echo ===================================================================
@echo.

cd %~dp0
cd ..
@echo.
rem -- Read model run settings from batch_file.yaml --
for /f "tokens=2 delims==" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=8 tokens=2 delims==" %%f in (batch_file.yaml) do (set transitAsmt=%%f & goto break2)
:break2
for /f "eol=# skip=11 tokens=2 delims==" %%b in (batch_file.yaml) do (set transitFilePath=%%b & goto break3)
:break3
for /f "eol=# skip=13 tokens=2 delims==" %%h in (batch_file.yaml) do (set selLineFile=%%h & goto break4)
:break4
for /f "eol=# skip=19 tokens=2 delims==" %%k in (batch_file.yaml) do (set RSPrun=%%k & goto break5)
:break5

set val=%val:~0,3%
set transitAsmt=%transitAsmt:~0,1%
set RSPrun=%RSPrun:~0,1%
@echo.
@echo ==============================================================
@echo     --- Model Run Settings ---
@echo  Scenario = %val%
@echo  Run transit assignment = %transitAsmt%
if "%transitAsmt%" EQU "T" (@echo  Location of transit network files = %transitFilePath%)
if "%transitAsmt%" EQU "T" (@echo  Transit assignment select line file = %selLineFile%)
@echo  RSP evaluation run = %RSPrun%
@echo ==============================================================
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

set /p ok="[RUN SETUP FOR SCENARIO %val%? (y/n)] "
@echo.
set ok=%ok:y=Y%
if not "%ok%"=="Y" (goto end)
if exist env (rmdir /S /Q env)
echo.

REM -- Build TOD transit networks
call emme -ng 000 -m transit_asmt_macros\setup_transit_asmt_1_build_transit_asmt_networks.mac %val% %transitFilePath%
if %ERRORLEVEL% NEQ 0 (goto issue)
REM -- Create matrices to hold TOD transit demand
%empypath% transit_asmt_macros/setup_transit_asmt_2_initialize_matrices.py %file1% %RSPrun%
if %ERRORLEVEL% NEQ 0 (goto issue)

rem ###############################################################################
rem The `CONDAPATH` environment variable should be set before running this .bat
rem It points to the place where conda is installed
rem Alternatively if running in a conda prompt itself then CONDA_PREFIX will be set
if defined CONDAPATH (
	goto condafound
)
if defined CONDA_PREFIX (
	set CONDAPATH=%CONDA_PREFIX%
	echo CONDA_PREFIX is %CONDAPATH%
	goto condafound
)
rem define here all the places where we might find the conda installation
rem If you try to run the model, you know that conda is installed, and the
rem model fails with "cannot find conda", then visit a conda prompt,
rem run `where conda`, and add the resulting path to this list.
for %%x in (
    %CONDAPATH%
    %CONDA_PREFIX%
    %LOCALAPPDATA%\mambaforge
    %LOCALAPPDATA%\miniforge
    %LOCALAPPDATA%\miniconda
    %LOCALAPPDATA%\miniconda3
    %LOCALAPPDATA%\Anaconda3
    %USERPROFILE%\Anaconda3
    %USERPROFILE%\Anaconda
    %USERPROFILE%\Anaconda2
    %USERPROFILE%\miniconda3
    %USERPROFILE%\miniconda
    %USERPROFILE%\miniconda2
) do (
    if exist %%x\Scripts\activate.bat (
      set CONDAPATH=%%x
      goto condafound
    )
)
@echo Cannot find conda in any of the usual places.
@echo CONDAPATH is not defined, first run set CONDAPATH=C:\... to point to the conda installation.
goto end

:condafound
@echo CONDAPATH IS %CONDAPATH%
@echo.

rem Define here the name of the environment to be used
set ENVNAME=CMAP-TRIP2

rem The following command prepares to activate the base environment if it is used.
if %ENVNAME%==base (set ENVPATH=%CONDAPATH%) else (set ENVPATH=%CONDAPATH%\envs\%ENVNAME%)

rem Activate the conda environment
rem Using call is required here, see: https://stackoverflow.com/questions/24678144/conda-environments-and-bat-files
call %CONDAPATH%\Scripts\activate.bat %ENVPATH%
if %errorlevel% neq 0 (
  @echo Error in activating conda
  goto end
)
@echo.
rem ###############################################################################

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

REM -- Adjust emmebank matrices if needed --
set /a trnAsmt=0
if "%transitAsmt%" EQU "T" (set /a trnAsmt+=1)
call %empypath% macros\verify_select_link.py %file1% "None" %RSPrun% %trnAsmt%
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

:issue
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO          THE LAST PROCEDURE DID NOT TERMINATE PROPERLY!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

:end
pause
exit
