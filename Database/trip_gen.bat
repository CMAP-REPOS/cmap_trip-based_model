@echo off

rem trip_gen.bat
rem Craig Heither, CMAP
rem Matt Stratton, CMAP
rem Nick Ferguson, CMAP

echo This script:
echo   1) Converts UrbanSim outputs into Trip Generation model inputs.
echo   2) Uses UrbanSim land use data to develop trip allocation factors
echo      for heavy trucks.
echo   3) Runs the work-from-home allocation model.
echo   4) Runs the Trip Generation model.
echo   5) Creates the trip generation files to import into the emmebank
echo      and runs distribute.trucks and distribute.poes.

echo ===================================================================
echo.

rem Revision history
rem ----------------
rem 10/05/2018 Heither: Change Module 2 so truck.class.skim.mac is the
rem            primary truck distribution macro; use Python version of
rem            prepare_iom_inputs.
rem 07/16/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 09/15/2020 Ferguson: Replaced summary_tg_results.sas with
rem            summarize_tg_results.py.
rem 03/01/2021 Ferguson: Updated paths for removal os sas directory.
rem 05/14/2021 Heither: Include option to run both modules in one pass.
rem 11/08/2022 Ferguson: Removed call to activate_python_env.bat. Uses only conda env.
rem 02/20/2023 Heither: Read arguments from batch_file.yaml to run model.

rem ====================================================================

rem Settings
rem --------
rem In case CMD.exe is doing stuff in the wrong directory, this command
rem changes the directory to where the batch file was called from.
cd %~dp0
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set sc=%%a & goto break1)
:break1
for /f "eol=# skip=4 tokens=2 delims=:" %%b in (batch_file.yaml) do (set wfhFile=%%b & goto break2)
:break2
for /f "eol=# skip=5 tokens=2 delims=:" %%c in (batch_file.yaml) do (set wfh=%%c & goto break3)
:break3
for /f "eol=# skip=6 tokens=2 delims=:" %%d in (batch_file.yaml) do (set tc14=%%d & goto break4)
:break4
set sc=%sc:~1,3%
set wfhFile=%wfhFile:~1%
set wfh=%wfh:~1%
set tc14=%tc14:~1%
@echo.
@echo ========================================
@echo     --- Model Run Settings ---
@echo  Scenario = %sc%
@echo  Create WFH validation file = %wfhFile%
@echo  Usual WFH share = %wfh%
@echo  WFH 1-4 days share = %tc14%
@echo ========================================
@echo.
rem
set dt=%date%
set tm=%time%

echo Run module list:
echo   1) Run trip generation model ONLY (items 1-4 above).
echo   2) Import trip generation files into emmebank and run distribute
echo      macros ONLY (item 5 above).
echo   3) Run the trip generation model, import files into the emmebank
echo      and run distribute macros. Use this for UrbanSim data. (All items)
echo.

set /p choice="[SELECT A MODULE TO RUN] "
echo.
if not "%choice%"=="" (
    set choice=%choice:~0,1%
    if "%choice%"=="1" (goto one)
    if "%choice%"=="2" (goto two)
    if "%choice%"=="3" (goto notes)	
)

echo !!! "%choice%" IS NOT A VALID SELECTION !!!
echo Please resubmit the program and select a number from the module list.
goto end

:notes
rem Notes for running both modules. Move these up front.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
echo   SET GROWTH FACTOR
echo.
echo   Before continuing, please update the growth factor variable in
echo   each of the following scripts:
echo     * prep_macros\distribute.trucks
echo     * prep_macros\distribute.poes
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
echo   WORK FROM HOME RATES
echo.
echo   Do the WFH rates and industry files need to be updated?
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
echo.

rem -- Run trip generation model. --
:one
set /a val=%sc%
if %val% geq 100 (goto runit)
goto badscen

:runit
rem Supply project and run titles.
set /p project="[ENTER PROJECT TITLE (E.G., c20q2)] "
echo.
set /p run="[ENTER RUN TITLE (E.G., 100_20200330)] "
echo.

set project=%project: =%
set run=%run: =%

if "%project%" == "" (
    echo !!! NO PROJECT TITLE !!!
    echo Please resubmit the program and enter a project title.
    goto end
)
if "%run%" == "" (
    echo !!! NO RUN TITLE !!!
    echo Please resubmit the program and enter a run title.
    goto end
)

echo ===================================================================
echo.

rem Activate Python env
call %~dp0..\Scripts\manage\env\activate_env.cmd
if %ERRORLEVEL% NEQ 0 (goto end)

cd tg\scripts
echo Updating Trip Generation inputs with UrbanSim data ...
python urbansim_update_tg_input_files.py
if %ERRORLEVEL% NEQ 0 (goto urbansim_issue)
@echo.
echo Updating Heavy Truck Trip allocation weights with UrbanSim data ...
python urbansim_hcv_allocation.py
if %ERRORLEVEL% NEQ 0 (goto hcv_issue)
cd ..\fortran

rem -- Run the Work-from-Home procedures. --
set savedir="%cd%"
cd wfhmodule
set filedir="%cd%"
echo.
echo Starting the work-from-home allocation model ...
echo  wfh arguments: %filedir% %savedir% %wfhFile% %wfh% %tc14%
python wfhflag.py %filedir% %savedir% %wfhFile% %wfh% %tc14%
if %ERRORLEVEL% NEQ 0 (goto wfh_issue)
cd ..

rem Check for necessary input files.
if not exist HH_IN.TXT (goto socec_data_error)
if not exist GQ_IN.TXT (goto socec_data_error)
if not exist ATTR_IN.TXT (goto socec_data_error)
if not exist POPSYN_HH.CSV (goto socec_data_error)

rem Delete old output files.
if exist *OUT.TXT (del *OUT.TXT)
if exist *OUTPUT.TXT (del *OUTPUT.TXT)
if exist MCHW_HH.TXT (del MCHW_HH.TXT)

TG_PopSyn.exe
cd ..\scripts

echo Creating summary files ...
python summarize_tg_results.py %project% %run%
python prepare_iom_inputs.py %project% %run%
echo.

cd ..\data
if not exist hoa.in (goto python_error)
if not exist hop.in (goto python_error)
if not exist hwahi.in (goto python_error)
if not exist hwalo.in (goto python_error)
if not exist hwphi.in (goto python_error)
if not exist hwplo.in (goto python_error)
if not exist nha.in (goto python_error)
if not exist nhp.in (goto python_error)
if not exist m01tg.txt (goto python_error)
if not exist m01auto.csv (goto m01_data_error)
if not exist m01type.csv (goto m01_data_error)

cd %~dp0

python tg\fortran\create_HHvtype_file.py

echo Module 1 finished.
echo.
echo ===================================================================
echo.
if "%choice%"=="3" (goto macros)
goto last

rem Import production and attraction files, then run distribution macros.
:two
echo ===================================================================
echo.

rem Set distribution factors before running.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
echo   SET GROWTH FACTOR
echo.
echo   Before continuing, please update the growth factor variable in
echo   each of the following scripts:
echo     * prep_macros\distribute.trucks
echo     * prep_macros\distribute.poes
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
echo.

set /a val=%sc%
if %val% geq 100 (goto macros)
goto badscen

:macros
if exist tg.rpt (del tg.rpt)
if exist report\stop_truck_distribution.txt (
    del report\stop_truck_distribution.txt
)
if not exist tg\fortran\MCHW_HH.TXT (goto filemiss1)
copy tg\fortran\MCHW_HH.TXT MCHW_HH.TXT /y
copy tg\fortran\TG_HHENUM_OUTPUT.TXT TG_HHENUM_OUTPUT.TXT /y
echo.

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

rem Activate Emme Python env
call %~dp0..\Scripts\manage\env\activate_env.cmd emme
if %ERRORLEVEL% NEQ 0 (goto end)

echo Preparing emmebank for model run...
call python useful_macros\cleanup_for_rerun.py %file1% %val%>> tg.rpt
echo.

echo Importing production and attraction matrices (used only for b/l/m truck distribution)...
call emme -ng 000 -m prep_macros\import.tg.results 1 >> tg.rpt
echo.

echo Skimming highway network...
call python prep_macros\free.skim.mac.py %file1% %val%
echo.

echo Distributing trucks...
call emme -ng 000 -m prep_macros\truck.class.skim.mac %val% 1 1 >> tg.rpt
if exist report\stop_truck_distribution.txt (goto truck_balance_err)
echo.

echo Distributing POEs...
call emme -ng 000 -m prep_macros\distribute.poes 1 >> tg.rpt
echo.

cd %~dp0

echo Module 2 finished.
echo.
goto last

rem ------------------------------------------------------------
:socec_data_error
echo !!! MISSING SOCEC FILES !!!
echo Socioeconomic input files are missing from tg\fortran.
goto end

:m01_data_error
echo !!! MISSING DATA FILES !!!
echo M01 data files (m01auto.csv, m01type.csv) are missing from tg\data.
goto end

:python_error
echo !!! MISSING IOM FILES !!!
echo Review tg\scripts\prepare_iom_inputs.log for errors.
goto end

:badscen
echo !!! NO 3-DIGIT SCENARIO !!!
echo Please resubmit the program and supply a scenario number.
goto end

:urbansim_issue
echo !!! THE URBANSIM FILES DID NOT PROCESS PROPERLY !!!
goto end

:hcv_issue
echo !!! THE HEAVY COMMERCIAL VEHICLE ALLOCATION DID NOT PROCESS PROPERLY !!!
goto end

:wfh_issue
echo !!! THE WORK FROM HOME ALLOCATION MODEL DID NOT RUN PROPERLY !!!
goto end

:filemiss1
echo !!! tg\fortran\MCHW_HH.TXT DOES NOT EXIST !!!
goto end

:truck_balance_err
echo !!! TRUCK BALANCING PROCEDURES PRODUCED NaN ERRORS !!!
echo Please review report\truck.access.rpt, modify distribute.trucks and
echo resubmit trip_gen.bat
goto end

:CheckEmpty
if %~z1 == 0 (goto badfile)
goto filepass

:badfile
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND .EMP FILE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goto end

:last
echo End of batch file.
@ECHO Trip Generation Model Start Time: %dt% %tm%
@ECHO Trip Generation Model End Time  : %date% %time%

:end
echo.
pause
exit
