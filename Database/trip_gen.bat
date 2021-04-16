@echo off

rem trip_gen.bat
rem Craig Heither, CMAP
rem Matt Stratton, CMAP
rem Nick Ferguson, CMAP

echo This script:
echo   1) Runs the Trip Generation model.
echo   2) Creates the trip generation files to import into the emmebank
echo      and runs distribute.trucks, distribute.poes, and airport.tg.
echo.
echo ===================================================================
echo.

rem Revision history
rem ----------------
rem 09/15/2010 Heither: airport.tg replaces zero.out.airports, del
rem            MCHW_HH.TXT if needed
rem 03/11/2011 Heither: logic changed so each Module is independent &
rem            must be submitted separately.
rem 12/07/2011 Heither: additional saspath logic added; verify ALL
rem            required files in ..\tg\sas\data; copy MCHW_HH.TXT to
rem            Database folder.
rem 10/23/2014 Heither: various housekeeping (update SAS path for
rem            version 9.4; drop check for Windows XP; renumber
rem            modules); add Python call for script
rem            create_HHvtype_file.py; updated arguments added to call
rem            distribute.trucks macro
rem 04/21/2015 Heither: revised TG file cleanup; conditional execution
rem            of python script (trip generation mode only)
rem 06/30/2015 Heither: additional error-checking logic to trap NaN
rem            errors in matrix balancing
rem 05/25/2016 Heither: modified to call new tg executable
rem            (TG_PopSyn.exe) & verify existence of POPSYN_HH.CSV
rem 03/09/2018 Heither: Automatically find Python and SAS executables.
rem 10/05/2018 Heither: Change Module 2 so truck.class.skim.mac is the
rem            primary truck distribution macro; use Python version of
rem            prepare_iom_inputs.
rem 07/16/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 09/15/2020 Ferguson: Replaced summary_tg_results.sas with
rem            summarize_tg_results.py.
rem 03/01/2021 Ferguson: Updated paths for removal os sas directory.

rem ====================================================================

rem Settings
rem --------
rem In case CMD.exe is doing stuff in the wrong directory, this command
rem changes the directory to where the batch file was called from.
cd %~dp0

echo Module list (each must be submitted separately):
echo   1) Run trip generation model.
echo   2) Import trip generation files into emmebank and run distribute
echo      macros.
echo.

set /p choice="[SELECT A MODULE TO RUN] "
echo.
if not "%choice%"=="" (
    set choice=%choice:~0,1%
    if "%choice%"=="1" (goto one)
    if "%choice%"=="2" (goto two)
)

echo !!! "%choice%" IS NOT A VALID SELECTION !!!
echo Please resubmit the program and select a number from the module list.
echo.
pause
goto end

rem Run trip generation mode.
:one
echo ===================================================================
echo.

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
    echo.
    pause
    goto end
)
if "%run%" == "" (
    echo !!! NO RUN TITLE !!!
    echo Please resubmit the program and enter a run title.
    echo.
    pause
    goto end
)

echo ===================================================================
echo.

call activate_python_env.bat
@echo.

cd tg\fortran

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

rem Submit the following only if the TG model was run in trip generation
rem mode.
if exist tg\fortran\MCHW_HH.TXT (
    python tg\fortran\create_HHvtype_file.py
    echo.
)

echo Module 1 finished.
echo.
echo ===================================================================
echo.
goto last

rem Import production and attraction files, then run distribution
rem macros.
:two
echo ===================================================================
echo.

echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
echo   CONNECT TO EMME
echo.
echo   Before continuing, please connect to an Emme license.
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
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
echo     * prep_macros\airport.tg
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
echo.

rem Supply 3-digit scenario to run macros.
set sc=
set /p sc="[ENTER 3-DIGIT SCENARIO NUMBER (E.G., 400)] "
echo.
if not '%sc%'=='' (set sc=%sc:~0,3%)
if not '%sc%'=='' (goto next)
goto badscen

:next
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

echo Preparing emmebank for model run...
call emme -ng 000 -m useful_macros\cleanup.for.rerun %val% 1 >> tg.rpt
echo.

echo Importing production and attraction matrices...
call emme -ng 000 -m prep_macros\import.tg.results 1 >> tg.rpt
echo.

echo Skimming highway network...
call emme -ng 000 -m prep_macros\free.skim.mac %val% 1 >> tg.rpt
echo.

echo Distributing trucks...
call emme -ng 000 -m prep_macros\truck.class.skim.mac %val% 1 1 >> tg.rpt
if exist report\stop_truck_distribution.txt (goto truck_balance_err)
echo.

echo Distributing POEs...
call emme -ng 000 -m prep_macros\distribute.poes 1 >> tg.rpt
echo.

echo Setting airport work attractions...
call emme -ng 000 -m prep_macros\airport.tg 1 >> tg.rpt
echo.

cd %~dp0

echo Module 2 finished.
echo.
goto last

:socec_data_error
echo !!! MISSING SOCEC FILES !!!
echo Socioeconomic input files are missing from tg\fortran.
echo
pause
goto end

:m01_data_error
echo !!! MISSING DATA FILES !!!
echo M01 data files (m01auto.csv, m01type.csv) are missing from tg\data.
echo
pause
goto end

:python_error
echo !!! MISSING IOM FILES !!!
echo Review tg\scripts\prepare_iom_inputs.log for errors.
echo.
pause
goto end

:badscen
echo !!! NO 3-DIGIT SCENARIO !!!
echo Please resubmit the program and supply a scenario number.
echo.
pause
goto end

:filemiss1
echo !!! tg\fortran\MCHW_HH.TXT DOES NOT EXIST !!!
echo.
pause
goto end

:truck_balance_err
echo !!! TRUCK BALANCING PROCEDURES PRODUCED NaN ERRORS !!!
echo Please review report\truck.access.rpt, modify distribute.trucks and
echo resubmit trip_gen.bat
echo.
pause
goto end

:last
echo End of batch file.
echo.
pause


:end
exit
