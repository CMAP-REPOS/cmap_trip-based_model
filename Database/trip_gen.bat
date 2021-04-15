@echo off

rem trip_gen.bat
rem Craig Heither, CMAP
rem Matt Stratton, CMAP
rem Nick Ferguson, CMAP

echo This script:
echo   1) runs the Trip Generation model,
echo   2) creates the trip generation files to import into the emmebank
echo      and runs distribute.trucks/distribute.poes/airport.tg.
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

rem ====================================================================
rem Settings
rem --------
rem Just in case CMD.exe is doing stuff in the wrong directory,
rem this command changes the directory to where the batch file was called from.
cd %~dp0

echo Module list (each must be submitted separately):
echo.
echo     1. Run trip generation model.
echo     2. Import trip generation files into emmebank and run
echo        distribute macros.
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
@echo.
pause
goto end

@echo ====================================================================
@echo.

:one
rem Run trip generation model

echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
echo   Update the settings variables in each of the following scripts
echo   before continuing:
rem Heither 07-27-2017: not necessary when using PopSyn_HH.csv
rem echo !  - fortran\TG_INPUT.txt                               !
rem echo !    - CONTROL TOTALS MUST MATCH REGIONAL HH TOTALS     !
echo.
echo     - sas\cntl\summary_tg_results_popsyn.sas
echo     - sas\cntl\prepare_iom_inputs.py
echo.
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.
pause
echo.

call activate_python_env.bat
@echo.

REM Now find SAS executable
set infile=path.txt
if exist %infile% (del %infile% /Q)
dir "C:\Program Files\*sas.exe" /s /b >> %infile% 2>nul
dir "D:\Program Files\*sas.exe" /s /b >> %infile% 2>nul
dir "E:\Program Files\*sas.exe" /s /b >> %infile% 2>nul
set /p path2=<%infile%
set paren="
set saspath=%paren%%path2%%paren%
echo saspath = %saspath%
call :CheckEmpty2 %infile%
:saspass
if exist %infile% (del %infile% /Q)

cd tg\fortran
REM --- CHECK FOR NECESSARY INPUT FILES ---
if not exist HH_IN.txt (goto saserr)
if not exist GQ_IN.txt (goto saserr)
if not exist ATTR_IN.txt (goto saserr)
if not exist POPSYN_HH.CSV (goto saserr)
REM --- DELETE OLD OUTPUT FILES ---
if exist *OUT.TXT (del *OUT.TXT)
if exist *OUTPUT.TXT (del *OUTPUT.TXT)
if exist MCHW_HH.TXT (del MCHW_HH.TXT)
TG_PopSyn.exe
cd ..\sas\cntl
%saspath% -SYSIN summary_tg_results_popsyn
python prepare_iom_inputs.py
REM %saspath% -SYSIN prepare_iom_inputs
cd ..\..\..
if not exist tg\sas\data\hoa.in (goto saserr)
if not exist tg\sas\data\hop.in (goto saserr)
if not exist tg\sas\data\hwahi.in (goto saserr)
if not exist tg\sas\data\hwalo.in (goto saserr)
if not exist tg\sas\data\hwphi.in (goto saserr)
if not exist tg\sas\data\hwplo.in (goto saserr)
if not exist tg\sas\data\nha.in (goto saserr)
if not exist tg\sas\data\nhp.in (goto saserr)
if not exist tg\sas\data\m01auto.csv (goto saserr)
if not exist tg\sas\data\m01tg.txt (goto saserr)
if not exist tg\sas\data\m01type.csv (goto saserr)
CD %~dp0
ECHO.
REM --- SUBMIT THE FOLLOWING ONLY IF MODEL RUN IN TRIP GENERATION MODE ---
if exist tg\fortran\MCHW_HH.TXT (python tg\fortran\create_HHvtype_file.py)
ECHO.
ECHO ~~~~~~~~~~~~~~~~~~~
ECHO Module 1 finished.
ECHO ~~~~~~~~~~~~~~~~~~~
goto last

@ECHO ======================================================================
REM IMPORT PRODUCTION AND ATTRACTION FILES, THEN RUN DISTRIBUTION MACROS
:two
ECHO.
ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO ! You must be connected to an Emme  !
ECHO ! license before continuing.        !
ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO.
pause
ECHO.

REM SET DISTRIBUTION FACTORS BEFORE RUNNING
ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO ! Update variables in:      !
ECHO !  - distribute.trucks      !
ECHO !  - distribute.poes        !
ECHO !  - airport.tg             !
ECHO ! before continuing.        !
ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO.
pause
ECHO.

REM SUPPLY 3-DIGIT SCENARIO TO RUN MACROS
set sc=
set /P sc=[ENTER 3-DIGIT SCENARIO NUMBER, ex: 400 ]
ECHO.
if not '%sc%'=='' set sc=%sc:~0,3%
if not '%sc%'=='' goto next
goto badscen

:next
set /A val=%sc%
if %val% GEQ 100 goto macros
goto badscen

:macros
if exist tg.rpt (del tg.rpt)
if exist report\stop_truck_distribution.txt (del report\stop_truck_distribution.txt)
if not exist tg\fortran\MCHW_HH.TXT (goto filemiss1)
copy tg\fortran\MCHW_HH.TXT MCHW_HH.TXT /y
copy tg\fortran\TG_HHENUM_OUTPUT.TXT TG_HHENUM_OUTPUT.TXT /y

ECHO PREPARING EMMEBANK FOR MODEL RUN ...
call emme -ng 000 -m useful_macros\cleanup.for.rerun %val% 1 >> tg.rpt
ECHO.
ECHO IMPORTING PRODUCTION AND ATTRACTION MATRICES ...
call emme -ng 000 -m prep_macros\import.tg.results 1 >> tg.rpt
ECHO.
ECHO SKIMMING HIGHWAY NETWORK ...
call emme -ng 000 -m prep_macros\free.skim.mac %val% 1 >> tg.rpt
ECHO.
ECHO DISTRIBUTING TRUCKS ...
call emme -ng 000 -m prep_macros\truck.class.skim.mac %val% 1 1 >> tg.rpt
if exist report\stop_truck_distribution.txt (goto truck_balance_err)
ECHO.
ECHO DISTRIBUTING POEs ...
call emme -ng 000 -m prep_macros\distribute.poes 1 >> tg.rpt
ECHO.
ECHO SETTING AIRPORT WORK ATTRACTIONS ...
call emme -ng 000 -m prep_macros\airport.tg 1 >> tg.rpt
CD %~dp0
ECHO ~~~~~~~~~~~~~~~~~~~
ECHO Module 2 finished.
ECHO ~~~~~~~~~~~~~~~~~~~
goto last

REM ======================================================================
:CheckEmpty2
if %~z1 == 0 (goto badsas)
goto saspass

:saserr
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO  MISSING P-A FILES:
@ECHO  REVIEW SAS LOGS FOR ERRORS!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:badscen
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO  NO 3-DIGIT SCENARIO, RESUBMIT PROGRAM!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:badsas
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND SAS INSTALLED ON THIS MACHINE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:filemiss1
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO tg\fortran\MCHW_HH.TXT DOES NOT EXIST!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:truck_balance_err
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO   Truck Balancing Procedures Produced NaN Errors!!
@ECHO     -- Review report\truck.access.rpt, modify distribute.trucks and
@ECHO        resubmit trip_gen.bat!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:last
ECHO.
ECHO end of batch file
pause

:end
exit
