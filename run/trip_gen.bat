REM trip_gen.bat
@ECHO off
ECHO #################################################
REM Craig Heither, rev. 03-09-2018
REM Updates by Matt Stratton, 06/15/10
REM   Heither, 09/15/10 - airport.tg replaces zero.out.airports, del MCHW_HH.TXT if needed
REM   Heither, 03/11/11 - logic changed so each Module is independent & must be submitted separately.
REM   Heither, 12/07/11 - additional saspath logic added; verify ALL required files in ..\tg\sas\data;
REM                       copy MCHW_HH.TXT to Database folder.
REM   Heither, 10/23/14 - various housekeeping (update SAS path for version 9.4; drop check for Windows XP;
REM				renumber modules); add Python call for script create_HHvtype_file.py;
REM				updated arguments added to call distribute.trucks macro
REM   Heither, 04/21/15 - revised TG file cleanup; conditional execution of python script (trip generation mode only)
REM   Heither, 06/30/15 - additional error-checking logic to trap NaN errors in matrix balancing 
REM   Heither, 05/25/16 - modified to call new tg executable (TG_PopSyn.exe) & verify existence of POPSYN_HH.CSV
REM   Heither, 03-09-2018 - Automatically find Python and SAS executables.
REM   Heither, 10-05-2018 - Change Module 2 so truck.class.skim.mac is the primary truck distribution macro; use Python version of prepare_iom_inputs.
REM
ECHO.
ECHO This script:
ECHO   1) runs the Trip Generation model, 
ECHO   2) creates the trip generation files to import into the emmebank
ECHO      and runs distribute.trucks/distribute.poes/airport.tg.
ECHO.
ECHO #################################################
ECHO.

REM - Just in case CMD.exe is doing stuff in the wrong directory,
REM - this command changes the directory to where the batch file was called from.
CD %~dp0

ECHO      Choose a Module to Run (Each must be submitted separately):
ECHO -----------------------------------------------------------------------
ECHO 1. Run trip generation model.
ECHO 2. Import trip generation files into emmebank and run distribute macros.
ECHO.

set choice=
set /p choice=[SELECT THE RUN MODE. ]
if not '%choice%'=='' set choice=%choice:~0,1%
if '%choice%'=='1' goto one
if '%choice%'=='2' goto two
ECHO "%choice%" is not valid, please resubmit the program with a new argument
pause
goto end

@ECHO ======================================================================
REM RUN TRIP GENERATION MODEL
:one

ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO ! Update variables in:                                  !
REM Heither 07-27-2017: not necessary when using PopSyn_HH.csv
REM ECHO !  - fortran\TG_INPUT.txt                               !
REM ECHO !    - CONTROL TOTALS MUST MATCH REGIONAL HH TOTALS     !
ECHO !  - sas\cntl\summary_tg_results_popsyn.sas             !
ECHO !  - sas\cntl\prepare_iom_inputs.py                     !
ECHO !    before continuing.                                 !
ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
ECHO.
pause

CD %~dp0

REM  Search for viable Python executables, write paths to file, redirect errors to nul in case file not found, read first path from file
set infile=path.txt
if exist %infile% (del %infile% /Q)
dir "C:\Python27\*python.exe" /s /b >> %infile% 2>nul
dir "D:\Python27\*python.exe" /s /b >> %infile% 2>nul
dir "E:\Python27\*python.exe" /s /b >> %infile% 2>nul
dir "C:\Program Files\INRO\*python.exe" /s /b >> %infile% 2>nul
dir "D:\Program Files\INRO\*python.exe" /s /b >> %infile% 2>nul
dir "E:\Program Files\INRO\*python.exe" /s /b >> %infile% 2>nul
set /p path1=<%infile%
set paren="
set pypath=%paren%%path1%%paren%
echo pypath = %pypath%
call :CheckEmpty %infile%
:pythonpass

REM Now find SAS executable
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
%pypath% prepare_iom_inputs.py
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
if exist tg\fortran\MCHW_HH.TXT (%pypath% tg\fortran\create_HHvtype_file.py)
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
:CheckEmpty
if %~z1 == 0 (goto badpython)
goto pythonpass

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

:badpython
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND PYTHON INSTALLED ON THIS MACHINE.
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
