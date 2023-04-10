@echo off

rem Submit_Full_Regional_Model_SOLA.bat
rem Craig Heither, CMAP
rem Nick Ferguson, CMAP
rem Karly Cazzato, CMAP

@echo BATCH FILE TO SUBMIT CMAP REGIONAL TRAVEL DEMAND MODEL
@echo   - 3 GLOBAL ITERATIONS
@echo   - 5 AUTO TRIP PURPOSES: HW LOW INCOME, HW HIGH INCOME, HB Shop, HO, NH
@echo   - SECOND ORDER LINEAR APPROXIMATION (SOLA) HIGHWAY ASSIGNMENT (7 VEHICLE CLASSES)
@echo       Class 1: 1 PERSON SOV LOW VOT (ALL PURPOSES)
@echo       Class 2: 1 PERSON SOV MED VOT (ALL PURPOSES)
@echo       Class 3: 1 PERSON SOV HIGH VOT (ALL PURPOSES)
@echo       Class 4: 2+ PERSON HOV (ALL PURPOSES)
@echo       Class 5: B-PLATE + LIGHT DUTY TRUCK
@echo       Class 6: MEDIUM DUTY TRUCK
@echo       Class 7: HEAVY DUTY TRUCK
@echo ==================================================================
@echo.
rem Revision history
rem ----------------
rem 07/16/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 02/25/2023 Heither: Read parameters from batch_file.yaml
rem 03/23/2023 Cazzato: Run create DISTR/M01 files 
rem ====================================================================
cd %~dp0
rem -- Read model run settings from batch_file.yaml --
for /f "tokens=2 delims==" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=2 tokens=2 delims==" %%b in (batch_file.yaml) do (set wfhFile=%%b & goto break2)
:break2
for /f "eol=# skip=3 tokens=2 delims==" %%c in (batch_file.yaml) do (set wfh=%%c & goto break3)
:break3
for /f "eol=# skip=4 tokens=2 delims==" %%d in (batch_file.yaml) do (set tc14=%%d & goto break4)
:break4
for /f "eol=# skip=6 tokens=2 delims==" %%e in (batch_file.yaml) do (set selLinkFile=%%e & goto break5)
:break5
set val=%val:~0,3%
@echo.
@echo ========================================
@echo     --- Model Run Settings ---
@echo  Scenario = %val%
@echo  Create WFH validation file = %wfhFile%
@echo  Usual WFH share = %wfh%
@echo  WFH 1-4 days share = %tc14%
@echo  Select Link file = %selLinkFile%
@echo ========================================
set check=%selLinkFile:~0,4%
if "%check%" NEQ "None" (
    if not exist Select_Link\%selLinkFile% (goto no_select_link_file)
)

REM Clean up prior to run
if exist cache\choice_simulator_trips_out (rmdir /S /Q cache\choice_simulator_trips_out)
if exist cache\choice_simulator_trips_out.001 (rmdir /S /Q cache\choice_simulator_trips_out.001)
if exist cache\choice_simulator_trips_out.002 (rmdir /S /Q cache\choice_simulator_trips_out.002)
if exist cache\choice_simulator_trips_out.003 (rmdir /S /Q cache\choice_simulator_trips_out.003)
if exist cache\choice_simulator_trips_out.004 (rmdir /S /Q cache\choice_simulator_trips_out.004)
del cache\logs\*.* /Q    
if exist usemacro_* (del usemacro_* /Q)
if exist errors (del errors /Q)

@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo.
@echo   CONNECT TO EMME
@echo.
@echo   Before continuing, please connect to an Emme license.
@echo.
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
@echo.

echo.
echo Select Destination Choice-Mode Choice model run mode:
echo   1) Minimize run time (default) - resources allocated to support a single model run.
echo   2) Balanced - resources allocated to support two simultaneous model runs.
echo.
set /a jobs=40
set /a zones=10
set /a sola_threads=63
set /p choice="[SELECT A MODEL RUN MODE] "
echo.
if not "%choice%"=="" (
    set choice=%choice:~0,1%
    if "%choice%"=="1" (goto proceed)
    if "%choice%"=="2" (
		set /a jobs=12
		set /a zones=7
		set /a sola_threads=31
		goto proceed
	)
)
:proceed
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
@echo cannot find conda in any of the usual places.
@echo CONDAPATH is not defined, first run set CONDAPATH=C:\... to point to the conda installation.
goto end

:condafound
@echo CONDAPATH IS %CONDAPATH%
@echo.

@echo Model run scenario: %val%
@echo.

set /p ok="[RUN MODEL FOR SCENARIO %val%? (y/n)] "
@echo.
set ok=%ok:y=Y%
if not "%ok%"=="Y" (goto end)
@echo ==================================================================
@echo.

rem Define here the name of the environment to be used
set ENVNAME=CMAP-TRIP

rem The following command prepares to activate the base environment if it is used.
if %ENVNAME%==base (set ENVPATH=%CONDAPATH%) else (set ENVPATH=%CONDAPATH%\envs\%ENVNAME%)

rem Activate the conda environment
rem Using call is required here, see: https://stackoverflow.com/questions/24678144/conda-environments-and-bat-files
call %CONDAPATH%\Scripts\activate.bat %ENVPATH%
if %errorlevel% neq 0 (
  @echo Error in activating conda
  goto end
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

REM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
copy tg\fortran\TG_HHENUM_OUTPUT.TXT TG_HHENUM_OUTPUT.TXT /y
copy tg\fortran\TRIP49_PA_OUT.TXT defaults_base_year\TRIP49_PA_OUT.TXT /y
copy tg\fortran\TRIP49_PA_WFH_OUT.TXT defaults_base_year\TRIP49_PA_WFH_OUT.TXT /y

echo.
if not exist TG_HHENUM_OUTPUT.TXT (goto hhmiss)
REM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM Craig Heither, 4/13/09
REM   Prep steps added to ensure matrix/scenario cleanup is done
REM   and highway network is skimmed prior to submitting full run.
REM   Error trapping ensures macros run if matrix/scenario does not exist.
REM
REM #################################################

if exist blog.txt (del blog.txt /Q)
if exist model_run_timestamp.txt (del model_run_timestamp.txt /Q)

@ECHO ============================================================= >> model_run_timestamp.txt
@ECHO BEGIN CMAP REGIONAL MODEL RUN - SCENARIO %val% >> model_run_timestamp.txt
@ECHO Model Run Start Time: %date% %time% >> model_run_timestamp.txt
@ECHO  -- Number of jobs: %jobs%   -- Number of zones per job: %zones% >> model_run_timestamp.txt
@ECHO ============================================================= >> model_run_timestamp.txt

REM DETERMINE IF CONGESTED TRAVEL TIME & DISTANCE FILES EXIST TO PRE-LOAD REASONABLE CONGESTION
set /A pld=0
if exist defaults_base_year\preload_mf44.txt (set /A pld=pld+1)
if exist defaults_base_year\preload_mf45.txt (set /A pld=pld+1)
if exist defaults_base_year\preload_mf46.txt (set /A pld=pld+1)
if exist defaults_base_year\preload_mf47.txt (set /A pld=pld+1)
if exist defaults_base_year\preload_mf76.txt (set /A pld=pld+1)
if exist defaults_base_year\preload_mf77.txt (set /A pld=pld+1)
if exist defaults_base_year\per3_timau.txt (set /A pld=pld+1)
if exist defaults_base_year\per5_timau.txt (set /A pld=pld+1)
set /A preload=0
if %pld% EQU 8 (set /A preload=1)

REM PREP WORK
@ECHO Update DISTR and M01 files
CD %~dp0
CD prep_macros

REM Now find R executable
set infile=path.txt
if exist %infile% (del %infile% /Q)
dir "C:\Program Files\R\*R.exe" /s /b >> %infile% 2>nul
set /p path2=<%infile%
set paren="
set rpath=%paren%%path2%%paren%
echo rpath = %rpath%
call :CheckEmpty2 %infile%
:Rpass
if exist %infile% (del %infile% /Q)
set rfile=create_distr_m01_files

REM -- Start DISTR & M01 Data Processing --
@ECHO.
@ECHO Start Time: %date% %time%
@ECHO.
if exist report.txt (del report.txt /Q)
if exist temp\nul (rmdir temp /S /Q)
if not exist temp (mkdir temp)
cd ..
if not exist tg\fortran\MCHW_HH.TXT (goto filemiss1)
copy tg\fortran\MCHW_HH.TXT MCHW_HH.TXT /y
if not exist tg\data\m01auto.csv (goto filemiss2)
if not exist tg\data\m01tg.txt (goto filemiss2)
if not exist tg\data\m01type.csv (goto filemiss2)
@ECHO.
@ECHO -- OBTAINING TRANSIT NETWORK DATA FROM EMME --
call emme -ng 000 -m prep_macros\distr_m01_data.mac %val% >> prep_macros\report.txt
if %ERRORLEVEL% GTR 0 (goto issue)
cd prep_macros
@ECHO.
@ECHO -- CREATING FILES FOR SPATIAL ANALYSIS --
@ECHO.
%rpath% CMD BATCH %rfile%.R
if %ERRORLEVEL% GTR 0 (goto issue)

@ECHO.
@ECHO -- DISTR AND M01 FILES CREATED --
@ECHO.
@ECHO End Time: %date% %time%
@ECHO.
if exist temp\nul (rmdir temp /S /Q)
if exist report.txt (del report.txt /Q)
CD ..

@ECHO   ***  Cleaning up databank.  ***
if exist cleanup.rpt (del cleanup.rpt)
call emme -ng 000 -m useful_macros\cleanup.for.rerun %val% 2 >> cleanup.rpt
if exist reports (del reports)

REM RUN FREESKIM TO CREATE TIME, DISTANCE AND TOLL MATRICES
@ECHO.
@ECHO   ***  Skimming highway network.  ***
call emme -ng 000 -m prep_macros\free.skim.mac %val% 2 >> blog.txt
@ECHO.

REM IF PRELOAD=1, REPLACE UNCONGESTED TIME AND DISTANCE MATRICES
if %preload% EQU 1 (@echo   ***  Preloading congested times and distances.  ***)
if %preload% EQU 1 (call emme -ng 000 -m prep_macros\preload_congested_times.mac %val% >> blog.txt)


@ECHO ==================================================================
REM - LOOP TO RUN MODEL
set /A counter=0
:while
if %counter% GTR 2 (goto loopend)
@ECHO -- Begin Transit skim Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TRANSIT SKIM - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\skim.transit.all %val% %counter% python >> blog.txt
if %ERRORLEVEL% neq 0 (goto issue)
@ECHO    -- End of Transit Skim Procedures: %date% %time% >> model_run_timestamp.txt

@ECHO Begin Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt
@ECHO PREPARING EMMEBANK - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rem @ECHO on
call emme -ng 000 -m macros\init_HOVsim_databk.mac %val% >> blog.txt

@ECHO -- Begin Mode-Destination Choice Procedures: %date% %time% >> model_run_timestamp.txt

@ECHO.
@ECHO RUN CMAP MODE-DESTINATION CHOICE MODEL - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

call cmap_modedest . --njobs %jobs% --max_zone_chunk %zones%
if %ERRORLEVEL% NEQ 0 (goto issue)

rem del cache\choice_simulator_trips_out\choice_simulator_util_*.pq /Q
@ECHO    -- End Mode-Destination Choice Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.

@ECHO -- Begin Time-of-Day Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TOD HIGHWAY ASSIGNMENT - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
REM -- Loop through time-of-day procedures --
set /A tod_cntr=1
:tod_loop
REM -- Flag for periods when toll skimming is run --
set toll=F
if %tod_cntr% equ 3 (set toll=T)
if %tod_cntr% equ 5 (set toll=T)
REM -- Flag which SOLA assignment script to run --
set sola_cmd=macros/SOLA_assignment.py
if %counter% EQU 2 (set sola_cmd=macros/SOLA_assignment_final_global_iteration.py)
if %counter% EQU 2 if "%check%" NEQ "None" (set sola_cmd=macros/SOLA_assignment_final_global_iteration_SelectLink.py)
REM
@ECHO   --- Begin ttables.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
call emme -ng 000 -m macros\ttables.mac %val% %tod_cntr% 92 93 >> blog.txt
@ECHO   --- End ttables.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO   --- Begin net5I_7c.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
call emme -ng 000 -m macros\net5I_7c.mac %tod_cntr% >> blog.txt
@ECHO   --- End net5I_7c.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO   --- Begin assignment Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO --- Begin assignment Period %tod_cntr%: %date% %time% ---
if "%toll%"=="T" (%empypath% macros/init_toll_skim_matrices.py %file1% %tod_cntr%)
if %ERRORLEVEL% NEQ 0 (goto issue)
@ECHO  -- Run TOD assignment --
%empypath% %sola_cmd% %file1% %tod_cntr% %sola_threads% %selLinkFile%
if %ERRORLEVEL% NEQ 0 (goto issue)
if "%toll%"=="T" (%empypath% macros/complete_toll_skim_matrices.py %file1% %tod_cntr%)
if %ERRORLEVEL% NEQ 0 (goto issue)
@ECHO   --- End assignment Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO --- End assignment Period %tod_cntr%: %date% %time% ---
@ECHO   --- Begin balance5I_7c.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO --- Balance time period volumes ---
call emme -ng 000 -m macros\balance5I_7c.mac %val% >> blog.txt
@ECHO   --- End balance5I_7c.mac Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO   --- Begin time-of-day skim Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO --- Complete time of day skims ---
call emme -ng 000 -m macros\TOD_skim_setup.mac %val% %tod_cntr% >> blog.txt
%empypath% macros/TOD_skim.py %file1% %tod_cntr% %val%%counter%%tod_cntr% %sola_threads%
if %ERRORLEVEL% NEQ 0 (goto issue)
@ECHO    -- End Time-of-Day Procedures Period %tod_cntr%: %date% %time% >> model_run_timestamp.txt
@ECHO -- End Time-of-Day Procedures for Period %tod_cntr%: %date% %time% --
set /A tod_cntr=tod_cntr+1
if %tod_cntr% LSS 9 (goto tod_loop)
:tod_end
REM -- End time-of-day loop --
@ECHO   --- Begin Global Iteration MSA skims: %date% %time% >> model_run_timestamp.txt
@ECHO --- Begin Global Iteration MSA skims: %date% %time% ---
%empypath% macros/MSA_iteration_skims.py %file1% 3 %val%%counter%3 %counter% %sola_threads%
if %ERRORLEVEL% NEQ 0 (goto issue)
%empypath% macros/MSA_iteration_skims.py %file1% 5 %val%%counter%5 %counter% %sola_threads%
if %ERRORLEVEL% NEQ 0 (goto issue)
@ECHO   --- End Global Iteration MSA skims: %date% %time% >> model_run_timestamp.txt
@ECHO --- End Global Iteration MSA skims: %date% %time%

@ECHO End Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt
@ECHO -- End Global Iteration %counter%: %date% %time% --
set /A counter=counter+1
goto while

:loopend
@ECHO END OF FULL MODEL LOOP
@ECHO ==================================================================

@ECHO Begin Daily Accumulation Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO END OF FULL MODEL ITERATIONS - PREPARING DAILY ACCUMULATION SCENARIO
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\Daily.Total.Asmt5I_7c.mac %val% >> blog.txt
REM Run script to complete select link analysis, if necessary
set /A counter=counter-1
if "%check%" NEQ "None" (%empypath% macros/complete_select_link.py %file1% %val%%counter%9 %val%%counter%0)
@ECHO End Daily Accumulation Procedures: %date% %time% >> model_run_timestamp.txt


REM The following two lines delete the trip and utility files from global iterations 0 and 1 to reduce storage space. Comment them out to retain.
if exist cache\choice_simulator_trips_out.001 (rmdir /S /Q cache\choice_simulator_trips_out.001)
if exist cache\choice_simulator_trips_out.002 (rmdir /S /Q cache\choice_simulator_trips_out.002)

:USskim
@ECHO Creating skim file for UrbanSim ...
python tg\scripts\urbansim_skims.py
goto last

REM ======================================================================
:filemiss
@ECHO
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM       Missing Files
REM       Open tod_factors.xls and create files
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goto end

:filemiss1
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO tg\fortran\MCHW_HH.TXT DOES NOT EXIST!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
goto end

:filemiss2
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO VERIFY m01auto.csv, m01tg.txt, m01type.csv EXIST in tg\data!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
goto end

:CheckEmpty2
if %~z1 == 0 (goto badsas)
goto Rpass

:badR
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND R INSTALLATION.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:mcmiss
@ECHO
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM       Missing M01, DISTR or Other Files
REM       in Database directory
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goto end

:hhmiss
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO     HH_VTYPE_TRIPS_IN.TXT or TG_HHENUM_OUTPUT.TXT
@ECHO     missing from Database folder.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

:issue
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO          THE LAST PROCEDURE DID NOT TERMINATE PROPERLY!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

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

:no_select_link_file
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    SELECT LINK FILE %selLinkFile% IS SPECIFIED BUT DOES NOT EXIST.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:last
@ECHO ====================================================== >> model_run_timestamp.txt
@ECHO END CMAP REGIONAL MODEL RUN - SCENARIO %val% >> model_run_timestamp.txt
@ECHO Model Run End Time: %date% %time% >> model_run_timestamp.txt
@ECHO ====================================================== >> model_run_timestamp.txt
@ECHO.
@ECHO END OF BATCH FILE - MODEL RUN COMPLETED
@ECHO ==================================================================
@ECHO ==================================================================

:end
pause
exit
