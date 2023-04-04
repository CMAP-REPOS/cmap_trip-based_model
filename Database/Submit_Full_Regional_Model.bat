@echo off

rem Submit_Full_Regional_Model.bat
rem Craig Heither, CMAP
rem Nick Ferguson, CMAP

@echo BATCH FILE TO SUBMIT CMAP REGIONAL TRAVEL DEMAND MODEL
@echo   - 3 GLOBAL ITERATIONS
@echo   - 5 AUTO TRIP PURPOSES: HW LOW INCOME, HW HIGH INCOME, HB Shop, HO, NH
@echo   - PATH-BASED HIGHWAY ASSIGNMENT (7 VEHICLE CLASSES)
@echo       Class 1: 1 PERSON SOV LOW VOT (ALL PURPOSES)
@echo       Class 2: 1 PERSON SOV MED VOT (ALL PURPOSES)
@echo       Class 3: 1 PERSON SOV HIGH VOT (ALL PURPOSES)
@echo       Class 4: 2+ PERSON HOV (ALL PURPOSES)
@echo       Class 5: B-PLATE + LIGHT DUTY TRUCK
@echo       Class 6: MEDIUM DUTY TRUCK
@echo       Class 7: HEAVY DUTY TRUCK
@echo.
@echo ==================================================================
@echo.
rem Revision history
rem ----------------
rem 07/16/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 11/08/2022 Ferguson: Removed call to activate_python_env.bat. Uses only conda env.
rem ====================================================================
rem Settings
rem --------
rem Set the 3-digit scenario number.
set /a val=100

REM Clean up prior to run
if exist cache\choice_simulator_trips_out (rmdir /S /Q cache\choice_simulator_trips_out)
if exist cache\choice_simulator_trips_out.001 (rmdir /S /Q cache\choice_simulator_trips_out.001)
if exist cache\choice_simulator_trips_out.002 (rmdir /S /Q cache\choice_simulator_trips_out.002)
if exist cache\choice_simulator_trips_out.003 (rmdir /S /Q cache\choice_simulator_trips_out.003)
if exist cache\choice_simulator_trips_out.004 (rmdir /S /Q cache\choice_simulator_trips_out.004)
del cache\logs\*.* /Q   
del usemacro_* /Q  

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
set /p choice="[SELECT A MODEL RUN MODE] "
echo.
if not "%choice%"=="" (
    set choice=%choice:~0,1%
    if "%choice%"=="1" (goto proceed)
    if "%choice%"=="2" (
		set /a jobs=12
		set /a zones=7
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
@echo.

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
REM - LOOP TO RUN MODEL (Heither 04/2010)
set /A counter=0
:while
if %counter% GTR 2 (goto loopend)

@ECHO -- Begin Transit skim Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TRANSIT SKIM - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\call\skim.transit.all %val% %counter% python >> blog.txt
if %errorlevel% neq 0 (goto end)
@ECHO    -- End of Transit Skim Procedures: %date% %time% >> model_run_timestamp.txt

@ECHO Begin Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt
@ECHO PREPARING EMMEBANK - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@ECHO on
call emme -ng 000 -m macros\init_HOVsim_databk.mac %val% >> blog.txt


@ECHO -- Begin Mode-Destination Choice Procedures: %date% %time% >> model_run_timestamp.txt

@ECHO.
@ECHO RUN CMAP MODE-DESTINATION CHOICE MODEL - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

call cmap_modedest . --njobs %jobs% --max_zone_chunk %zones%
if %ERRORLEVEL% NEQ 0 (goto issue)

del cache\choice_simulator_trips_out\choice_simulator_util_*.pq /Q
@ECHO    -- End Mode-Destination Choice Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.

@ECHO -- Begin Time-of-Day Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TOD HIGHWAY ASSIGNMENT - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\iter.master7c.mac %val% >> blog.txt
@ECHO    -- End Time-of-Day Procedures: %date% %time% >> model_run_timestamp.txt

@ECHO.
@ECHO DELETING TEMPORARY PATH FILES - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set /A prev=counter-1
if %counter% LSS 2 (goto no_delete)
if exist PATHS_s%val%%prev%* (del PATHS_s%val%%prev%* /Q)
:no_delete

@ECHO End Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt

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
@ECHO End Daily Accumulation Procedures: %date% %time% >> model_run_timestamp.txt

:USskim
@ECHO Creating skim file for UrbanSim ...
python tg\scripts\urbansim_skims.py
goto last

:filemiss
@ECHO on
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM       Missing Files
REM       Open tod_factors.xls and create files
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goto end

:mcmiss
@ECHO on
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
@ECHO     DESTINATION CHOICE-MODE CHOICE MODEL DID NOT TERMINATE PROPERLY!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

:last
rem Deactivate the environment
call conda deactivate

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