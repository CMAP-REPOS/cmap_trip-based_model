@ECHO BATCH FILE TO SUBMIT CMAP REGIONAL TRAVEL DEMAND MODEL
@ECHO  - 5 GLOBAL ITERATIONS
@ECHO  - 4 AUTO TRIP PURPOSES: HW LOW INCOME, HW HIGH INCOME, HO, NH
@ECHO  - PATH-BASED HIGHWAY ASSIGNMENT (7 VEHICLE CLASSES)
@ECHO         Class 1: 1 PERSON SOV (ALL PURPOSES)
@ECHO         Class 2: 2 PERSON HOV (ALL PURPOSES)
@ECHO         Class 3: 3+ PERSON HOV (ALL PURPOSES)
@ECHO         Class 4: B-PLATE TRUCK
@ECHO         Class 5: LIGHT DUTY TRUCK
@ECHO         Class 6: MEDIUM DUTY TRUCK
@ECHO         Class 7: HEAVY DUTY TRUCK
@ECHO ====================================================================
@ECHO.
@ECHO off
REM   Last revised 10-28-2014: Craig Heither, CMAP - include Non-work HOV procedures
REM   Last revised 02-20-2015: Craig Heither, CMAP - include Toll Mode Choice procedures
REM   Last revised 06-01-2015: Nick Ferguson, CMAP - call 7-class assignment macros
REM   Last revised 02-09-2017: Craig Heither, CMAP - use copy rather than rename for NAMELIST files
REM   Last revised 02-27-2017: Craig Heither, CMAP - call update_Namelist.py, add Monte Carlo iteration variables
REM   Last revised 03-09-2018: Craig Heither, CMAP - Automatically find Python executable.
REM   Last revised 03-18-2019: Craig Heither, CMAP - Use random integer to uniquely label Fortran executables and prevent conflict with a simultaneous model run.
REM   NRF 6/13/2019: Skip pre-distribution and distribution in global iterations 3 and 4. Call fixed seed EXEs PreDist_RnSeed and ModeChoice_RnSeed with seed CSV as argument.

REM *************************************
REM    Set 3-digit scenario number here
REM    --------------------------------
set /A val=100
REM *************************************

REM *************************************
REM    Set Iteration value (Global Iterations 0-3) here
REM    ------------------------------------------------
REM !!!NOT 3-DIGIT SCENARIO NUMBER!!!
set /A iter1=100
REM *************************************

REM *************************************
REM    Set Iteration value (Global Iteration 4) here
REM    ------------------------------------------------
REM !!!NOT 3-DIGIT SCENARIO NUMBER!!!
set /A iter2=100
REM *************************************

REM *************************************
REM    Select Random Integer to label Fortran executables
REM    ------------------------------------------------
set /A rndmint=%random% %%100
REM *************************************

@ECHO.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO         You must be connected to an Emme
@ECHO         license before continuing!!!        
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
@ECHO.
@ECHO ### SCENARIO RUN SET TO %val% ###
@ECHO  -- Pre-Distribution/Mode Choice Simulations (Global Iterations 0-3): %iter1%
@ECHO  -- Pre-Distribution/Mode Choice Simulations (Global Iteration 4): %iter2%
@ECHO.

set /P ok=[ RUN MODEL FOR SCENARIO %val%? (y/n) ]
@ECHO.
if '%ok%'=='y' (goto continue)
if '%ok%'=='Y' (goto continue)
goto end


:continue
set /A keeppath=0 
set /P ok2=[ SAVE ALL INTERIM CLASS-SPECIFIC PATH FILES? (y/n) - Generally not necessary. ]
@ECHO.
if '%ok2%'=='y' (set /A keeppath=1)
if '%ok2%'=='Y' (set /A keeppath=1)
if %keeppath% EQU 1 (@ECHO ALL PATH FILES WILL BE RETAINED.)

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
if exist %infile% (del %infile% /Q)

REM #################################################
REM Craig Heither, 6/4/08 - added to verify files exist before running
if not exist data\tod_factors.p1 (goto filemiss)
if not exist data\tod_factors.p2 (goto filemiss)
if not exist data\tod_factors.p3 (goto filemiss)
if not exist data\tod_factors.p4 (goto filemiss)
if not exist data\tod_factors.p5 (goto filemiss)
if not exist data\tod_factors.p6 (goto filemiss)
if not exist data\tod_factors.p7 (goto filemiss)
if not exist data\tod_factors.p8 (goto filemiss)
if not exist data\tod_occ.p1 (goto filemiss)
if not exist data\tod_occ.p2 (goto filemiss)
if not exist data\tod_occ.p3 (goto filemiss)
if not exist data\tod_occ.p4 (goto filemiss)
if not exist data\tod_occ.p5 (goto filemiss)
if not exist data\tod_occ.p6 (goto filemiss)
if not exist data\tod_occ.p7 (goto filemiss)
if not exist data\tod_occ.p8 (goto filemiss)
if not exist data\directional.splits (goto filemiss)
REM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM Craig Heither, 3/30/10 - added to verify M01, DISTR  & other files exist
if not exist MCHO_DISTR.TXT (goto mcmiss)
if not exist MCHO_M01.TXT (goto mcmiss)
if not exist MCHO_M023.TXT (goto mcmiss)
if not exist MCHW_CBDPARK.TXT (goto mcmiss)
if not exist MCHW_DISTR.TXT (goto mcmiss)
if not exist MCHW_HH.TXT (goto mcmiss)
if not exist MCHW_M01.TXT (goto mcmiss)
if not exist MCHW_M023.TXT (goto mcmiss)
if not exist MCNH_DISTR.TXT (goto mcmiss)
if not exist MCNH_M01.TXT (goto mcmiss)
if not exist MCNH_M023.TXT (goto mcmiss)
if not exist PDHO_DISTR.TXT (goto mcmiss)
if not exist PDHO_M01.TXT (goto mcmiss)
if not exist PDHO_M023.TXT (goto mcmiss)
if not exist PDHW_CBDPARK.TXT (goto mcmiss)
if not exist PDHW_DISTR.TXT (goto mcmiss)
if not exist PDHW_M01.TXT (goto mcmiss)
if not exist PDHW_M023.TXT (goto mcmiss)
if not exist PDNH_DISTR.TXT (goto mcmiss)
if not exist PDNH_M01.TXT (goto mcmiss)
if not exist PDNH_M023.TXT (goto mcmiss)
REM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REM Craig Heither, 10/28/14 - added to verify household attribute files present for non-work vehicle occupancy model
if not exist HH_VTYPE_TRIPS_IN.TXT (goto hhmiss)
copy tg\fortran\TG_HHENUM_OUTPUT.TXT TG_HHENUM_OUTPUT.TXT /y
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

@ECHO ====================================================== >> model_run_timestamp.txt
@ECHO BEGIN CMAP REGIONAL MODEL RUN - SCENARIO %val% >> model_run_timestamp.txt
@ECHO Model Run Start Time: %date% %time% >> model_run_timestamp.txt
@ECHO ====================================================== >> model_run_timestamp.txt


REM PREP WORK
@ECHO   ***  Cleaning up databank.  ***
if exist cleanup.rpt (del cleanup.rpt)
call emme -ng 000 -m useful_macros\cleanup.for.rerun %val% 2 >> cleanup.rpt
@ECHO.
@ECHO   ***  Skimming highway network.  ***
if exist reports (del reports)
call emme -ng 000 -m prep_macros\free.skim.mac %val% 2 >> blog.txt
@ECHO.

REM MAKE COPIES OF FORTRAN EXECUATBLES TO RUN
copy PreDist_RnSeed.exe PreDist_RnSeed_%rndmint%.exe /y
copy ModeChoice_RnSeed.exe ModeChoice_RnSeed_%rndmint%.exe /y
copy VehOcc.exe VehOcc_%rndmint%.exe /y

@ECHO ======================================================================
REM - LOOP TO RUN MODEL (Heither 04/2010)
set /A counter=0 
:while
if %counter% GTR 4 (goto loopend)

@ECHO -- Begin Transit skim Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TRANSIT SKIM - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\call\skim.transit.all %val% %counter% %pypath% >> blog.txt
@ECHO    -- End of Transit Skim Procedures: %date% %time% >> model_run_timestamp.txt


@ECHO Begin Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt
@ECHO PREPARING EMMEBANK - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@ECHO on
call emme -ng 000 -m macros\init_HOVsim_databk.mac %val% >> blog.txt


REM UPDATE NAMELIST FILES
if %counter% EQU 0 (%pypath% update_Namelist.py %iter1%)
if %counter% EQU 4 (%pypath% update_Namelist.py %iter2%)


if %counter% GTR 2 (goto skipdistr)
@ECHO -- Begin Pre-Distribution: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING PRE-DISTRIBUTION - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@ECHO set files for PreDistribution run (NAMELIST file, output log file, zonal interchange random seed file)
start "Home-Work Pre Distribution Scen. %val%" cmd /c PreDist_RnSeed_%rndmint%.exe PDHW_NAMELIST.TXT HW%counter%_PREDIST_LOG.TXT hwlow_seeds.csv
start "Home-Other Pre Distribution Scen. %val%" cmd /c PreDist_RnSeed_%rndmint%.exe PDHO_NAMELIST.TXT HO%counter%_PREDIST_LOG.TXT ho_seeds.csv
PreDist_RnSeed_%rndmint%.exe PDNH_NAMELIST.TXT NH%counter%_PREDIST_LOG.TXT nh_seeds.csv
:check_PreDist
timeout 20
tasklist | findstr /I "PreDist_RnSeed_%rndmint%.exe" > nul
if %errorlevel% equ 0 (@echo "PreDist still running ..." & goto check_PreDist)
timeout 5
move /Y HW%counter%_PREDIST_LOG.TXT report\iter_%counter%\
move /Y HO%counter%_PREDIST_LOG.TXT report\iter_%counter%\
move /Y NH%counter%_PREDIST_LOG.TXT report\iter_%counter%\
@ECHO    -- End Pre-Distribution: %date% %time% >> model_run_timestamp.txt


@ECHO -- Begin Trip Distribution: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING IOM DISTRIBUTION - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\four_purpose_IOM.mac %val% >> blog.txt
@ECHO    -- End Trip Distribution: %date% %time% >> model_run_timestamp.txt
:skipdistr


@ECHO -- Begin Mode Choice: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING MODE CHOICE - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@ECHO set files for Mode Choice run (NAMELIST file, output log file)
start "Home-Work Low Income Mode Choice Scen. %val%" cmd /c ModeChoice_RnSeed_%rndmint%.exe MC_HWlow_NAMELIST.TXT HWlow%counter%_MC_LOG.TXT hwlow_seeds.csv
start "Home-Work High Income Mode Choice Scen. %val%" cmd /c ModeChoice_RnSeed_%rndmint%.exe MC_HWhigh_NAMELIST.TXT HWhigh%counter%_MC_LOG.TXT hwhigh_seeds.csv
start "Home-Other Mode Choice Scen. %val%" cmd /c ModeChoice_RnSeed_%rndmint%.exe MC_HO_NAMELIST.TXT HO%counter%_MC_LOG.TXT ho_seeds.csv
ModeChoice_RnSeed_%rndmint%.exe MC_NH_NAMELIST.TXT NH%counter%_MC_LOG.TXT nh_seeds.csv
:check_ModeChoice
timeout 20
tasklist | findstr /I "ModeChoice_RnSeed_%rndmint%.exe" > nul
if %errorlevel% equ 0 (@echo "Mode Choice still running ..." & goto check_ModeChoice)
timeout 5
move /Y HWlow%counter%_MC_LOG.TXT report\iter_%counter%\
move /Y HWhigh%counter%_MC_LOG.TXT report\iter_%counter%\
move /Y HO%counter%_MC_LOG.TXT report\iter_%counter%\
move /Y NH%counter%_MC_LOG.TXT report\iter_%counter%\
@ECHO    -- End Mode Choice: %date% %time% >> model_run_timestamp.txt


@ECHO -- Begin Non-Work Vehicle Occupancy Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING NON-WORK VEHICLE OCCUPANCY SUBMODEL - GLOBAL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\nonwork_vehocc_setup.mac 1 >> blog.txt

@ECHO set files for NonWork Vehicle Occupancy run (NAMELIST file, output log file)
start "Non-Work Vehicle Occupancy - Zn Group 1 Scen. %val%" cmd /c VehOcc_%rndmint%.exe VEHOCC_NAMELIST_1.TXT HVOCC_LOGOUT1_iter%counter%.TXT
start "Non-Work Vehicle Occupancy - Zn Group 2 Scen. %val%" cmd /c VehOcc_%rndmint%.exe VEHOCC_NAMELIST_2.TXT HVOCC_LOGOUT2_iter%counter%.TXT
start "Non-Work Vehicle Occupancy - Zn Group 3 Scen. %val%" cmd /c VehOcc_%rndmint%.exe VEHOCC_NAMELIST_3.TXT HVOCC_LOGOUT3_iter%counter%.TXT
VehOcc_%rndmint%.exe VEHOCC_NAMELIST_4.TXT HVOCC_LOGOUT4_iter%counter%.TXT
:check_VehOcc
timeout 20
tasklist | findstr /I "VehOcc_%rndmint%.exe" > nul
if %errorlevel% equ 0 (@echo "NonWork Vehicle Occupancy still running ..." & goto check_VehOcc)
timeout 5
move /Y HVOCC_LOGOUT1_iter%counter%.TXT report\iter_%counter%\
move /Y HVOCC_LOGOUT2_iter%counter%.TXT report\iter_%counter%\
move /Y HVOCC_LOGOUT3_iter%counter%.TXT report\iter_%counter%\
move /Y HVOCC_LOGOUT4_iter%counter%.TXT report\iter_%counter%\

call emme -ng 000 -m macros\nonwork_vehocc_setup.mac 2 >> blog.txt
@ECHO    -- End Non-Work Vehicle Occupancy Procedures: %date% %time% >> model_run_timestamp.txt


REM   ~~~~~~ Begin using toll mode choice procedures in Global Iteration 1
if %counter% EQU 0 (goto skip_toll)
@ECHO -- Begin Toll Mode Choice Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO BEGINNING TOLL MODE CHOICE PROCEDURES - GLOBAL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\toll_mode_choice.mac %val% %counter% >> blog.txt
@ECHO    -- End Toll Mode Choice Procedures: %date% %time% >> model_run_timestamp.txt
:skip_toll


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
if %keeppath% EQU 0 (if exist PATHS_s%val%%prev%* (del PATHS_s%val%%prev%* /Q))

@ECHO End Global Iteration %counter%: %date% %time% >> model_run_timestamp.txt

set /A counter=counter+1
goto while

:loopend
@ECHO END OF FULL MODEL LOOP
@ECHO ======================================================================

if exist PreDist_RnSeed_%rndmint%.exe (del PreDist_RnSeed_%rndmint%.exe /Q)
if exist ModeChoice_RnSeed_%rndmint%.exe (del ModeChoice_RnSeed_%rndmint%.exe /Q)
if exist VehOcc_%rndmint%.exe (del VehOcc_%rndmint%.exe /Q)


@ECHO Begin Daily Accumulation Procedures: %date% %time% >> model_run_timestamp.txt
@ECHO.
@ECHO END OF FULL MODEL ITERATIONS - PREPARING DAILY ACCUMULATION SCENARIO
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call emme -ng 000 -m macros\Daily.Total.Asmt5I_7c.mac %val% >> blog.txt
@ECHO End Daily Accumulation Procedures: %date% %time% >> model_run_timestamp.txt
goto last

REM ======================================================================
:CheckEmpty
if %~z1 == 0 (goto badpython)
goto pythonpass

:badpython
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND PYTHON INSTALLED ON THIS MACHINE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

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

:last
@ECHO ====================================================== >> model_run_timestamp.txt
@ECHO END CMAP REGIONAL MODEL RUN - SCENARIO %val% >> model_run_timestamp.txt
@ECHO Model Run End Time: %date% %time% >> model_run_timestamp.txt
@ECHO ====================================================== >> model_run_timestamp.txt
@ECHO.
@ECHO END OF BATCH FILE - MODEL RUN COMPLETED
@ECHO ======================================================================
@ECHO ======================================================================
:end
pause
exit