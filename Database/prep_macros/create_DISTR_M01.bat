@echo off

rem create_DISTR_M01.bat
rem Craig Heither, CMAP
rem Nick Ferguson, CMAP

@echo This program creates the scenario-specific DISTR and M01 files
@echo needed for a full model run. Processing steps:
@echo   1. call Emme macro DISTR_M01_DATA.MAC to output network files.
@echo   2. call SAS program CREATE_DISTR_M01_FILES.SAS to format data
@echo      for spatial analysis.
@echo   3. call Python script DISTR_M01_SPATIAL_ANALYSIS.PY to get all
@echo      spatial information.
@echo   4. call SAS program CREATE_DISTR_M01_FILES.SAS again to create
@echo      final files.
@echo.
@echo ==================================================================
@echo.
rem Revision history
rem ----------------
rem 07/24/2012 Heither: calls Python2.6 even after 3.2 installed
rem 11/28/2012 Heither: additional fallback Python2.6 call added.
rem 04/22/2013 Heither: paths updated for ArcGIS 10.1.
rem 06/11/2013 Heither: paths updated for SAS 9.3.
rem 10/23/2014 Heither: paths updated for SAS 9.4.
rem 03/09/2018 Heither: Automatically find Python and SAS executables.
rem 07/16/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 03/02/2021 Ferguson: Updated paths for removal of tg/sas directory.

rem ====================================================================
rem Settings
rem --------
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo   Connect to an available Emme license before continuing.
@echo.
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo.
pause
@echo.

@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo   The geoprocessing procedures in this script require an
@echo   available ArcGIS Desktop Advanced license. This setting must
@echo   be selected in ArcGIS Administrator before continuing.
@echo.
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo.
pause
@echo.

rem Supply 3-digit scenario to run macro.
set /p sc="[ENTER THE 3-DIGIT SCENARIO NUMBER (e.g., 100)] "
@echo.
if not "%sc%"=="" (set sc=%sc:~0,3%) else (goto badscen)

set /a val=%sc%
if %val% lss 100 (goto badscen)

set /p ok="[CREATE DISTR AND M01 FILES FOR SCENARIO %val%? (y/n)] "
@echo.
set ok=%ok:y=Y%
if not "%ok%"=="Y" (goto end)

@echo ==================================================================
@echo.

CD %~dp0

call ..\activate_python_env.bat arcpy
@echo.

CD %~dp0

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
set sasfile=create_distr_m01_files

REM -- Start Data Processing --
@ECHO.
@ECHO Start Time: %date% %time%
@ECHO.
CD %~dp0
if exist report.txt (del report.txt /Q)
if exist temp\nul (rmdir temp /S /Q)
if not exist temp (mkdir temp)
if exist %sasfile%.lst (del %sasfile%.lst /Q)
if exist saserr.txt (del saserr.txt /Q)
cd ..
if not exist tg\fortran\MCHW_HH.TXT (goto filemiss1)
copy tg\fortran\MCHW_HH.TXT MCHW_HH.TXT /y
if not exist tg\data\m01auto.csv (goto filemiss2)
if not exist tg\data\m01tg.txt (goto filemiss2)
if not exist tg\data\m01type.csv (goto filemiss2)

@ECHO.
@ECHO -- OBTAINING TRANSIT NETWORK DATA FROM EMME --
call emme -ng 000 -m prep_macros\distr_m01_data.mac %val% >> prep_macros\report.txt
cd prep_macros
@ECHO.
@ECHO -- CREATING FILES FOR SPATIAL ANALYSIS --
@ECHO.
%saspath% -sysin %sasfile% -sysparm 1
if %ERRORLEVEL% GTR 0 (goto saserr)

@ECHO -- PERFORMING SPATIAL ANALYSIS --
@ECHO    (THIS MAY TAKE 10-15 MINUTES)
@ECHO.
python distr_m01_spatial_analysis.py
@ECHO.
if %ERRORLEVEL% NEQ 0 (goto pyerr)

%saspath% -sysin %sasfile% -sysparm 2
if %ERRORLEVEL% GTR 0 (goto saserr)
if exist saserr.txt (goto sasdataerr)

@ECHO.
@ECHO -- DISTR AND M01 FILES CREATED --
@ECHO.
@ECHO End Time: %date% %time%
@ECHO.
if exist temp\nul (rmdir temp /S /Q)
if exist report.txt (del report.txt /Q)
if exist %sasfile%.log (del %sasfile%.log /Q)
goto last


REM ======================================================================
:CheckEmpty2
if %~z1 == 0 (goto badsas)
goto saspass

:badsas
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO     COULD NOT FIND SAS INSTALLED ON THIS MACHINE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:badscen
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO NO 3-DIGIT SCENARIO, RESUBMIT PROGRAM!!!
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
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

:saserr
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO SAS DID NOT TERMINATE PROPERLY!!!
@ECHO REVIEW .LOG FILE TO IDENTIFY AND CORRECT ISSUE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:sasdataerr
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO SOME SAS DATA INPUTS WERE NOT CREATED!!!
@ECHO REVIEW SASERR.TXT TO IDENTIFY AND CORRECT ISSUE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:pyerr
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO PYTHON ENCOUNTERED PROCESSING ERRORS!!!
@ECHO REVIEW TEXT ABOVE TO IDENTIFY AND CORRECT ISSUE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:last
pause
@ECHO end of batch file
@ECHO ======================================================================

:end
exit
