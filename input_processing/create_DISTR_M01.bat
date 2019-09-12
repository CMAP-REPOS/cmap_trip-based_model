REM Create_DISTR_M01.bat
@ECHO off
@ECHO #####################################################################
REM Craig Heither, revised 10-23-2014
@ECHO.
@ECHO This program creates the scenario-specific DISTR and M01 files needed
@ECHO for a full model run. Processing steps:
@ECHO   1. call Emme macro DISTR_M01_DATA.MAC to output network files.
@ECHO   2. call SAS program CREATE_DISTR_M01_FILES.SAS to format data
@ECHO      for spatial analysis.
@ECHO   3. call Python script DISTR_M01_SPATIAL_ANALYSIS.PY to get all spatial
@ECHO      information.
@ECHO.  4. call SAS program CREATE_DISTR_M01_FILES.SAS again to create final files.
@ECHO.
@ECHO #####################################################################
@ECHO.

REM      Revised: 07-24-2012 - calls Python2.6 even after 3.2 installed
REM      Revised: 11-28-2012 - additional fallback Python2.6 call added. 
REM      Revised: 04-22-2013 - paths updated for ArcGIS 10.1. 
REM      Revised: 06-11-2013 - paths updated for SAS 9.3. 
REM      Revised: 10-23-2014 - paths updated for SAS 9.4.
REM      Revised: 03-09-2018 - Automatically find Python and SAS executables. 
REM =======================================================================

@ECHO.
@ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
@ECHO   You must be connected to an Emme  
@ECHO   license before continuing.        
@ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
@ECHO.
pause
@ECHO.

@ECHO.
@ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
@ECHO   Note: The geoprocessing procedures in this 
@ECHO   script require an ArcInfo license.
@ECHO ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
@ECHO.
pause
@ECHO.

REM ## Supply 3-digit scenario to run macro. ##
set sc=
set /P sc=[ENTER 3-DIGIT SCENARIO NUMBER, ex: 100 ]
@ECHO.
if not '%sc%'=='' (set sc=%sc:~0,3%)
if not '%sc%'=='' (goto next)
goto badscen

:next
set /A val=%sc%
if %val% GEQ 100 (goto continue)
goto badscen 

:continue
set /P ok=[ CREATE DISTR AND M01 FILES FOR SCENARIO %val%? (y/n) ]
@ECHO.
if '%ok%'=='y' (goto findexe)
if '%ok%'=='Y' (goto findexe)
goto end

:findexe
REM  Search for viable Python executables (ArcGS one first), write paths to file, redirect errors to nul in case file not found, read first path from file
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
if not exist tg\sas\data\m01auto.csv (goto filemiss2)
if not exist tg\sas\data\m01tg.txt (goto filemiss2)
if not exist tg\sas\data\m01type.csv (goto filemiss2)

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
%pypath% distr_m01_spatial_analysis.py 
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
:CheckEmpty
if %~z1 == 0 (goto badpython)
goto pythonpass

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

:badpython
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO     COULD NOT FIND PYTHON INSTALLED ON THIS MACHINE.
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
@ECHO VERIFY m01auto.csv, m01tg.txt, m01type.csv EXIST in tg\sas\data\!!!
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