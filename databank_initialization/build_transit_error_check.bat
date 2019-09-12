@ECHO off
@ECHO.
@ECHO ==========================================================================
@ECHO BUILD_TRANSIT_ERROR_CHECK.BAT
REM Nick Ferguson
REM 12/6/2017
@ECHO ##########################################################################
@ECHO.
@ECHO This program calls build_transit_error_check.py to check the
@ECHO build transit reports for errors.
@ECHO.
@ECHO ##########################################################################
@ECHO.

REM ## Supply 3-digit scenario to run macro. ##
set sc=%1

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
REM ============================================================================


echo scenario = %sc%
if not '%sc%'=='' (set sc=%sc:~0,3%)
if not '%sc%'=='' (goto next)
goto badscen

:next
set /A val=%sc%
if %val% GEQ 100 (goto continue)
goto badscen 

:continue
REM ## Ensure CMD.exe recognizes current directory is where batch file was
REM    called from (from Matt Stratton). ##
CD %~dp0

@ECHO -- CHECKING FOR TRANSIT NETWORK INPUT ERRORS --
if exist ..\report\build_transit.error (del ..\report\build_transit.error /Q)
%pypath% build_transit_error_check.py %val%
if exist ..\report\build_transit.error (goto badpknet)
set /A p5=%val%+5
%pypath% build_transit_error_check.py %p5%
if exist ..\report\build_transit.error (goto badopnet)

@ECHO.
@ECHO NO TRANSIT NETWORK ERRORS FOUND.
@ECHO.
goto last
REM ============================================================================
:CheckEmpty
if %~z1 == 0 (goto badpython)
goto pythonpass

:badpython
@ECHO.
@ECHO PYTHON EXECUTABLE NOT FOUND.
@ECHO MANUALLY SET PATH VARIABLE PYPATH1 AND RE-RUN. 
@ECHO.
pause
goto end

:badscen
@ECHO NO 3-DIGIT SCENARIO, RESUBMIT PROGRAM.
pause
goto end

:badpknet
@ECHO.
@ECHO ERRORS FOUND IN PEAK TRANSIT NETWORK.
@ECHO REVIEW report\build_%val%transit.rpt AND REBUILD TRANSIT NETWORK,
@ECHO THEN RERUN.
@ECHO.
pause
goto end

:badopnet
@ECHO.
@ECHO ERRORS FOUND IN OFF-PEAK TRANSIT NETWORK.
@ECHO REVIEW report\build_%p5%transit.rpt AND REBUILD TRANSIT NETWORK,
@ECHO THEN RERUN.
@ECHO.
pause
goto end

:last
pause
@ECHO end of batch file
@ECHO ==========================================================================
:end
exit