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

REM 10/21/2020 Ferguson: Refined the Python search to use a virtual
rem            environment with custom package requirements by calling
rem            activate_python_env.bat.
rem 11/08/2022 Ferguson: Removed call to activate_python_env.bat. Uses only conda env.
REM ## Supply 3-digit scenario to run macro. ##
set sc=%1

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
rem Activate Python env
call %~dp0..\..\Scripts\manage\env\activate_env.cmd

CD %~dp0

@ECHO -- CHECKING FOR TRANSIT NETWORK INPUT ERRORS --
if exist ..\report\build_transit.error (del ..\report\build_transit.error /Q)
python build_transit_error_check.py %val%
if exist ..\report\build_transit.error (goto badpknet)
set /A p5=%val%+5
python build_transit_error_check.py %p5%
if exist ..\report\build_transit.error (goto badopnet)

@ECHO.
@ECHO NO TRANSIT NETWORK ERRORS FOUND.
@ECHO.
goto last

REM ============================================================================

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
@ECHO end of batch file
@ECHO ==========================================================================
:end
exit