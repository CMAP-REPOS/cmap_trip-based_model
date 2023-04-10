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
REM ## Ensure CMD.exe recognizes current directory is where batch file was
REM    called from (from Matt Stratton). ##
CD %~dp0

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
@echo Cannot find conda in any of the usual places.
@echo CONDAPATH is not defined, first run set CONDAPATH=C:\... to point to the conda installation.
goto end

:condafound
@echo CONDAPATH IS %CONDAPATH%
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
rem Deactivate the environment
call conda deactivate
pause
@ECHO end of batch file
@ECHO ==========================================================================
:end
exit