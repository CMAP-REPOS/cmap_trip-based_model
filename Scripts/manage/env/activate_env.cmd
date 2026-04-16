@echo off

rem activate_env.cmd [1]

rem Activates an existing conda environment or, if it does not exist,
rem creates the environment before activating it.

rem Parameters:
rem     1 : emme
rem         Activate the custom Emme environment instead of the TBM
rem         environment (default).
rem     1 : r
rem         Activate the custom R environment instead of the TBM environment.

set PDIR=%~dp0

rem Define here the name of the TBM environment to be used.
set PY_ENVNAME=tbm-py
rem Define here the name of the R environment to be used.
set R_ENVNAME=tbm-r
rem Define here the name of the custom Emme environment to be used.
set EMME_ENVNAME=emme-plus

call "%PDIR%set_condapath.cmd"
if %ERRORLEVEL% neq 0 (goto end)

if [%~1]==[emme] (
    set ENVNAME=%EMME_ENVNAME%
) else if [%~1]==[r] (
    set ENVNAME=%R_ENVNAME%
) else (
    set ENVNAME=%PY_ENVNAME%
)
set ENVPATH=%CONDAPATH%\envs\%ENVNAME%

rem Create it if it doesn't exist.
if [%~1]==[r] (
    if not exist "%ENVPATH%\Scripts\Rscript.exe" (
        echo R not found at "%ENVPATH%"
        echo.
        goto create
    ) else (goto activate)
) else (
    if not exist "%ENVPATH%\python.exe" (
        echo Python not found at "%ENVPATH%"
        echo.
        goto create
    ) else (goto activate)
)

:create
rem Activate the conda base environment.
rem Using call is required here, see:
rem https://stackoverflow.com/questions/24678144/conda-environments-and-bat-files
call "%CONDAPATH%\Scripts\activate.bat"
if %ERRORLEVEL% neq 0 (
    echo Error in activating conda
    goto end
)
rem Create it from a file.
echo Creating %ENVNAME% environment
echo.
call conda env create -f "%PDIR%%ENVNAME%.yml"

if [%~1]==[emme] (
    rem Add the path configuration file to include Emme Python libraries.
    copy "%PDIR%emme.pth" "%ENVPATH%\Lib\site-packages"
)

:activate
rem Activate it.
call "%CONDAPATH%\Scripts\activate.bat" "%ENVPATH%"
if %ERRORLEVEL% neq 0 (
    echo Error in activating %ENVNAME%
    goto end
) else (goto activated)

:end
echo.
pause
exit

:activated
echo Active conda environment is %ENVNAME%
echo.