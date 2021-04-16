rem activate_python_env.bat
rem 07/17/2020
rem N. Ferguson

rem Activates existing Python virtual environment, or builds one, before
rem checking/installing required Python packages. Needs Python installed
rem on machine to build a new virtual environment.

rem Parameters:
rem   1 (optional) - Passing 'arcpy' will use ArcGIS Python instead of a
rem                  virtual environment.

rem ====================================================================
rem Settings
rem --------
set drives=C: D: E:
if '%~1'=='arcpy' (
    set arcpy=y
    set pydirs="Python27\ArcGIS"
    goto noenv
) else (
    set arcpy=n
    set pydirs="Program Files\INRO\Emme" "Python27\ArcGIS"
)
rem ====================================================================

cd %~dp0

rem If the virtual environment is already available, skip the process of
rem building it.
if exist env (call .\env\Scripts\activate) else (goto noenv)
if %virtual_env%==%~dp0env (
    goto envfound
) else (
	call .\env\Scripts\deactivate
	rmdir /s /q env
)

:noenv
@echo Searching for Python...
@echo.

rem Look for Python in each location, redirect errors to nul if not
rem found, and store path to directory of first python.exe found.
for %%a in (%drives%) do (
    for %%b in (%pydirs%) do (
        for /d %%c in ("%%a\%%~b*") do (
            for /f "delims=" %%d in ('dir "%%c\*python.exe" /b /s 2^> nul') do (
                set pypath=%%~dpd
                goto pythonfound
            )
        )
    )
)

goto pythonnotfound

:pythonfound
rem Temporarily (for the shell session) add the stored path to the start
rem of the path environment variable.
set path=%pypath%;%path%

if %arcpy%==y (
    @echo Python found:
    where python
    @echo.
    goto usearcgispython
)

@echo Updating pip...
@echo.

rem Installs latest version in user appdata directory
python -m pip install --user --upgrade pip
@echo.

@echo Installing virtualenv...
@echo.

rem Installs in user appdata directory
python -m pip install --user virtualenv
@echo.

@echo Creating the virtual environment...
@echo.

python -m virtualenv env
@echo.

:activateenv
@echo Activating the virtual environment...
@echo.

call .\env\Scripts\activate
@echo.

:envfound
@echo Python found:
where python
@echo.

@echo Checking package requirements...
@echo.

rem Installs Python packages listed in requirements file
python -m pip install -r requirements.txt
@echo.

:usearcgispython
@echo Installed packages:
@echo.

python -m pip list
@echo.

exit /b

:pythonnotfound
if %arcpy%==y (@echo !!! COULD NOT FIND ARCPY INSTALLED ON THIS MACHINE !!!)
else (@echo !!! COULD NOT FIND PYTHON INSTALLED ON THIS MACHINE !!!)
@echo.
pause
exit