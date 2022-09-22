rem activate_python_env.bat
rem 07/17/2020
rem N. Ferguson

rem Activates existing Python virtual environment, or builds one, before
rem checking/installing required Python packages.  Needs Python
rem installed to build a new virtual environment.  Required packages are
rem specified in a text file.

rem Parameters:
rem   1 (optional) - Passing 'arcpy' will use ArcGIS Python instead of a
rem                  virtual environment.

rem ====================================================================

rem Settings

rem Drives to search for Python.
set drives=C: D: E:

if '%~1'=='arcpy' (
    set arcpy=y
    rem Directory paths to search for ArcGIS Python.
    set pydirs="Program Files\ArcGIS\Pro"
) else (
    set arcpy=n
    rem Directory paths to search for Python.
    set pydirs="Program Files\INRO\Emme" "Program Files\ArcGIS\Pro"
)

rem Path to required packages specification.
set reqs="requirements.txt"

rem ====================================================================

rem Move to the directory of this script.
cd %~dp0

@echo Searching for Python...

rem If arcpy is not needed, look for an existing virtual environment.
if %arcpy%==n (if exist env (goto env))

goto noenv

:env

rem Activate the existing virtual environment.
call .\env\Scripts\activate
rem Check the location of the virtual environment that was activated.
if %virtual_env%==%~dp0env (
    rem Check that activation was successful.
    call :showpython
    python -c "print('Python virtual environment activated.')"
    if %errorlevel% equ 0 (goto envfound)
)

rem Remove the existing virtual environment if activation was
rem unsuccessful.
call .\env\Scripts\deactivate
rmdir /s /q env

:noenv

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
call :showpython
if %arcpy%==y (goto testarcpy)

@echo Building the virtual environment...
python -m venv env

rem Activating the virtual environment.
call .\env\Scripts\activate
rem Check that activation was successful.
call :showpython
python -c "print('Python virtual environment activated.')"
if %errorlevel% neq 0 (goto envnotactivated)

:envfound

@echo.
@echo Updating pip...
python -m pip install -U pip
@echo.

@echo Checking package requirements...
rem Install required Python packages.
python -m pip install -r %reqs%
@echo.

@echo Installed packages:
python -m pip list

goto end

:testarcpy
python -c "exec(\"import sys\ntry:\n import arcpy\n print('arcpy found.')\nexcept: sys.exit(1)\")"
if %errorlevel% equ 0 (goto end) else (goto pythonnotfound)

rem ====================================================================

rem Subroutines

:showpython
python -c "exec(\"import sys\nprint(sys.executable)\")"
@echo.
goto :eof

rem ====================================================================

rem Error messages

:pythonnotfound
@echo.
if %arcpy%==y (
    @echo !!! COULD NOT FIND ARCPY INSTALLED ON THIS MACHINE !!!
)else (
    @echo !!! COULD NOT FIND PYTHON INSTALLED ON THIS MACHINE !!!
)
@echo.
pause
exit

:envnotactivated
@echo.
@echo !!! PYTHON VIRTUAL ENVIRONMENT COULD NOT BE ACTIVATED !!!
@echo.
pause
exit

rem ====================================================================

:end
@echo.
exit /b