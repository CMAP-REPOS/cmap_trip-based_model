@echo off
rem Define here all the places where we might find the conda installation.
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
goto end

:condafound
@echo CONDAPATH is %CONDAPATH%
@echo.

rem Define here the name of the environment to be removed
rem and the specification file for the environment to be created.
set ENVNAME=CMAP-TRIP
set ENVPATH=%CONDAPATH%\envs\%ENVNAME%
set SPECFILE=conda-environment.yml
set SPECPATH=src\Mode-Dest-TOD\%SPECFILE%

rem Activate the base conda environment.
rem Using call is required here, see: https://stackoverflow.com/questions/24678144/conda-environments-and-bat-files
call %CONDAPATH%\Scripts\activate.bat
if %errorlevel% neq 0 (
    @echo Error in activating conda.
    goto end
)

rem Delete existing env and build new env from file.
if exist %ENVPATH% (
    @echo Removing %ENVNAME%...
    call conda env remove -n %ENVNAME%
    if %errorlevel% neq 0 (
        @echo Error in removing env.
        goto end
    )
    @echo.
)
@echo Creating %ENVNAME% from %SPECPATH%...
call conda env create -f %SPECPATH%
if %errorlevel% neq 0 (
    @echo Error in creating env.
    goto end
)
@echo.

rem Deactivate the base environment.
call conda deactivate
@echo.
@echo Finished.

:end
@echo.
pause
exit