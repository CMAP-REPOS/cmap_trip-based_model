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

rem Define here the name of the environment to be renamed
rem and the specification file for the environment to be created.
set ENVNAME=CMAP-TRIP
set ENVPATH=%CONDAPATH%\envs\%ENVNAME%

set SPECFILE=conda-environment.yml
set SPECPATH=%~dp0src\Mode-Dest-TOD\%SPECFILE%

set CUR_YYYY=%date:~10,4%
set CUR_MM=%date:~4,2%
set CUR_DD=%date:~7,2%
set CUR_HH=%time:~0,2%
if %CUR_HH% lss 10 (set CUR_HH=0%time:~1,1%)
set CUR_NN=%time:~3,2%
set CUR_SS=%time:~6,2%
set CUR_DATETIME=%CUR_YYYY%%CUR_MM%%CUR_DD%_%CUR_HH%%CUR_NN%%CUR_SS%
set BAKNAME=%ENVNAME%_%CUR_DATETIME%
set BAKPATH=%CONDAPATH%\envs\%BAKNAME%

rem Activate the base conda environment.
rem Using call is required here, see: https://stackoverflow.com/questions/24678144/conda-environments-and-bat-files
call %CONDAPATH%\Scripts\activate.bat
if %errorlevel% neq 0 (
    @echo Error in activating conda.
    goto end
)

@echo Updating conda...
call conda update -n base -c defaults conda
if %errorlevel% neq 0 (
    @echo Error in updating conda.
    goto end
)
@echo.

rem Backup existing env and build new env from file.
if exist %ENVPATH% (
    @echo Saving backup of %ENVNAME%...
    call conda rename -n %ENVNAME% %BAKNAME%
    if %errorlevel% neq 0 (
        @echo Error in saving backup env.
        goto end
    )
    @echo.
)

@echo Creating %ENVNAME% from %SPECPATH%...
call conda env create -f %SPECPATH%
if %errorlevel% neq 0 (
    @echo Error in creating env.
	if exist %BAKPATH% (
        goto restore
	)
	goto end
)
@echo.

goto deactivate

:restore
rem Restore env from backup.
@echo Restoring %ENVNAME% from backup...
    call conda rename -n %BAKNAME% %ENVNAME%
    if %errorlevel% neq 0 (
        @echo Error in restoring env from backup.
        goto end
    )
    @echo.
	
:deactivate
rem Deactivate the base environment.
call conda deactivate
@echo.
@echo Finished.

:end
@echo.
pause
exit