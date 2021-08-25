@echo off

rem Submit_Full_Regional_Model.bat
rem Craig Heither, CMAP
rem Nick Ferguson, CMAP

@echo.
@echo ==============================================================
@echo BATCH FILE TO PRECOMPILE CMAP MODE DESTINATION AND TIME OF DAY
@echo ==============================================================
@echo.

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
@echo cannot find conda in any of the usual places
goto end

:condafound
@echo CONDAPATH IS %CONDAPATH%
@echo.


@ECHO.
@ECHO RUN CMAP MODE-DESTINATION CHOICE MODEL - FULL MODEL ITERATION %counter%
@ECHO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rem The `CONDAPATH` environment variable should be set before running this .bat
rem It points to the place where conda is installed
rem Alternatively if running in a conda prompt itself then CONDA_PREFIX will be set
IF DEFINED CONDAPATH (
	ECHO CONDAPATH IS %CONDAPATH%
) ELSE (
	IF DEFINED CONDA_PREFIX (
		set CONDAPATH=%CONDA_PREFIX%
		ECHO CONDA_PREFIX is %CONDAPATH%
	) ELSE (
		ECHO CONDAPATH is not defined, first run set CONDAPATH=C:\... to point to the conda installation
		pause
		EXIT /b
	)
)

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

call cmap_modedest . --njobs 1 --max_zone_chunk 5 --short 1

rem Deactivate the environment
call conda deactivate
@ECHO    -- End Mode-Destination Choice Precompile: %date% %time% ---

:end
pause
exit
