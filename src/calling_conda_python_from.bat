@echo OFF
rem How to run a Python script in a given conda environment from a batch file.

rem explicitly define here the path to your conda installation, or define it in the CONDAPATH environment variable
rem `set CONDAPATH=C:\Users\jeffnewman\AppData\Local\mambaforge`

rem The `CONDAPATH` environment variable should be set before running this .bat
rem It points to the place where conda is installed
rem Alternatively if running in a conda prompt itself then CONDA_PREFIX will be set
IF DEFINED CONDAPATH (
	ECHO CONDAPATH IS %CONDAPATH%
) ELSE (
	IF DEFINED CONDA_PREFIX (
		set CONDAPATH=%CONDA_PREFIX%
		ECHO CONDA_PREFIX IS %CONDAPATH%
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

rem Run a the cmap mode and destination tool in that environment
call cmap_modedest --help

rem Deactivate the environment
call conda deactivate

rem Pause holds open the window until a key is pressed
pause
