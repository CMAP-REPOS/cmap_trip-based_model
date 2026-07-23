@echo off

rem The `CONDAPATH` environment variable should be set before running this .cmd
rem It points to the place where conda is installed
rem Alternatively, if running in a conda prompt itself then CONDA_PREFIX will be set
if defined CONDAPATH (
	goto condafound
)
if defined CONDA_PREFIX (
	set CONDAPATH=%CONDA_PREFIX%
	echo CONDA_PREFIX is "%CONDAPATH%"
	goto condafound
)
rem Define here all the places where we might find the conda installation
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
    if exist "%%x\Scripts\activate.bat" (
        set CONDAPATH=%%x
        goto condafound
    )
)
echo Cannot find conda in any of the usual places.
echo CONDAPATH is not defined, first run set CONDAPATH=C:\... to point to the conda installation.
echo.
pause
exit

:condafound
echo CONDAPATH is "%CONDAPATH%"
echo.