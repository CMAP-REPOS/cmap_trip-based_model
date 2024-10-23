REM -- Ensure CMAP-TRIP2 uses the destination choice-mode choice source code in this model setup --
REM  Heither, 07-03-2024
REM -- Get path to Anaconda installation --
set infile=anaconda.txt
if exist %infile% (del %infile% /Q)
dir /S "%USERPROFILE%\*Anaconda3" /b /D >> %infile% 2>nul
for /f "delims=" %%x in (%infile%) do set anapath=%%x
echo Anaconda path = %anapath%
call :CheckEmpty %infile%
:filepass
if exist %infile% (del %infile% /Q)
rem point CMAP-TRIP2 to the source code in this model run
call "%anapath%\Scripts\activate.bat"
call activate CMAP-TRIP2
pip install -e src/Mode-Dest-TOD
pip install -e src/Mode-Dest-TOD/sharrow
call conda deactivate
goto end


:CheckEmpty
if %~z1 == 0 (goto badConda)
goto filepass
:badConda
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND ANACONDA INSTALLATION.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:end
exit /B
