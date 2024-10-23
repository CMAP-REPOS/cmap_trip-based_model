@echo off

rem build_scenario_networks.bat
rem Craig Heither, CMAP - October 2024

@echo Build highway and transit time-of-day networks..
rem =========================================================================================
cd %~dp0
rem -- Read model run settings from batch_file.yaml --
for /f "eol=# skip=1 tokens=2 delims=:" %%z in (batch_file.yaml) do (set ver=%%z & goto break0)
:break0
for /f "eol=# skip=2 tokens=2 delims=:" %%a in (batch_file.yaml) do (set val=%%a & goto break1)
:break1
for /f "eol=# skip=12 tokens=2* delims=:" %%g in (batch_file.yaml) do (set transactFilePath1=%%g & set transactFilePath2=%%h & goto break6)
:break6

set ver=%ver:~1,5%
set val=%val:~1,3%
rem Construct complete transit file path
set transactFilePath1=%transactFilePath1:~1,-1%
set transactFilePath2=%transactFilePath2:~1,-1%
set sep=:\
set transactFilePath=%transactFilePath1%%sep%%transactFilePath2%
REM Remove trailing spaces from transactFilePath
set transactFilePath=%transactFilePath:~0,-1%

REM -- Get name of .emp file --
set infile=empfile.txt
cd ..
if exist %infile% (del %infile% /Q)
dir "*.emp" /b >> %infile% 2>nul
set /p file1=<%infile%
echo file1 = %file1%
call :CheckEmpty %infile%
:filepass
if exist %infile% (del %infile% /Q)
cd Database

@echo.
@echo ==================================================================================
@echo     --- Model Run Settings ---
@echo  Conformity version = %ver%
@echo  Scenario = %val%
@echo  Location of network transaction files = %transactFilePath%
@echo ==================================================================================
@echo.

set /p ok="[BUILD TIME-OF-DAY NETWORKS FOR SCENARIO %val%? (y/n)] "
@echo.
set ok=%ok:y=Y%
if not "%ok%"=="Y" (goto end)
@echo ==================================================================

rem Activate Emme Python env
call %~dp0..\Scripts\manage\env\activate_env.cmd emme

@ECHO --- Cleaning up databank ---
if exist cleanup.rpt (del cleanup.rpt)
call python useful_macros\cleanup_for_rerun.py %file1% %val%>> cleanup.rpt
if exist reports (del reports)
@ECHO  Cleanup complete.

@ECHO --- Creating time-of-day highway and transit networks ---
call python prep_macros\initialize_scenarios.py
if %ERRORLEVEL% GTR 0 (goto end)

@ECHO -- Checking for transit network input errors --
set /A trnscen=%val%+21
set /A maxscen=%val%+27
if exist report\build_transit.error (del report\build_transit.error /Q)

:while
if %trnscen% GTR %maxscen% (goto loopend)
python prep_macros\build_transit_error_check.py %trnscen%
if exist report\build_transit.error (goto badnet)
set /A trnscen=%trnscen%+2
goto while
:loopend

@ECHO.
@ECHO NO TRANSIT NETWORK ERRORS FOUND.
@ECHO.
goto last

rem ####################################################################################
:CheckEmpty
if %~z1 == 0 (goto badfile)
goto filepass

:badfile
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND .EMP FILE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
goto end

:badnet
@ECHO.
@ECHO ERRORS FOUND IN TRANSIT NETWORK %trnscen%.
@ECHO REVIEW report\build_%trnscen%transit.rpt AND REBUILD TRANSIT NETWORK,
@ECHO THEN RERUN.
@ECHO.
pause
goto end

:last
@ECHO Done!
@ECHO ==================================================================
@ECHO ==================================================================

:end
pause
exit