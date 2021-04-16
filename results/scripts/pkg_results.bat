rem pkg_results.bat
@echo.
@echo off
rem Claire Bozic, CMAP
rem Nick Ferguson, CMAP

rem Processes model results and packages outputs into a collection of
rem .zip files

rem Revision history
rem ----------------
rem 03/21/2019 Ferguson: Added generic and transit path error messages
rem 10/24/2019 Ferguson: Now calls emme to run results macros

rem ====================================================================
rem Settings
rem --------
if [%1]==[] (
    set /p conf_code="Enter in the conformity code (e.g., c20q1): "
    @echo.
) else ( set conf_code=%1)
if [%2] EQU [] (
    set /p scen_code="Enter in the 3-digit scenario code (e.g., 200): "
    @echo.
) else (set scen_code=%2)

rem ====================================================================
for /d %%a in (..\..\%conf_code%_%scen_code%*) do (
    set rundir=%%~dpa
    set run=%%~na
    goto getresults
)

goto nomodelrun

:getresults
@echo Gathering %run% results...
@echo.

set batdir=%~dp0
set macdir=%batdir%macros
set dbdir=%rundir%%run%\Database
set outdir=%rundir%results\output\%run%

if exist "%outdir%" (rmdir /s /q "%outdir%")
mkdir "%outdir%"
mkdir "%outdir%\trips"
mkdir "%outdir%\trips\work_trips"
mkdir "%outdir%\trips\hov_trips"
mkdir "%outdir%\networks"
mkdir "%outdir%\networks\highway"
mkdir "%outdir%\networks\transit"
mkdir "%outdir%\skims"

@echo Started %date% %time% >> "%outdir%\pkg_results.log"

@echo Creating %conf_code% %scen_code% data backup...
@echo.
xcopy "%dbdir%\emmemat" "%dbdir%\emmemat_bak" /i /q
fc /b "%dbdir%\emmemat\*" "%dbdir%\emmemat_bak\*" >nul || goto backupfailed

copy "%dbdir%\emmebank" "%dbdir%\emmebank.bak"
fc /b "%dbdir%\emmebank" "%dbdir%\emmebank.bak" >nul || goto backupfailed
@echo.

@echo Running macros...
@echo.
chdir "%dbdir%"
call emme -ng 000 -m "%macdir%\call_all.mac" "%macdir%" "%outdir%" %scen_code% >> "%outdir%\call_all.log"
@echo.

@echo Packaging results...
@echo.
chdir "%batdir%"
set trdir=..\..\transit
.\7-Zip\7z a "%outdir%\trips_%conf_code%_%scen_code%.zip" "%outdir%\trips\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\worktrips_%conf_code%_%scen_code%.zip" "%outdir%\trips\work_trips\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\hovtrips_%conf_code%_%scen_code%.zip" "%outdir%\trips\hov_trips\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\emmenet_highway_%conf_code%_%scen_code%.zip" "%outdir%\networks\highway\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\highwayshp_ampk_%conf_code%_%scen_code%.zip" "%outdir%\networks\highway\highway_ampk-%scen_code%\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\highwayshp_pmpk_%conf_code%_%scen_code%.zip" "%outdir%\networks\highway\highway_pmpk-%scen_code%\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\highwayshp_%conf_code%_%scen_code%.zip" "%outdir%\networks\highway\highway-%scen_code%\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\emmenet_transit_%conf_code%_%scen_code%.zip" "%outdir%\networks\transit\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\transitshp_pk_%conf_code%_%scen_code%.zip" "%outdir%\networks\transit\transit_pk-%scen_code%\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\transitshp_op_%conf_code%_%scen_code%.zip" "%outdir%\networks\transit\transit_op-%scen_code%\*.*"
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\skims_%conf_code%_%scen_code%.zip" "%outdir%\skims\*.*"
if %errorlevel% neq 0 (goto nozip)

.\7-Zip\7z a "%outdir%\emmemat_%conf_code%_%scen_code%.7z" "%dbdir%\emmemat" -mmt
if %errorlevel% neq 0 (goto nozip)
.\7-Zip\7z a "%outdir%\emmebank_%conf_code%_%scen_code%.7z" "%dbdir%\emmebank" -mmt
if %errorlevel% neq 0 (goto nozip)

.\7-Zip\7z a "%outdir%\emmenet_transit_tod_%conf_code%_%scen_code%.zip" "%trdir%\%scen_code%\*.*"
if %errorlevel% neq 0 (goto badtransitpath)

.\7-Zip\7z a "%outdir%\prods_attrs_%conf_code%_%scen_code%.zip" "%dbdir%\tg\sas\data\*.in"
if %errorlevel% neq 0 (goto nozip)
@echo.

@echo *** %conf_code% %scen_code% package complete. ***
@echo.

@echo Restoring %conf_code% %scen_code% data from backup...
@echo.
rmdir /s /q "%dbdir%\emmemat"
xcopy "%dbdir%\emmemat_bak" "%dbdir%\emmemat" /i /q
fc /b "%dbdir%\emmemat\*" "%dbdir%\emmemat_bak\*" >nul || goto restorefailed
rmdir /s /q "%dbdir%\emmemat_bak"

del "%dbdir%\emmebank"
copy "%dbdir%\emmebank.bak" "%dbdir%\emmebank"
fc /b "%dbdir%\emmebank" "%dbdir%\emmebank.bak" >nul || goto restorefailed
del "%dbdir%\emmebank.bak"
@echo.

goto end

:nomodelrun
@echo.
@echo !!! NO SCENARIO %conf_code% %scen_code% MODEL RUN FOUND !!!
@echo.
pause
exit

:backupfailed
@echo.
@echo !!! %conf_code% %scen_code% DATA BACKUP FAILED !!!
@echo File compare (fc) returned errorlevel=%errorlevel%.
@echo.
pause
exit

:nozip
@echo.
@echo !!! PACKAGING STOPPED !!!
@echo Something went wrong.
@echo.
pause
exit

:badtransitpath
@echo.
@echo !!! PACKAGING STOPPED !!!
@echo emmenet_transit_tod_%conf_code%_%scen_code% was not completely packaged. Check path to transit directory.
@echo.
pause
exit

:restorefailed
@echo.
@echo !!! %conf_code% %scen_code% DATA RESTORE FAILED !!!
@echo File compare (fc) returned errorlevel=%errorlevel%.
@echo.
pause
exit

:end
@echo Finished %date% %time% >> "%outdir%\pkg_results.log"
exit /b
