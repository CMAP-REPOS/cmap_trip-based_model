@echo off
REM Delete transit skims.
REM  Heither, 11-26-2022

cd %~dp0
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@echo.
@echo   BEFORE CONTINUING:
@echo.
@echo   - Please connect to an Emme license.
@echo.
@echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pause
echo.
REM -- Get name of .emp file --
set infile=empfile.txt
cd ../..
if exist %infile% (del %infile% /Q)
dir "*.emp" /b >> %infile% 2>nul
set /p file1=<%infile%
echo file1 = %file1%
call :CheckEmpty %infile%
:filepass
if exist %infile% (del %infile% /Q)
cd Database

rem Activate Emme Python env
call %~dp0..\..\Scripts\manage\env\activate_env.cmd emme

echo.
REM -- Delete transit assignment matrices
python transit_asmt_macros/delete_transit_skims.py %file1%
@echo.
@echo MATRICES DELETED.
goto end

rem ===========================================================================
:CheckEmpty
if %~z1 == 0 (goto badfile)
goto filepass

:badfile
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO    COULD NOT FIND .EMP FILE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end

:end
pause
exit
