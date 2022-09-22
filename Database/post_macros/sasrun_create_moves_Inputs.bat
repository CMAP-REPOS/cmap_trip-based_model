@ECHO OFF

set script=create.MOVES.input.file.IMversion


"C:/Program Files/SASHome/SASFoundation/9.4/sas.exe" -sysin %script%
if %ERRORLEVEL% GTR 1 (goto saserr)
@ECHO %script% completed successfully.
goto end


@ECHO.
goto end

:saserr
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO %script% DID NOT TERMINATE PROPERLY!!! 
@ECHO REVIEW .LOG FILE TO IDENTIFY AND CORRECT ISSUE.
@ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@ECHO.
pause
goto end


:end
pause