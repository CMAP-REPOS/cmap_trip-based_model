@echo off

set cycle=c20q4
set scenarios=100 200 300 400 600 700

for %%a in (%scenarios%) do (call pkg_results %cycle% %%a)
@echo.

@echo Finished.
@echo.
pause
exit
