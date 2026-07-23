@echo off

cd %~dp0

call "..\..\manage\env\activate_env.cmd" "emme"
python ".\src\prepare_conformity_scenario.py"

pause