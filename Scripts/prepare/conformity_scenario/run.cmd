@echo off

cd %~dp0

uv run ".\src\prepare_conformity_scenario.py"

pause