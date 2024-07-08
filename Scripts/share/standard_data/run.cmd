@echo off

call ..\..\manage\env\activate_env.cmd emme
python src\standard_data.py

pause