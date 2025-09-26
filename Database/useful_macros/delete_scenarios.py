'''
`delete_scenarios.py`
Author: Tim O'Leary
Date: 9/26/2025

Description: This script deletes specified scenarios from the emme databank. It can 
be called as a subscript or by itself. Contains delete_scenarios() function that can take 
a string or a list of strings as input.

Translated to Python 3 from 'delete.scenarios' Emme macro 
Written by DBE 5/2004 and modified to handle non-existent scenarios by CH 3/2009
Added functionality with Python translation:
- Can call a single scenario, a range of scenarios, or list
- Runs interactively with user input if called by itself, or the function 'delete_scenarios()' can be imported and called by another script
'''

print('delete_scenarios.py\nstarting up...')
import os
import sys
from pathlib import Path
import textwrap

proj_dir = Path(__file__).resolve().parents[2]
sys.path.append(str(proj_dir.joinpath('Scripts')))
from tbmtools import project as tbm

#connect to modeller
modeller = tbm.connect(proj_dir)
emmebank = modeller.emmebank

#define tools
delete_scenario = modeller.tool('inro.emme.data.scenario.delete_scenario')

def delete_scenarios(scens_to_delete):
    '''
    Deletes specified scenarios from the databank. 
    Input may be one of the following: 
    - a single scenario (e.g., '20003')
    - a range of scenarios separated by a hyphen (e.g., '20021-20028')
    - a list of scenarios (e.g., '221, 22003, 22004'))
    '''
    
    bad_input_msg = textwrap.dedent(f'''\
        Input not recognized. Please input scenario IDs as integers in one of the following formats:
        \t- a single scenario ID (e.g., '20003');
        \t- a range of IDs with hyphen between (e.g., '20021-20028'); or 
        \t- a list of IDs (e.g., '221, 22003, 22004').''')
    
    #if passed function with no input
    if scens_to_delete is None or scens_to_delete == '':
        raise ValueError(bad_input_msg)
    
    #check for incorrectly formatted input
    if '-' in str(scens_to_delete):
        input_type = 'range'
        scens_check = str(scens_to_delete).split('-')
        scens_check = [m.strip() for m in scens_check]
    else:
        input_type = 'list'
        scens_check = str(scens_to_delete).replace('[','').replace(']','').split(',')
        scens_check = [m.strip() for m in scens_check]
    
    #make sure prefixes and suffixes aren't invalid, and check if mixed matrix types
    not_a_number = [m for m in scens_check if not m.isnumeric()]
    if not_a_number:
        raise ValueError(bad_input_msg)
   
    #handle range input
    if input_type == 'range':
        min = int(scens_check[0])
        max = int(scens_check[1])
        if min >= max:
            raise ValueError('Your min should be less than your max.')
        scens = [m for m in range(min, max+1)]
    else:
        scens = scens_check
    
    #delete matrices
    for scen in scens:
        if emmebank.scenario(scen):
            delete_scenario(emmebank.scenario(scen))
        else:
            print(f'  - scenario {scen} not in dabatank, skipping...')
    
print('''
    DELETE_SCENARIOS.PY
    This script will delete specified scenarios from the emme databank! 
    Input may be one of the following: 
    - a single scenario ID (e.g., '20027')
    - a range of scenarios, min and max separated by a hyphen (e.g., '221-227')
    - a list of scenario IDs, separated by commas (e.g., '5, 8, 20005, 20008'))
    ''')

while True:
    try:
        user_input = input('Enter scenarios to delete (or "exit" to quit): ')
        if user_input.lower() == 'exit':
            break
        delete_scenarios(user_input)
        print('\n  - done! want to go again?\n')
    except ValueError as e:
        print(f'\n!! Error: {e}\n')
    except Exception as e:
        print(f'\n!! Unexpected error occurred: {e}\n')
print('bye-bye, then!')
input('press any key to exit')
sys.exit()
