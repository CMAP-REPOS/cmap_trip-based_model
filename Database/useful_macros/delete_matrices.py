'''
`delete_matrices.py`
Author: Tim O'Leary
Date: 9/26/2025

Description: This script deletes specified matrices from the emme databank. It can 
be called as a subscript or by itself. Contains delete_matrices() function that can take 
a string or a list of strings as input.

Translated from 'delete.matrices' Emme macro written by DBE 5/11/2004
- Added functionality to call a single matrix, range, or list
- Runs interactively if called by itself, or function could be imported and called by another script
'''

print('delete_matrices.py\nstarting up...')
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
delete_matrix = modeller.tool('inro.emme.data.matrix.delete_matrix')

def delete_matrices(mats_to_delete):
    '''
    Deletes specified matrices from the databank. 
    Input may be one of the following: 
    - a single matrix (e.g., 'mf2')
    - a range of matrices separated by a hyphen (e.g., 'mf25-mf90')
    - a list of numbers (e.g., 'mf25, mf30, mf45'))
    '''
    
    bad_input_msg = textwrap.dedent(f'''\
        Input not recognized. Please input IDs with prefix (e.g., "mf"), in one of the following formats:
        \t- a single matrix ID (e.g., "mf54");
        \t- a range of IDs with hyphen between (e.g., "mf25-mf90"); or 
        \t- a list of IDs (e.g., [id1,id2,...]).''')
    
    #if passed function with no input
    if mats_to_delete is None or mats_to_delete == '':
        raise ValueError(bad_input_msg)
    
    #check for incorrectly formatted matrix IDs
    if '-' in str(mats_to_delete):
        input_type = 'range'
        mats_check = str(mats_to_delete).split('-')
        mats_check = [m.strip() for m in mats_check]
    else:
        input_type = 'list'
        mats_check = str(mats_to_delete).replace('[','').replace(']','').split(',')
        mats_check = [m.strip() for m in mats_check]
    
    #make sure prefixes and suffixes aren't invalid, and check if mixed matrix types
    bad_prefix = [m for m in mats_check if m[:2] not in ['mf', 'mo', 'md', 'ms']]
    mixed_types = [m[:2] for m in mats_check]
    bad_suffix = [m for m in mats_check if not m[2:].isdigit()]
    if len(bad_prefix + bad_suffix) > 0:
        raise ValueError(bad_input_msg)
    if len(set(mixed_types)) > 1:
        raise ValueError('Sorry, cannot mix matrix types in one call for now. Please only do one matrix type at a time.')
    
    #handle range input
    if input_type == 'range':
        mtx_type = set(mixed_types)[0]
        min = int(mats_check[0][2:])
        max = int(mats_check[1][2:])
        mats = [f'{mtx_type}{m}' for m in range(min, max+1)]
    else:
        mats = mats_check
    
    #delete matrices
    for mat in mats:
        if emmebank.matrix(mat):
            delete_matrix(emmebank.matrix(mat))
        else:
            print(f'  - matrix {mat} not in dabatank, skipping...')
    
print('''
    DELETE_MATRICES.PY
    This script will delete specified matrices from the emme databank! 
    Input may be one of the following: 
    - a single matrix (e.g., 'mf2')
    - a range of matrices, min and max separated by a hyphen (e.g., 'mf25-mf90')
    - a list of matrix IDs, separated by commas (e.g., 'mf25, mf30, mf45'))
    ''')

while True:
    try:
        user_input = input('Enter matrices to delete (or "exit" to quit): ')
        if user_input.lower() == 'exit':
            break
        delete_matrices(user_input)
        print('\n  - done! want to go again?\n')
    except ValueError as e:
        print(f'\n!! Error: {e}\n')
    except Exception as e:
        print(f'\n!! Unexpected error occurred: {e}\n')
print('bye-bye, then!')
input('press any key to exit')
sys.exit()
