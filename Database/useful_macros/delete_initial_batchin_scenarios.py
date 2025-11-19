'''
delete_initial_batchin_scenarios.py

author: Craig Heither, 3/31/09
    - deletes scenarios
    - submit with 3-digit scenario number
    - modified 7/26/2016: also delete build.turn.rpt
translated: Tim O'Leary, 9/26/2025 to Python 3
    - reads batch_file.yaml to retrieve 3-digit scenario number
'''

import os
import sys
from pathlib import Path
import yaml

proj_dir = Path(__file__).resolve().parents[2]
sys.path.append(str(proj_dir.joinpath('Scripts')))
from tbmtools import project as tbm

print('delete initial batchin scenarios (*0001-*0008)')
print('(and build_turn.rpt if it exists)')
print('  executing...')

#connect to modeller
modeller = tbm.connect(proj_dir)
emmebank = modeller.emmebank

#define tools
delete_scenario = modeller.tool('inro.emme.data.scenario.delete_scenario')

#get scenario info from batch_file.yaml for scenario numbers to delete
db = proj_dir.joinpath('Database')
with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)
scen_yr = config['scenario_code']  # e.g., '200'
scenarios_to_delete = [f'{scen_yr}0{i}' for i in range(1,9)] + [f'{scen_yr}0']

#delete scenarios
for scen in scenarios_to_delete:
    if emmebank.scenario(scen): #returns None if DNE
        delete_scenario(emmebank.scenario(scen))
    else:
        print(f'scenario {scen} does not exist')

#delete build_turn.rpt if it exists
rptfile = db.joinpath('report/build_turn.rpt')
if os.path.exists(rptfile):
    os.remove(rptfile)
else:
    print('build_turn.rpt does not exist')

print('done')