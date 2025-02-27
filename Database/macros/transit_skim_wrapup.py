'''
 transit_skim_wrapup.py

 Finalizes the transit skimming process: transit skim matrices moved to final locations in preparation for the destination choice-
 mode choice model and temporary matrices deleted.


 Arguments:  1= time period indicator: AM, MD, PM, EA


 Craig Heither, 01-14-2025
 ==========================================================================================       
'''

import os
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

timePeriod = sys.argv[1]

maxInternal=3632                                    ## -- highest non-POE zone number

proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)
my_emmebank = my_modeller.emmebank

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
###########################################################################################################################

if timePeriod == 'AM':
    mtx = ("mf837","mf838","mf821","mf824")
    a = list(range(803,820))                ## -- matrices to delete
elif timePeriod == 'MD':
    mtx = ("mf937","mf938","mf921","mf924")
    a = list(range(903,920))                ## -- matrices to delete 
elif timePeriod == 'PM':
    mtx = ("mf887","mf888","mf871","mf874")
    a = list(range(853,870))                ## -- matrices to delete
elif timePeriod == 'EA':
    mtx = ("mf987","mf988","mf971","mf974")
    a = list(range(953,970))                ## -- matrices to delete 


## -- Initialize matrices -- ##
new_mf1 = matrix_init(matrix_id="%s" %(mtx[0]), matrix_name="kasmtp_%s" %(timePeriod),
                        matrix_description="indexed station zone for transit assignment - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf2 = matrix_init(matrix_id="%s" %(mtx[1]), matrix_name="phdway_%s" %(timePeriod),
                        matrix_description="indexed headway (indexed wait time x 2) - %s" %(timePeriod), overwrite=True, default_value=0)
## -- Create indexed station zone matrix for transit assignment: equal to mfx21 [05-06-2022] -- ##
m1Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[0]), "expression": "%s" %(mtx[2]),
        "constraint": {
            "by_value": None, 
            "by_zone": {"origins": "1,%s" %(maxInternal), "destinations": "1,%s" %(maxInternal)}
        },        
        "aggregation": {"origins": None, "destinations": None},
    }
## -- Recalculate headway matrix -- ##
m2Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[1]), "expression": "%s*2" %(mtx[3]),
        "constraint": {"by_value": None, "by_zone": None},        
        "aggregation": {"origins": None, "destinations": None},
    }
report = compute_matrix([m1Spec,m2Spec])
## -- Initialize ui3 -- ##
calcUi3 = {"type": "NETWORK_CALCULATION", "result": "ui3", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
report=netcalc(calcUi3, full_report=False)

## -- Delete unneeded matrices -- ##
for item in a:
    matrix = my_emmebank.matrix("mf" + str(item))
    if matrix:
        delete_matrix(matrix)
      
print(' -- {0} transit skim matrix clean up done'.format(timePeriod))