'''
 transit_skim_wrapup.py

 Finalizes the transit skimming process: transit skim matrices moved to final locations in preparation for the destination choice-
 mode choice model and temporary matrices deleted.


 Arguments:  1= name of Emme project file
             2= time period indicator: AM or MD 

 Craig Heither, 03-25-2024
 ==========================================================================================       
'''

import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
timePeriod = sys.argv[2]

maxInternal=3632                                    ## -- highest non-POE zone number

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)
my_emmebank = my_modeller.emmebank

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
###########################################################################################################################

## -- Initialize matrices -- ##
mtx = ("mf837","mf838","mf821","mf824")
if timePeriod == 'MD':
    mtx = ("mf937","mf938","mf921","mf924")

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
a = list(range(803,820))  
if timePeriod == 'MD':
    a = list(range(903,920)) 

for item in a:
    matrix = my_emmebank.matrix("mf" + str(item))
    if matrix:
        delete_matrix(matrix)
      
print(' -- {0} transit skim matrix clean up done'.format(timePeriod))