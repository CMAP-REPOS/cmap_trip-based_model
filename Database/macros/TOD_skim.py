## TOD_skim.py
##
## Create travel time (and distance) skims for each time period, which are used in the utility calculations in
## the time-of-day model. Travel time skims are stored in the following matrices:
##  - mf461 = overnight 8p-6a SOV hwy time skim
##  - mf462 = am pre-pk shoulder 6a-7a SOV hwy time skim
##  - mf463 = am peak 7a-9a SOV time skim
##  - mf464 = am post-pk shoulder 9a-10a SOV hwy time skim
##  - mf465 = midday 10a-2p SOV hwy time skim
##  - mf466 = pm pre-pk shoulder 2p-4p SOV hwy time skim
##  - mf467 = pm peak 4p-6p SOV hwy time
##  - mf468 = pm post-pk shoulder 6p-8p SOV hwy time skim
##
## Travel distance skims are stored in the following matrices:
##  - mf471 = overnight 8p-6a SOV hwy time skim
##  - mf472 = am pre-pk shoulder 6a-7a SOV hwy time skim
##  - mf473 = am peak 7a-9a SOV time skim
##  - mf474 = am post-pk shoulder 9a-10a SOV hwy time skim
##  - mf475 = midday 10a-2p SOV hwy time skim
##  - mf476 = pm pre-pk shoulder 2p-4p SOV hwy time skim
##  - mf477 = pm peak 4p-6p SOV hwy time
##  - mf478 = pm post-pk shoulder 6p-8p SOV hwy time skim
##
##    ms97 = dummy operand for demand matrix in assignment setup
##    extra function parameter (el1) used in averaging vdfs
##
## This is the SOLA Python implementation of the Emme macro skimTOD5I_7c.mac.
##
## Heither, rev. 01-12-2025
## ==========================================================================================
import os
import sys
from pathlib import Path
import inro.modeller as _m
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

tod = int(sys.argv[1])
currentScen = int(sys.argv[2])
sThreads = int(sys.argv[3])

proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")
matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
my_emmebank = my_modeller.emmebank
delete_function = my_modeller.tool("inro.emme.data.function.delete_function")
readin_functions = my_modeller.tool("inro.emme.data.function.function_transaction")
change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
delete_scenario = my_modeller.tool("inro.emme.data.scenario.delete_scenario")
copy_scenario = my_modeller.tool("inro.emme.data.scenario.copy_scenario")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
set_extra_function_parameters = my_modeller.tool("inro.emme.traffic_assignment.set_extra_function_parameters")
skimScen = my_emmebank.scenario(currentScen)
## =======================================================================================================================

default_path = os.getcwd().replace("\\","/")
vdf_file = os.path.join(default_path,"data/vdf_iter.in")

mtxIndex = tod-1

## -- Initialize storage matrices -- ##
mtx = ["mf461","mf462","mf463","mf464","mf465","mf466","mf467","mf468"]     ##-- TOD time storage matrices 
mtx2 = ["mf471","mf472","mf473","mf474","mf475","mf476","mf477","mf478"]    ##-- TOD distance storage matrices 
#
new_mf1 = matrix_init(matrix_id="%s" %(mtx[mtxIndex]), matrix_name="todtime_%s" %(tod),
                        matrix_description="SOV hwy time skim period %s" %(tod), overwrite=True, default_value=0)
new_mf2 = matrix_init(matrix_id="%s" %(mtx2[mtxIndex]), matrix_name="toddist_%s" %(tod),
                        matrix_description="SOV hwy distance skim period %s" %(tod), overwrite=True, default_value=0)

## -- Delete current volume-delay functions (fd1 - fd10) -- ##
for i in range(1,11):
    f_id = "fd%s" % i
    f = my_modeller.emmebank.function(f_id)
    if f is not None:
        delete_function(f)
## -- Import global iteration vdf functions -- ##
readin_functions(transaction_file=vdf_file, throw_on_error=True)

## -- Copy scenario for skimming and set as primary scenario -- ## 
s90 = copy_scenario(from_scenario=skimScen,
                    scenario_id=90,
                    scenario_title="temp scenario for skimming",
                    copy_linkshapes=True,
                    overwrite=True,
                    set_as_primary=True)

## -- Create @avtot to hold the sum of class average volumes -- ##
new_att = create_extra(extra_attribute_type="LINK",
                        extra_attribute_name="@avtot",
                        extra_attribute_description="total average volume (veq)",
                        overwrite=True) 
                           
calcSpec = {"type": "NETWORK_CALCULATION", "result": "@avtot",
            "expression": "@avs1v+@avs2v+@avs3v+@avh2v+@avh3v+@avbqv+@avlqv+@avmqv+@avhqv",
            "aggregation": None, "selections": {"link": "all"}}
report = netcalc(calcSpec, full_report=False)

## -- Set extra function parameters -- ##
set_extra_function_parameters(el1="@avtot", el2="@busveq")
         

skimSpec = {
    "type": "SOLA_TRAFFIC_ASSIGNMENT",
    "classes": [
        {
            "mode": "S",
            "demand": "ms97",
            "generalized_cost": None,
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": mtx[mtxIndex]
                }
            },
            "path_analyses": [
                {
                    "link_component": "length",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": None,
                        "upper": None
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": mtx2[mtxIndex]
                    }
                }
            ]
        }
    ],
    "performance_settings": {
        "number_of_processors": sThreads
    },
    "background_traffic": None,
    "stopping_criteria": {
        "max_iterations": 0,
        "relative_gap": 0.0001,
        "best_relative_gap": 0.01,
        "normalized_gap": 0.005
    }
}

with _m.logbook_trace("Travel Time (and Distance) Skims for Time-of-Day Model: Scenario %s" % currentScen):
    report = assign_SOLA(skimSpec)

    ## -- Reset primary scenario -- ##
    change_scenario(scenario=currentScen)

    ## -- Delete temporary skim scenario (cannot be current scenario) -- ##
    s90 = my_emmebank.scenario(90)
    delete_scenario(scenario=s90)
print("         Travel Time and Distance Skims Completed for Time Period {0}".format(tod))  
