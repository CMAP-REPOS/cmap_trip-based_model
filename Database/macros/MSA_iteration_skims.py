## MSA_iteration_skims.py
##
## Create time and distance skims from am-peak (xxxx3) and mid-day (xxxx5) scenarios for
## the Destination Choice-Mode Choice model. This uses link volumes derived by applying the 
## Method of Successive Averages (MSA) between global iteration scenarios. Time and distance
## skims are prepared for the following time periods and stored in these matricies:
##  - mf44 = am peak 7-9 am SOV hwy time skims (s=xxxx3)
##  - mf45 = am peak 7-9 am SOV hwy distance skims (s=xxxx3)
##  - mf46 = midday 10 am - 2 pm SOV hwy time skims (s=xxxx5)
##  - mf47 = midday 10 am - 2 pm SOV hwy distance skims (s=xxxx5)
##  - mf76 = am peak 7-9 am HOV hwy time skims (s=xxxx3)
##  - mf77 = am peak 7-9 am HOV hwy distance skims (s=xxxx3)
##  - mf78 = midday 10 am - 2 pm HOV hwy time skims (s=xxxx5)
##  - mf79 = midday 10 am - 2 pm HOV hwy distance skims (s=xxxx5)
##
##    ms97 = dummy operand for demand matrix in assignment setup
##    extra function parameter (el1) used in averaging vdfs
##
## This is the Python SOLA implementation of the Emme macro skim5I_7c.mac.
##
## Heither 11-11-2022
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
tod = int(sys.argv[2])
currentScen = int(sys.argv[3])
globalIter = int(sys.argv[4])
sThreads = int(sys.argv[5])

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)
my_emmebank = my_modeller.emmebank

delete_function = my_modeller.tool("inro.emme.data.function.delete_function")

default_path = os.getcwd().replace("\\","/")
vdf_file = os.path.join(default_path,"data/vdf_iter.in")
readin_functions = my_modeller.tool("inro.emme.data.function.function_transaction")

copy_scenario = my_modeller.tool("inro.emme.data.scenario.copy_scenario")
skimScen = my_emmebank.scenario(currentScen)

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")

create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")

set_extra_function_parameters = my_modeller.tool("inro.emme.traffic_assignment.set_extra_function_parameters")

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
delete_scenario = my_modeller.tool("inro.emme.data.scenario.delete_scenario")
 
if tod == 3:
    holdSkims = ["mf44", "mf45", "mf76", "mf77"] ##-- storage matrices        
else:
    holdSkims = ["mf46", "mf47", "mf78", "mf79"] ##-- storage matrices     
 
solaIterSpec = {
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
                    "shortest_paths": holdSkims[0]
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
                        "od_values": holdSkims[1]
                    }
                }
            ]
        },
        {
            "mode": "H",
            "demand": "ms97",
            "generalized_cost": None,
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": holdSkims[2]
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
                        "od_values": holdSkims[3]
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

#---------------------------------------------------------------------------------------
with _m.logbook_trace("MSA Time and Distance Skims for Destination Choice-Mode Choice model: Scenario %s" % currentScen):
    ## -- 1. Delete current volume delay functions (fd1 through fd10) -- ## 
    for i in range(1,11):
        f_id = "fd%s" % i
        f = my_modeller.emmebank.function(f_id)
        if f is not None:
           report = delete_function(f)

    ## -- 2. Load volume delay functions -- ## 
    report = readin_functions(transaction_file=vdf_file, throw_on_error=True)

    ## -- 3. Copy scenario for skimming and set as primary scenario -- ## 
    s90 = copy_scenario(from_scenario=skimScen,
                          scenario_id=90,
                          scenario_title="temp scenario for skimming",
                          copy_linkshapes=True,
                          overwrite=True,
                          set_as_primary=True)

    ## -- 4. Initialize time and distance matrices -- ##
    if tod == 3:
        new_mf44 = matrix_init(matrix_id="mf44",
                            matrix_name="amtime",
                            matrix_description="am peak 7-9am SOV hwy time skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0) 
        new_mf45 = matrix_init(matrix_id="mf45",
                            matrix_name="amdist",
                            matrix_description="am peak 7-9am SOV hwy distance skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)
        new_mf76 = matrix_init(matrix_id="mf76",
                            matrix_name="amhovt",
                            matrix_description="am peak 7-9am HOV hwy time skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)                         
        new_mf77 = matrix_init(matrix_id="mf77",
                            matrix_name="amhovd",
                            matrix_description="am peak 7-9am HOV hwy distance skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0) 
    else:
        new_mf46 = matrix_init(matrix_id="mf46",
                            matrix_name="mdtime",
                            matrix_description="midday 10a-2p SOV hwy time skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)                         
        new_mf47 = matrix_init(matrix_id="mf47",
                            matrix_name="mddist",
                            matrix_description="midday 10a-2p SOV hwy distance skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)   
        new_mf78 = matrix_init(matrix_id="mf78",
                            matrix_name="mdhovt",
                            matrix_description="midday 10a-2p HOV hwy time skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)                        
        new_mf79 = matrix_init(matrix_id="mf79",
                            matrix_name="mdhovd",
                            matrix_description="midday 10a-2p HOV hwy distance skim balanced iter%s" % (globalIter),
                            overwrite=True,
                            default_value=0)

    ## -- 5. Create @avtot to hold the sum of class average volumes -- ##
    new_att = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@avtot",
                           extra_attribute_description="total average volume (veq)",
                           overwrite=True) 
                           
    calcSpec = {
        "type": "NETWORK_CALCULATION",
        "result": "@avtot",
        "expression": "@avs1v+@avs2v+@avs3v+@avh2v+@avh3v+@avbqv+@avlqv+@avmqv+@avhqv",
        "aggregation": None,
        "selections": {
            "link": "all"}}

    report = netcalc(calcSpec, full_report=False)

    ## -- 6. Calculate time and distance skims -- ##
    set_extra_function_parameters(
                 el1="@avtot",
                 el2="@busveq")
    report = assign_SOLA(solaIterSpec)

    ## -- 7. Reset primary scenario -- ##
    change_scenario(scenario=currentScen)

    ## -- 8. Delete temporary skim scenario (cannot be current scenario) -- ##
    s90 = my_emmebank.scenario(90)
    delete_scenario(scenario=s90)
    print("         MSA Time and Distance Skims Completed for Time Period {0}".format(tod))

    ## -- 9. Increment global iteration counter (ms98) -- ##
    ms98Spec = {
        "type": "MATRIX_CALCULATION",
        "result": "ms98",
        "expression": "ms98 + 1",
        "constraint": {
            "by_zone": None,
            "by_value": None
        }}
    
if tod == 5:    
    report = compute_matrix(ms98Spec) 
    print("         Global Iteration Counter Incremented")
