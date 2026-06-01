#filename: free.skim.mac.py
#description: creates time and distance skims from am-peak and midday scenarios to start distribution and mode choice
#author: Karly Cazzato, 12/05/2023

import os
import os.path
import sys
import inro.emme.desktop.app as _app
import inro.modeller as _m

   
def main():

    #set variables
    currentScen = int(sys.argv[2])
    scenario = str(currentScen)
    directory = os.getcwd().replace('\\Database','')
    batchin = directory + "\\Database\\data\\vdf_free.in"

    # Define the path to the Emme project (.emp file)
    empFl = sys.argv[1]
    empFile = os.path.join(directory,empFl)
    desktop = _app.start_dedicated(
        visible=True,
        user_initials='KCC',
        project= empFile
    )
    
    # Connect to the Modeller
    modeller = _m.Modeller(desktop=desktop)
    emmebank = modeller.emmebank

    #define tools
    delete_function = modeller.tool("inro.emme.data.function.delete_function")
    batchin_function = modeller.tool("inro.emme.data.function.function_transaction")
    
    create_matrix = modeller.tool("inro.emme.data.matrix.create_matrix")
    matrix_calculation = modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
    delete_matrix = modeller.tool("inro.emme.data.matrix.delete_matrix")

    copy_scenario = modeller.tool("inro.emme.data.scenario.copy_scenario")
    change_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")
    delete_scenario = modeller.tool("inro.emme.data.scenario.delete_scenario")

    net_calc = modeller.tool("inro.emme.network_calculation.network_calculator")

    sola_assign_traffic = modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")

    #delete existing functions for skimming and batch in new functions
    for i in range(1,10):
        f_id = "fd%s" % i
        f = _m.Modeller().emmebank.function(f_id)
        if f: delete_function(f)

    #input functions using process function transaction tool
    batchin_function(transaction_file = batchin, throw_on_error = True)

    #initialize base toll matrices
    mf111 = create_matrix(matrix_id="mf111", matrix_name="tshwl", matrix_description="low inc h-w sov toll", default_value=0, overwrite = "True")
    mf112 = create_matrix(matrix_id="mf112", matrix_name="th2hwl", matrix_description="low inc h-w hov 2 per toll", default_value=0, overwrite = "True")
    mf117 = create_matrix(matrix_id="mf117", matrix_name="tsho", matrix_description="h-o sov toll", default_value=0, overwrite = "True")
    mf118 = create_matrix(matrix_id="mf118", matrix_name="th2ho", matrix_description="h-o hov 2 per toll", default_value=0, overwrite = "True")
    
    spec_mf112 = {"expression": "mf111", "result": "mf112", "type": "MATRIX_CALCULATION"}
    spec_mf118 = {"expression": "mf117", "result": "mf118", "type": "MATRIX_CALCULATION"}
    
    matrix_calculation([spec_mf112, spec_mf118])
    
    #set scenario and define temporary copy of AM and MD scenarios for skimming
    scen = currentScen*100
    scen = scen + 3
    currentScen = currentScen+3         ## -- set to AM transit skim scenario
    scenarioWorking = emmebank.scenario(scen)
    s93 = copy_scenario(from_scenario=scenarioWorking, scenario_id=93, scenario_title="temp am peak scenario for skimming", overwrite=True)

    #then repeate with midday period scenario
    scen = scen + 2
    scenarioWorking = emmebank.scenario(scen)
    s95 = copy_scenario(from_scenario=scenarioWorking, scenario_id=95, scenario_title="temp midday scenario for skimming", overwrite=True)

    #spec to @ftime to ul1
    ftime_to_ul1 = {
        "result": "ul1",
        "expression": "@ftime",
        "selections": {
            "link": "!vdf = 6,7",
        },
        "type": "NETWORK_CALCULATION"
    }

    periodMat = {
        93 : ["S", #mode class [0]
              "mf44", "amtime", "am peak 7-9am SOV hwy time skim initsim", #[1, 2, 3]
              "mf45", "amdist", "am peak 7-9am SOV hwy distance skim init", #[4, 5, 6]
              "mf111", #matrix to hold additional attribute, toll SOVAM, [7]
              "H", #mode class [8]
              "mf76", "amhovt", "am peak 7-9am HOV hwy time skim initsim", #[9, 10, 11]
              "mf77", "amhovd", "am peak 7-9am HOV hwy distance skim init", #[12, 13, 14]
              "mf112"], #matrix to hold additional attribute, toll HOVAM [15]
        95 : ["S", #mode class, [0]
              "mf46", "mdtime", "midday 10a-2p SOV hwy time skim initsim", #[1, 2, 3]
              "mf47", "mddist", "midday 10a-2p SOvhwy distance skim init", #[4, 5, 6]
              "mf117", #matrix to hold additional attribute, toll SOVMD, [7]
              "H", #mode class, [8]
              "mf78", "mdhovt", "midday 10a-2p HOV hwy time skim initsim", #[9, 10, 11]
              "mf79", "mdhovd", "midday 10a-2p HOV hwy distance skim init", #[12, 13, 14]
              "mf118"] #matrix to hold additional attribute, toll HOVMD
    }
    #the loop will say for scenario in periodMat
    #then call periodMat[scenario][#]
    for scenario in periodMat:
        change_scenario(scenario = str(scenario)) #set scenario
        net_calc(ftime_to_ul1) #put @ftime in ul1

        delete_matrix(periodMat[scenario][1]) #delete first matrix in the list
        new_mat = create_matrix(matrix_id=periodMat[scenario][1], matrix_name=periodMat[scenario][2], matrix_description=periodMat[scenario][3], default_value=0.0, overwrite ="True") 
        delete_matrix(periodMat[scenario][4]) #delete second matrix in the list
        new_mat = create_matrix(matrix_id=periodMat[scenario][4], matrix_name=periodMat[scenario][5], matrix_description=periodMat[scenario][6], default_value=0.0, overwrite ="True") 
        
        traffic_assignment = {
            "type": "SOLA_TRAFFIC_ASSIGNMENT",
            "background_traffic": None,
            "classes": [
                {#start mode SOV
                    "mode": periodMat[scenario][0], #mode class
                    "demand": "ms97", #dummy matrix to hold assignment demand
                    "results": {
                        "od_travel_times": {
                            "shortest_paths": periodMat[scenario][1] #store skimmed time
                            } 
                    },
                    "path_analyses": [
                        {
                            "link_component": "length",
                            "operator": "+",
                            "selection_threshold": {
                               "lower": .0000000001,
                               "upper": 999999
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
                                "od_values": periodMat[scenario][4]
                                }
                        },

                        {
                            "link_component": "@toll",
                            "operator": "+",
                            "selection_threshold": {
                               "lower": .0000000001,
                               "upper": 999999
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
                                "od_values": periodMat[scenario][7]
                                }
                        }
                        ]
                },
                
                { #start mode HOV
                    "mode": periodMat[scenario][8], #mode class
                    "demand": "ms97", #dummy matrix to hold assignment demand
                    "results": {
                        "od_travel_times": {
                            "shortest_paths": periodMat[scenario][9] #store skimmed time
                            } 
                    },
                    "path_analyses": [
                        {
                            "link_component": "length",
                            "operator": "+",
                            "selection_threshold": {
                               "lower": .0000000001,
                               "upper": 999999
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
                                "od_values": periodMat[scenario][12]}
                        },
                        {
                            "link_component": "@toll",
                            "operator": "+",
                            "selection_threshold": {
                               "lower": .0000000001,
                               "upper": 999999
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
                                "od_values": periodMat[scenario][15]
                            }
                        }
                    ]
                } 

                ],
                
                "performance_settings": {
                        "number_of_processors": "max"
                },
                "stopping_criteria": {
                    "max_iterations": 0,
                    "relative_gap": 0.0001,
                    "best_relative_gap": 0.01,
                    "normalized_gap": 0.001
                }
            }
        #setup highway asmt - see spec above
        #run assignment
        sola_assign_traffic(traffic_assignment)
        print("ran spec for " + str(scenario))

        #change scenario (can't delete current primary scenario)
        change_scenario(scenario = str(currentScen)) #set scenario

        #delete temporary scenarios
        sdel = emmebank.scenario(scenario)
        delete_scenario(scenario = sdel)

    print("completed loop")

    #initialize time of day skim travel time matrices and toll skims 
    mf461 = create_matrix(matrix_id="mf461", matrix_name="todtime1", matrix_description="SOV hwy time skim period 1", default_value=0, overwrite = "True") 
    mf462 = create_matrix(matrix_id="mf462", matrix_name="todtime2", matrix_description="SOV hwy time skim period 2", default_value=0, overwrite = "True") 
    mf463 = create_matrix(matrix_id="mf463", matrix_name="todtime3", matrix_description="SOV hwy time skim period 3", default_value=0, overwrite = "True") 
    mf464 = create_matrix(matrix_id="mf464", matrix_name="todtime4", matrix_description="SOV hwy time skim period 4", default_value=0, overwrite = "True") 
    mf465 = create_matrix(matrix_id="mf465", matrix_name="todtime5", matrix_description="SOV hwy time skim period 5", default_value=0, overwrite = "True") 
    mf466 = create_matrix(matrix_id="mf466", matrix_name="todtime6", matrix_description="SOV hwy time skim period 6", default_value=0, overwrite = "True") 
    mf467 = create_matrix(matrix_id="mf467", matrix_name="todtime7", matrix_description="SOV hwy time skim period 7", default_value=0, overwrite = "True") 
    mf468 = create_matrix(matrix_id="mf468", matrix_name="todtime8", matrix_description="SOV hwy time skim period 8", default_value=0, overwrite = "True") 

    mf113 = create_matrix(matrix_id="mf113", matrix_name="th3hwl", matrix_description="low inc h-w hov 3+ per toll", default_value=0, overwrite = "True") 
    mf114 = create_matrix(matrix_id="mf114", matrix_name="tshwh", matrix_description="high inc h-w sov toll", default_value=0, overwrite = "True") 
    mf115 = create_matrix(matrix_id="mf115", matrix_name="th2hwh", matrix_description="high inc h-w hov 2 per toll", default_value=0, overwrite = "True") 
    mf116 = create_matrix(matrix_id="mf116", matrix_name="th3hwh", matrix_description="high inc h-w hov 3+ per toll", default_value=0, overwrite = "True") 
    mf119 = create_matrix(matrix_id="mf119", matrix_name="th3ho", matrix_description="h-o hov 3+ per toll", default_value=0, overwrite = "True") 
    mf120 = create_matrix(matrix_id="mf120", matrix_name="tsnh", matrix_description="nh sov toll", default_value=0, overwrite = "True") 
    mf121 = create_matrix(matrix_id="mf121", matrix_name="th2nh", matrix_description="nh hov 2 per toll", default_value=0, overwrite = "True") 
    mf122 = create_matrix(matrix_id="mf122", matrix_name="th3nh", matrix_description="nh hov 3+ per toll", default_value=0, overwrite = "True") 

    spec_mf461 = {"expression": "mf46", "result": "mf461", "type": "MATRIX_CALCULATION"}
    spec_mf462 = {"expression": "mf44", "result": "mf462", "type": "MATRIX_CALCULATION"}
    spec_mf463 = {"expression": "mf44", "result": "mf463", "type": "MATRIX_CALCULATION"}
    spec_mf464 = {"expression": "mf44", "result": "mf464", "type": "MATRIX_CALCULATION"}
    spec_mf465 = {"expression": "mf46", "result": "mf465", "type": "MATRIX_CALCULATION"}
    spec_mf466 = {"expression": "mf44", "result": "mf466", "type": "MATRIX_CALCULATION"}
    spec_mf467 = {"expression": "mf44", "result": "mf467", "type": "MATRIX_CALCULATION"}
    spec_mf468 = {"expression": "mf44", "result": "mf468", "type": "MATRIX_CALCULATION"}
    matrix_calculation([spec_mf461, spec_mf462, spec_mf463, spec_mf464, spec_mf465, spec_mf466, spec_mf467, spec_mf468])

    spec_mf113 = {"expression": "mf112", "result": "mf113", "type": "MATRIX_CALCULATION"}
    spec_mf114 = {"expression": "mf111", "result": "mf114", "type": "MATRIX_CALCULATION"}
    spec_mf115 = {"expression": "mf112", "result": "mf115", "type": "MATRIX_CALCULATION"}
    spec_mf116 = {"expression": "mf112", "result": "mf116", "type": "MATRIX_CALCULATION"}
    spec_mf119 = {"expression": "mf118", "result": "mf119", "type": "MATRIX_CALCULATION"}
    spec_mf120 = {"expression": "mf117", "result": "mf120", "type": "MATRIX_CALCULATION"}
    spec_mf121 = {"expression": "mf118", "result": "mf121", "type": "MATRIX_CALCULATION"}
    spec_mf122 = {"expression": "mf118", "result": "mf122", "type": "MATRIX_CALCULATION"}
    matrix_calculation([spec_mf113, spec_mf114, spec_mf115, spec_mf116, spec_mf119, spec_mf120, spec_mf121, spec_mf122])

    #convert tolls to cents
    toll111 = {"expression": "mf111*100", "result": "mf111", "type": "MATRIX_CALCULATION"}
    toll112 = {"expression": "mf112*100", "result": "mf112", "type": "MATRIX_CALCULATION"}
    toll113 = {"expression": "mf113*100", "result": "mf113", "type": "MATRIX_CALCULATION"}
    toll114 = {"expression": "mf114*100", "result": "mf114", "type": "MATRIX_CALCULATION"}
    toll115 = {"expression": "mf115*100", "result": "mf115", "type": "MATRIX_CALCULATION"}
    toll116 = {"expression": "mf116*100", "result": "mf116", "type": "MATRIX_CALCULATION"}
    toll117 = {"expression": "mf117*100", "result": "mf117", "type": "MATRIX_CALCULATION"}
    toll118 = {"expression": "mf118*100", "result": "mf118", "type": "MATRIX_CALCULATION"}
    toll119 = {"expression": "mf119*100", "result": "mf119", "type": "MATRIX_CALCULATION"}
    toll120 = {"expression": "mf120*100", "result": "mf120", "type": "MATRIX_CALCULATION"}
    toll121 = {"expression": "mf121*100", "result": "mf121", "type": "MATRIX_CALCULATION"}
    toll122 = {"expression": "mf122*100", "result": "mf122", "type": "MATRIX_CALCULATION"}
    matrix_calculation([toll111, toll112, toll113, toll114, toll115, toll116, toll117, toll118, toll119, toll120, toll121, toll122])
        
if __name__ == '__main__':
    main()