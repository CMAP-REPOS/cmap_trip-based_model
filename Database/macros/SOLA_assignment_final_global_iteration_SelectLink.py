## SOLA_assignment_final_global_iteration_SelectLink.py
##
## Peforms the time-of-day SOLA traffic assignments: one specification is used for Period 3
## (HW trips), one for period 5 (non-work trips) and a third specification is used for all other
## time periods.
##
## This differs from SOLA_assignment.py because during the final global iteration the
## time-of-day assignments also capture the link volumes for medium and heavy truck trips
## of 200+ miles, used as an input to MOVES.
##
## This incorporates part of the Emme macro toll_skim.mac to save AM Peak & Midday toll skim data
## (which is used by complete_toll_skim_matrices.py) and part of the Emme macro punch.moves.data.mac,
## which ran the long distance truck trip path analysis.
##
## This differs from SOLA_assignment_final_global_iteration.py because it also conducts a select
## link analysis based on links inlcuded in the file specified in batch_file.yaml. Select link
## results are in:
##  -   @slvol: total select link volume (in vehicles) for each time period
##  -   mf60: select link daily total vehicle demand
##  -   mf68: select link period 3 auto vehicle demand (user classes 1-4)
##  -   mf69: select link period 3 truck vehicle demand (user classes 5-7)
##  -   mf61: select link daily mode S VOT1 vehicle demand
##  -   mf62: select link daily mode S VOT2 vehicle demand
##  -   mf63: select link daily mode S VOT3 vehicle demand
##  -   mf64: select link daily mode H vehicle demand
##  -   mf65: select link daily mode B and L truck vehicle demand
##  -   mf66: select link daily mode M truck vehicle demand
##  -   mf67: select link daily mode H truck vehicle demand
##
##
## Heither rev. 04-14-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
tmPeriod = int(sys.argv[2])
sThreads = int(sys.argv[3])
linkFile = sys.argv[4]

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=True, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
my_emmebank = my_modeller.emmebank

## -- Calculate the select link attribute values to flag tolled paths -- ##
calcSpec = {
    "type": "NETWORK_CALCULATION",
	"result": "@tollflag",
    "expression": "@toll > 0",
	"aggregation": None,
    "selections": {
        "link": "all"}}

## -- Calculate distance adjustment for long distance truck trips (set POE length to 50 miles) -- ##
calcSpec2 = {
    "type": "NETWORK_CALCULATION",
	"result": "@mvlen",
    "expression": "length",
	"aggregation": None,
    "selections": {
        "link": "all"}}
calcSpec2b = {
    "type": "NETWORK_CALCULATION",
	"result": "@mvlen",
    "expression": "50",
	"aggregation": None,
    "selections": {
        "link": "i=3633,3649 or j=3633,3649"}}

## -- Calculate the select link attribute values for a select link analysis -- ##
calcSpecLink = {
    "type": "NETWORK_CALCULATION",
	"result": "@sellk",
    "expression": "1",
	"aggregation": None,
    "selections": {
        "link": "~<Select_Link\%s" %(linkFile)}}

## -- Sum the select link volumes (VEQs) into vehicles -- ##
calcSpecSlvol = {
    "type": "NETWORK_CALCULATION",
	"result": "@slvol",
    "expression": "@slcl1 + @slcl2 + @slcl3 + @slcl4 + @slcl5 + (@slcl6/2) + (@slcl7/3)",
	"aggregation": None,
    "selections": {
        "link": "all"}}


## -- Initialize matrices to hold select link demand -- ##
if tmPeriod == 1:
    new_mf60 = matrix_init(matrix_id="mf60",
                            matrix_name="select_link_total",
                            matrix_description="select link daily total vehicle demand",
                            overwrite=True,
                            default_value=0) 
    new_mf61 = matrix_init(matrix_id="mf61",
                            matrix_name="select_link_sov_vot1",
                            matrix_description="select link daily mode S VOT1 vehicle demand",
                            overwrite=True,
                            default_value=0) 
    new_mf62 = matrix_init(matrix_id="mf62",
                            matrix_name="select_link_sov_vot2",
                            matrix_description="select link daily mode S VOT2 vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf63 = matrix_init(matrix_id="mf63",
                            matrix_name="select_link_sov_vot3",
                            matrix_description="select link daily mode S VOT3 vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf64 = matrix_init(matrix_id="mf64",
                            matrix_name="select_link_hov",
                            matrix_description="select link daily mode H vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf65 = matrix_init(matrix_id="mf65",
                            matrix_name="select_link_b-light",
                            matrix_description="select link daily mode B and L truck vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf66 = matrix_init(matrix_id="mf66",
                            matrix_name="select_link_medium",
                            matrix_description="select link daily mode M truck vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf67 = matrix_init(matrix_id="mf67",
                            matrix_name="select_link_heavy",
                            matrix_description="select link daily mode H truck vehicle demand",
                            overwrite=True,
                            default_value=0)
    new_mf68 = matrix_init(matrix_id="mf68",
                            matrix_name="select_link_auto_TOD3",
                            matrix_description="select link period 3 auto vehicle demand (user classes 1-4)",
                            overwrite=True,
                            default_value=0)
    new_mf69 = matrix_init(matrix_id="mf69",
                            matrix_name="select_link_truck_TOD3",
                            matrix_description="select link period 3 truck vehicle demand (user classes 5-7)",
                            overwrite=True,
                            default_value=0)
    new_mf101 = matrix_init(matrix_id="mf101",
                            matrix_name="select_link_class1_demand",
                            matrix_description="temporary select link user class 1 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf102 = matrix_init(matrix_id="mf102",
                            matrix_name="select_link_class2_demand",
                            matrix_description="temporary select link user class 2 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf103 = matrix_init(matrix_id="mf103",
                            matrix_name="select_link_class3_demand",
                            matrix_description="temporary select link user class 3 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf104 = matrix_init(matrix_id="mf104",
                            matrix_name="select_link_class4_demand",
                            matrix_description="temporary select link user class 4 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf105 = matrix_init(matrix_id="mf105",
                            matrix_name="select_link_class5_demand",
                            matrix_description="temporary select link user class 5 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf106 = matrix_init(matrix_id="mf106",
                            matrix_name="select_link_class6_demand",
                            matrix_description="temporary select link user class 6 time period demand",
                            overwrite=True,
                            default_value=0)
    new_mf107 = matrix_init(matrix_id="mf107",
                            matrix_name="select_link_class7_demand",
                            matrix_description="temporary select link user class 7 time period demand",
                            overwrite=True,
                            default_value=0)


## -- get scalar values --
ms84_val = my_emmebank.matrix("ms84").data
print("  - SOV low VOT minutes per dollar: {0:.3f}".format(ms84_val))
ms85_val = my_emmebank.matrix("ms85").data
print("  - SOV medium VOT minutes per dollar: {0:.3f}".format(ms85_val))
ms86_val = my_emmebank.matrix("ms86").data
print("  - SOV high VOT minutes per dollar: {0:.3f}".format(ms86_val))
ms87_val = my_emmebank.matrix("ms87").data
print("  - HOV weighted minutes per dollar: {0:.3f}".format(ms87_val))
ms88_val = my_emmebank.matrix("ms88").data
print("  - B-truck/Light truck weighted minutes per dollar: {0:.3f}".format(ms88_val))
ms89_val = my_emmebank.matrix("ms89").data
print("  - Medium truck minutes per dollar: {0:.3f}".format(ms89_val))
ms90_val = my_emmebank.matrix("ms90").data
print("  - Heavy truck minutes per dollar: {0:.3f}".format(ms90_val))


## -- This specification is used for all time periods except 3 (AM peak) and 5 (midday) -- ##
solaSpec = {
    "type": "SOLA_TRAFFIC_ASSIGNMENT",
    "classes": [
        {
            ## -- CLASS 1: SOV VOT1 -- ##
            "mode": "S",
            "demand": "mf94",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms84_val
            },
            "results": {
                "link_volumes": "@vsov1",
                "turn_volumes": "@tsov1",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl1",
                        "selected_turn_volumes": None,
                        "od_values": "mf101"
                    }
                }
            ]
        },
        {
            ## -- CLASS 2: SOV VOT2 -- ##
            "mode": "S",
            "demand": "mf95",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms85_val
            },
            "results": {
                "link_volumes": "@vsov2",
                "turn_volumes": "@tsov2",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl2",
                        "selected_turn_volumes": None,
                        "od_values": "mf102"
                    }
                }
            ]
        },
        {
            ## -- CLASS 3: SOV VOT3 -- ##
            "mode": "S",
            "demand": "mf96",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms86_val
            },
            "results": {
                "link_volumes": "@vsov3",
                "turn_volumes": "@tsov3",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl3",
                        "selected_turn_volumes": None,
                        "od_values": "mf103"
                    }
                }
            ]
        },
        {
            ## -- CLASS 4: HOVs -- ##
            "mode": "H",
            "demand": "mf18",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms87_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf92",
                    "results": {
                        "selected_link_volumes": "@vhov2",
                        "selected_turn_volumes": "@thov2"
                    }
                },
                {
                    "analyzed_demand": "mf93",
                    "results": {
                        "selected_link_volumes": "@vhov3",
                        "selected_turn_volumes": "@thov3"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl4",
                        "selected_turn_volumes": None,
                        "od_values": "mf104"
                    }
                }
            ]
        },
        {
            ## -- CLASS 5: LIGHT AND B-PLATE TRUCKS -- ##
            "mode": "l",
            "demand": "mf97",
            "generalized_cost": {
                "link_costs": "@toll2",
                "perception_factor": ms88_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf14",
                    "results": {
                        "selected_link_volumes": "@vbplt",
                        "selected_turn_volumes": "@tbplt"
                    }
                },
                {
                    "analyzed_demand": "mf15",
                    "results": {
                        "selected_link_volumes": "@vlght",
                        "selected_turn_volumes": "@tlght"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl5",
                        "selected_turn_volumes": None,
                        "od_values": "mf105"
                    }
                }
            ]
        },
        {
            ## -- CLASS 6: MEDIUM TRUCKS -- ##
            "mode": "m",
            "demand": "mf16",
            "generalized_cost": {
                "link_costs": "@toll3",
                "perception_factor": ms89_val
            },
            "results": {
                "link_volumes": "@vmed",
                "turn_volumes": "@tmed",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance medium trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@m200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl6",
                        "selected_turn_volumes": None,
                        "od_values": "mf106"
                    }
                }
            ]
        },
        {
            ## -- CLASS 7: HEAVY TRUCKS -- ##
            "mode": "h",
            "demand": "mf17",
            "generalized_cost": {
                "link_costs": "@toll4",
                "perception_factor": ms90_val
            },
            "results": {
                "link_volumes": "@vhevy",
                "turn_volumes": "@thevy",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance heavy trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@h200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl7",
                        "selected_turn_volumes": None,
                        "od_values": "mf107"
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
        "max_iterations": 400,
        "relative_gap": 0.0001,
        "best_relative_gap": 0.01,
        "normalized_gap": 0.005
    }
}

'''
This specification is for period 3 and it generates toll skims along accumulated paths for HW trips. Steps: identify the person movements 
traveling tolled paths by auto occupancy, Sum and then average the tolls along the used toll paths.
 ''' 
solaSpecAMPK = {
    "type": "SOLA_TRAFFIC_ASSIGNMENT",
    "classes": [
        {
            ## -- CLASS 1: SOV VOT1 -- ##
			"mode": "S",
            "demand": "mf94",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms84_val
            },
            "results": {
                "link_volumes": "@vsov1",
                "turn_volumes": "@tsov1",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: Work trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf413",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf123"
                    }
                },
                {
                    ## -- Save demand on tolled paths: Work Trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf413",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf134"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl1",
                        "selected_turn_volumes": None,
                        "od_values": "mf101"
                    }
                }
            ]
        },
        {
            ## -- CLASS 2: SOV VOT2 -- ##
			"mode": "S",
            "demand": "mf95",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms85_val
            },
            "results": {
                "link_volumes": "@vsov2",
                "turn_volumes": "@tsov2",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: Work trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf423",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf124"
                    }
                },
                {
                    ## -- Save demand on tolled paths: Work trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf423",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf135"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl2",
                        "selected_turn_volumes": None,
                        "od_values": "mf102"
                    }
                }
            ]
        },
        {
            ## -- CLASS 3: SOV VOT3 -- ##
			"mode": "S",
            "demand": "mf96",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms86_val
            },
            "results": {
                "link_volumes": "@vsov3",
                "turn_volumes": "@tsov3",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: Work trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf433",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf125"
                    }
                },
                {
                    ## -- Save demand on tolled paths: Work trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf433",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf136"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl3",
                        "selected_turn_volumes": None,
                        "od_values": "mf103"
                    }
                }
            ]
        },
        {
            ## -- CLASS 4: HOVs -- ##
			"mode": "H",
            "demand": "mf18",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms87_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf92",
                    "results": {
                        "selected_link_volumes": "@vhov2",
                        "selected_turn_volumes": "@thov2"
                    }
                },
                {
                    "analyzed_demand": "mf93",
                    "results": {
                        "selected_link_volumes": "@vhov3",
                        "selected_turn_volumes": "@thov3"
                    }
                },
				{
                    ## -- Calculate average tolls between O-D pairs: Work trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf443",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf112"
                    }
                },
				{
                    ## -- Save demand on tolled paths: Work trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf443",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf132"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl4",
                        "selected_turn_volumes": None,
                        "od_values": "mf104"
                    }
                }
            ]
        },
        {
            ## -- CLASS 5: LIGHT AND B-PLATE TRUCKS -- ##
			"mode": "l",
            "demand": "mf97",
            "generalized_cost": {
                "link_costs": "@toll2",
                "perception_factor": ms88_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf14",
                    "results": {
                        "selected_link_volumes": "@vbplt",
                        "selected_turn_volumes": "@tbplt"
                    }
                },
                {
                    "analyzed_demand": "mf15",
                    "results": {
                        "selected_link_volumes": "@vlght",
                        "selected_turn_volumes": "@tlght"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl5",
                        "selected_turn_volumes": None,
                        "od_values": "mf105"
                    }
                }
            ]
        },
        {
            ## -- CLASS 6: MEDIUM TRUCKS -- ##
			"mode": "m",
            "demand": "mf16",
            "generalized_cost": {
                "link_costs": "@toll3",
                "perception_factor": ms89_val
            },
            "results": {
                "link_volumes": "@vmed",
                "turn_volumes": "@tmed",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance medium trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@m200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl6",
                        "selected_turn_volumes": None,
                        "od_values": "mf106"
                    }
                }
            ]
        },
        {
            ## -- CLASS 7: HEAVY TRUCKS -- ##
			"mode": "h",
            "demand": "mf17",
            "generalized_cost": {
                "link_costs": "@toll4",
                "perception_factor": ms90_val
            },
            "results": {
                "link_volumes": "@vhevy",
                "turn_volumes": "@thevy",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance heavy trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@h200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl7",
                        "selected_turn_volumes": None,
                        "od_values": "mf107"
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
        "max_iterations": 400,
        "relative_gap": 0.0001,
        "best_relative_gap": 0.01,
        "normalized_gap": 0.005
    }
}

'''
This specification is for period 5 and it generates toll skims along accumulated paths for non-work trips. Steps: identify the person movements 
traveling tolled paths by auto occupancy, Sum and then average the tolls along the used toll paths.
 ''' 
solaSpecMidday = {
    "type": "SOLA_TRAFFIC_ASSIGNMENT",
    "classes": [
        {
            ## -- CLASS 1: SOV VOT1 -- ##
			"mode": "S",
            "demand": "mf94",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms84_val
            },
            "results": {
                "link_volumes": "@vsov1",
                "turn_volumes": "@tsov1",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: NonWork trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf415",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf126"
                    }
                },
				{
                    ## -- Save demand on tolled paths: NonWork Trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf415",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf140"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl1",
                        "selected_turn_volumes": None,
                        "od_values": "mf101"
                    }
                }
            ]
        },
        {
            ## -- CLASS 2: SOV VOT2 -- ##
			"mode": "S",
            "demand": "mf95",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms85_val
            },
            "results": {
                "link_volumes": "@vsov2",
                "turn_volumes": "@tsov2",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: NonWork trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf425",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf127"
                    }
                },
				{
                    ## -- Save demand on tolled paths: NonWork trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf425",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf141"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl2",
                        "selected_turn_volumes": None,
                        "od_values": "mf102"
                    }
                }
            ]
        },
        {
            ## -- CLASS 3: SOV VOT3 -- ##
			"mode": "S",
            "demand": "mf96",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms86_val
            },
            "results": {
                "link_volumes": "@vsov3",
                "turn_volumes": "@tsov3",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Calculate average tolls between O-D pairs: NonWork trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf435",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf128"
                    }
                },
				{
                    ## -- Save demand on tolled paths: NonWork trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf435",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf142"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl3",
                        "selected_turn_volumes": None,
                        "od_values": "mf103"
                    }
                }
            ]
        },
        {
            ## -- CLASS 4: HOVs -- ##
			"mode": "H",
            "demand": "mf18",
            "generalized_cost": {
                "link_costs": "@toll",
                "perception_factor": ms87_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf92",
                    "results": {
                        "selected_link_volumes": "@vhov2",
                        "selected_turn_volumes": "@thov2"
                    }
                },
                {
                    "analyzed_demand": "mf93",
                    "results": {
                        "selected_link_volumes": "@vhov3",
                        "selected_turn_volumes": "@thov3"
                    }
                },
				{
                    ## -- Calculate average tolls between O-D pairs: NonWork trips -- ##
					"link_component": "@toll",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 0.001,
                        "upper": 9999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "ALL",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": False,
                            "path_value": True
                        }
                    },
                    "analyzed_demand": "mf445",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf118"
                    }
                },
				{
                    ## -- Save demand on tolled paths: NonWork trips -- ##
					"link_component": "@tollflag",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": "mf445",
                    "results": {
                        "selected_link_volumes": None,
                        "selected_turn_volumes": None,
                        "od_values": "mf138"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl4",
                        "selected_turn_volumes": None,
                        "od_values": "mf104"
                    }
                }
            ]
        },
        {
            ## -- CLASS 5: LIGHT AND B-PLATE TRUCKS -- ##
			"mode": "l",
            "demand": "mf97",
            "generalized_cost": {
                "link_costs": "@toll2",
                "perception_factor": ms88_val
            },
            "results": {
                "link_volumes": None,
                "turn_volumes": None,
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    "analyzed_demand": "mf14",
                    "results": {
                        "selected_link_volumes": "@vbplt",
                        "selected_turn_volumes": "@tbplt"
                    }
                },
                {
                    "analyzed_demand": "mf15",
                    "results": {
                        "selected_link_volumes": "@vlght",
                        "selected_turn_volumes": "@tlght"
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl5",
                        "selected_turn_volumes": None,
                        "od_values": "mf105"
                    }
                }
            ]
        },
        {
            ## -- CLASS 6: MEDIUM TRUCKS -- ##
			"mode": "m",
            "demand": "mf16",
            "generalized_cost": {
                "link_costs": "@toll3",
                "perception_factor": ms89_val
            },
            "results": {
                "link_volumes": "@vmed",
                "turn_volumes": "@tmed",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance medium trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@m200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl6",
                        "selected_turn_volumes": None,
                        "od_values": "mf106"
                    }
                }
            ]
        },
        {
            ## -- CLASS 7: HEAVY TRUCKS -- ##
			"mode": "h",
            "demand": "mf17",
            "generalized_cost": {
                "link_costs": "@toll4",
                "perception_factor": ms90_val
            },
            "results": {
                "link_volumes": "@vhevy",
                "turn_volumes": "@thevy",
                "od_travel_times": {
                    "shortest_paths": None
                }
            },
            "path_analyses": [
                {
                    ## -- Save volau for long distance heavy trucks -- ##
					"link_component": "@mvlen",
                    "turn_component": None,
                    "operator": "+",
                    "selection_threshold": {
                        "lower": 200.001,
                        "upper": 999999
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@h200",
                        "selected_turn_volumes": None,
                        "od_values": None
                    }
                },
                {
                    ## -- Save select link demand -- ##
					"link_component": "@sellk",
                    "turn_component": None,
                    "operator": ".max.",
                    "selection_threshold": {
                        "lower": 1,
                        "upper": 1
                    },
                    "path_to_od_composition": {
                        "considered_paths": "SELECTED",
                        "multiply_path_proportions_by": {
                            "analyzed_demand": True,
                            "path_value": False
                        }
                    },
                    "analyzed_demand": None,
                    "results": {
                        "selected_link_volumes": "@slcl7",
                        "selected_turn_volumes": None,
                        "od_values": "mf107"
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
        "max_iterations": 400,
        "relative_gap": 0.0001,
        "best_relative_gap": 0.01,
        "normalized_gap": 0.005
    }
}

## -- Perform the traffic assignment -- ##
with _m.logbook_trace("SOLA Traffic Assignment for Time Period %s" % tmPeriod):
    #- Create variables for MOVES inputs -#
    create_extra(extra_attribute_type="LINK", extra_attribute_name="@m200", extra_attribute_description="medium truck long distance volau (MOVES)", overwrite=True)
    create_extra(extra_attribute_type="LINK", extra_attribute_name="@h200", extra_attribute_description="heavy truck long distance volau (MOVES)", overwrite=True)
    create_extra(extra_attribute_type="LINK", extra_attribute_name="@mvlen", extra_attribute_description="length - truck long distance (MOVES)", overwrite=True)
    report=netcalc(calcSpec2, full_report=False)
    report=netcalc(calcSpec2b, full_report=False)
    ## -- Create and calculate select link variable -- ##
    new_att2 = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@sellk",
                           extra_attribute_description="select link flag for specified links",
                           overwrite=True)
    report=netcalc(calcSpecLink, full_report=False)
    ## -- Create select link TOD variables -- ##
    for i in range(1,8):
        slv1 = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@slcl%s" %(i),
                           extra_attribute_description="select link class %s TOD volumes (VEQ)" %(i),
                           overwrite=True)

    slvol = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@slvol",
                           extra_attribute_description="select link period %s total volumes (VEH)" %(tmPeriod),
                           overwrite=True)

    ## -- Run traffic assignment -- ##
    if tmPeriod == 3:
        new_att = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@tollflag",
                           extra_attribute_description="select link flag to indicate a toll",
                           overwrite=True)
        report=netcalc(calcSpec, full_report=False)
        report = assign_SOLA(solaSpecAMPK)
    elif tmPeriod == 5:
        new_att = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@tollflag",
                           extra_attribute_description="select link flag to indicate a toll",
                           overwrite=True)
        report=netcalc(calcSpec, full_report=False)
        report = assign_SOLA(solaSpecMidday)    
    else:
        report = assign_SOLA(solaSpec)

    ## -- Sum TOD select link class volumes (VEQs) into total link volume (vehicles) -- ##
    report=netcalc(calcSpecSlvol, full_report=False)
    

    ## -- Sum TOD select link class demand (VEQs) into total daily demand (vehicles) -- ##
    specMf61 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf61",
			"expression": "mf61 + mf101",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}	
    specMf62 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf62",
			"expression": "mf62 + mf102",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}
    specMf63 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf63",
			"expression": "mf63 + mf103",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}		
    specMf64 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf64",
			"expression": "mf64 + mf104",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}	
    specMf65 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf65",
			"expression": "mf65 + mf105",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}	
    specMf66 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf66",
			"expression": "mf66 + (mf106/2)",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}			
    specMf67 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf67",
			"expression": "mf67 + (mf107/3)",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}
    specMf60 = {
			"type": "MATRIX_CALCULATION",
			"result": "mf60",
			"expression": "mf60 + mf101 + mf102 + mf103 + mf104 + mf105 + (mf106/2) + (mf107/3)",
			"constraint": {
				"by_zone": None,
				"by_value": None
				}
			}
    report = compute_matrix([specMf61,specMf62,specMf63,specMf64,specMf65,specMf66,specMf67,specMf60])    
    if tmPeriod == 3:
        specMf68 = {
			    "type": "MATRIX_CALCULATION",
			    "result": "mf68",
			    "expression": "mf68 + mf101 + mf102 + mf103 + mf104",
			    "constraint": {
				    "by_zone": None,
				    "by_value": None
				    }
			    }
        specMf69 = {
			    "type": "MATRIX_CALCULATION",
			    "result": "mf69",
			    "expression": "mf69 + mf105 + (mf106/2) + (mf107/3)",
			    "constraint": {
				    "by_zone": None,
				    "by_value": None
				    }
			    }
        report = compute_matrix([specMf68,specMf69]) 


    if tmPeriod == 8:
        ## -- Delete temporary matrices -- ##
        a = list(range(101, 108))
        matrices = []
        for item in a:
            matrices.append("mf"+str(item))
        for m in matrices:
            delete_matrix(matrix=my_emmebank.matrix(m))



print("         Final Global Iteration SOLA Assignment with Select Link Analysis Completed for Time Period {0}".format(tmPeriod))    
