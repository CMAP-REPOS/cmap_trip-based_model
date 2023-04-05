## SOLA_assignment_final_global_iteration.py
##
## Peforms the time-of-day SOLA traffic assignments: one specification is used for Period 3
## (HW trips), one for period 5 (non-work trips) and a third specification is used for all other
## time periods
##
## This differs from SOLA_assignment.py because during the final global iteration the
## time-of-day assignments also capture the link volumes for medium and heavy truck trips
## of 200+ miles, used as an input to MOVES.
##
## This incorporates part of the Emme macro toll_skim.mac to save AM Peak & Midday toll skim data
## (which is used by complete_toll_skim_matrices.py) and part of the Emme macro punch.moves.data.mac,
## which ran the long distance truck trip path analysis.
##
## Heither rev. 03-13-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
tmPeriod = int(sys.argv[2])
sThreads = int(sys.argv[3])

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=True, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")

create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")

netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")

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
    "expression": "length * (i < 3633 && j < 3633) + 50 * (i > 3632 || j > 3632)",
	"aggregation": None,
    "selections": {
        "link": "all"}}

my_emmebank = my_modeller.emmebank


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
            "path_analyses": []
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
            "path_analyses": []
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
            "path_analyses": []
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

print("         Final Global Iteration SOLA Assignment Completed for Time Period {0}".format(tmPeriod))    