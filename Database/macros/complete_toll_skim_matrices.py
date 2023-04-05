## complete_toll_skim_matrices.py
##
## Complete toll skim calculations for time period 3 - the resulting matrices are used
## in the utility calculations of the Destination Choice-Mode Choice model. Weighted
## average toll values are calculated based on the actual trips made by user classes. 
## Final output:
##  - mf111: Toll - SOV low income (HBW)
##  - mf114: Toll - SOV high income (HBW)
##  - mf112: Toll - HOV low income (HBW)
##  - mf115: Toll - HOV high income (HBW)
##  - mf117: Toll - (non-work purposes)
##
## Heither rev. 02-20-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
tod = int(sys.argv[2])
directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")

delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
my_emmebank = my_modeller.emmebank

if tod == 3:
    ## -- Accumulate SOV trips, compute average SOV toll, copy HOV2 toll for HOV3+  -- ##
    specMf131 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf131",
        "expression": "mf134 + mf135 + mf136",
        "constraint": {
		    "by_zone": None,
		    "by_value": None
		    }
	    }	

    specMs23 = {
        "type": "MATRIX_CALCULATION",
	    "expression": "mf134",
        "result": "ms23",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    specMs24 = {
        "type": "MATRIX_CALCULATION",
	    "expression": "mf135",
        "result": "ms24",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    specMs25 = {
        "type": "MATRIX_CALCULATION",
	    "expression": "mf136",
        "result": "ms25",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    ## -- Convert tolls from dollars to cents -- ##
    specMf111 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf111",
        "expression": "(mf123*ms23+mf124*ms24+mf125*ms25)/(ms23+ms24+ms25) * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }
	
    specMf114 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf114",
        "expression": "(mf123*ms23+mf124*ms24+mf125*ms25)/(ms23+ms24+ms25) * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf112 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf112",
        "expression": "mf112 * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf113 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf113",
        "expression": "mf112",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf115 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf115",
        "expression": "mf112",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf116 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf116",
        "expression": "mf112",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }
    
    report = compute_matrix([specMf131,specMs23,specMs24,specMs25,specMf111,specMf114,specMf112,specMf113,specMf115,specMf116])
    ## -- Delete Scalar Matrices -- ##
    matrix_ms23 = my_emmebank.matrix("ms23")
    matrix_ms24 = my_emmebank.matrix("ms24")
    matrix_ms25 = my_emmebank.matrix("ms25")
    delete_matrix(matrix=matrix_ms23) 	
    delete_matrix(matrix=matrix_ms24) 	
    delete_matrix(matrix=matrix_ms25)

else:
    ## -- Accumulate SOV trips, compute average SOV toll, copy HOV2 toll for HOV3+  -- ##
    specMf137 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf137",
        "expression": "mf140 + mf141 + mf142",
        "constraint": {
		    "by_zone": None,
		    "by_value": None
		    }
	    }

    specMs26 = {
        "type": "MATRIX_CALCULATION",
        "expression": "mf140",
        "result": "ms26",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    specMs27 = {
        "type": "MATRIX_CALCULATION",
        "expression": "mf141",
        "result": "ms27",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    specMs28 = {
        "type": "MATRIX_CALCULATION",
        "expression": "mf142",
        "result": "ms28",
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": "+",
            "destinations": "+"
        }
    }

    ## -- Convert tolls from dollars to cents -- ##
    specMf117 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf117",
        "expression": "(mf126*ms26+mf127*ms27+mf128*ms28)/(ms26+ms27+ms28) * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }
	
    specMf120 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf120",
        "expression": "(mf126*ms26+mf127*ms27+mf128*ms28)/(ms26+ms27+ms28) * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf118 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf118",
        "expression": "mf118 * 100",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf119 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf119",
        "expression": "mf118",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf121 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf121",
        "expression": "mf118",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    specMf122 = {
        "type": "MATRIX_CALCULATION",
        "result": "mf122",
        "expression": "mf118",
        "constraint": {
            "by_zone": None,
            "by_value": None
            }
        }

    report = compute_matrix([specMf137,specMs26,specMs27,specMs28,specMf117,specMf120,specMf118,specMf119,specMf121,specMf122])
    ## -- Delete Scalar Matrices -- ##
    matrix_ms26 = my_emmebank.matrix("ms26")
    matrix_ms27 = my_emmebank.matrix("ms27")
    matrix_ms28 = my_emmebank.matrix("ms28")
    delete_matrix(matrix=matrix_ms26) 	
    delete_matrix(matrix=matrix_ms27) 	
    delete_matrix(matrix=matrix_ms28) 

	 
print("         Toll matrices calculated")  
