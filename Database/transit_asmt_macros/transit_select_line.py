## transit_select_line.py
## 
## Complete a transit select line analysis.
##
## Heither rev. 08-21-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
currentScen = int(sys.argv[2])
lineFile = sys.argv[3]

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=True, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
strategy = my_modeller.tool("inro.emme.transit_assignment.extended.strategy_based_analysis") 
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
my_emmebank = my_modeller.emmebank


## -- Calculate the transit select line attribute value -- ##
calcSpecLine = {"type": "NETWORK_CALCULATION",
	"result": "@sline",
    "expression": "1",
	"aggregation": None,
    "selections": {
        "transit_line": "~<Select_Line\%s" % (lineFile)
        }
    }

scens = (currentScen+21, currentScen+23, currentScen+25, currentScen+27)
## -- mf21: storage for all select line trips 
## -- mf22: storage for all select line HBW trips 
## -- mf23: temp storage for time period VOT class select line trips 
## -- mf24: temp storage for time period VOT class select line HBW trips 
new_mat = matrix_init(matrix_id="mf21", matrix_name="sel_line_all_demand", matrix_description="select line ALL demand", overwrite=True, default_value=0)
new_mat = matrix_init(matrix_id="mf22", matrix_name="sel_line_hbw_demand", matrix_description="select line HBW demand", overwrite=True, default_value=0)

mtx1 = ("mf501", "mf502", "mf503", "mf513", "mf514", "mf515")     ##-- overnight transit demand matrices, NT HBW transit demand
mtx2 = ("mf504", "mf505", "mf506", "mf516", "mf517", "mf518")     ##-- AM peak transit demand matrices, AM HBW transit demand
mtx3 = ("mf507", "mf508", "mf509", "mf519", "mf520", "mf521")     ##-- midday transit demand matrices, MD HBW transit demand
mtx4 = ("mf510", "mf511", "mf512", "mf522", "mf523", "mf524")     ##-- PM peak transit demand matrices, PM HBW transit demand
vot = ("VOT1", "VOT2", "VOT3")

x = 0
matrices = mtx1
## create sel link demand storage matrices - need all select line trips to determine EDA users
##                need HBW trips for commute analysis
## store sel link boardings - need all 
## loop through scenarios
for s in scens:
    ## -- Set storage matrix group -- ##
    if x ==1:
        matrices = mtx2
    elif x == 2:
        matrices = mtx3
    elif x == 3:
        matrices = mtx4

    with _m.logbook_trace("Transit Select Line Analysis - Scenario %s" %(s)):
        ## -- Set primary scenario -- ##
        scen = int(scens[x])
        change_scenario(scenario=scen)

        ## -- create select line variable and set value -- ##
        new_att = create_extra(extra_attribute_type="TRANSIT_LINE",
                               extra_attribute_name="@sline",
                               extra_attribute_description="transit select line flag",
                               overwrite=True)
        report=netcalc(calcSpecLine, full_report=False)
       
        for i in range(0,3):
            ## -- create and reuse matrices to hold select line demand - ##
            new_mat = matrix_init(matrix_id="mf23",
                                matrix_name="sel_line_demand_%s_%s" % (s, vot[i]),
                                matrix_description="select line demand - Scenario %s - %s" % (s, vot[i]),
                                overwrite=True,
                                default_value=0)
            new_mat = matrix_init(matrix_id="mf24",
                                matrix_name="sel_line_hw_dmd_%s_%s" % (s, vot[i]),
                                matrix_description="select line hw demand - Scenario %s - %s" % (s, vot[i]),
                                overwrite=True,
                                default_value=0)

            ## -- Identify matrix with user class demand -- ##
            class_mtx = matrices[i]
            user_dmd = my_emmebank.matrix(class_mtx)
            classLabel = "%s" %(user_dmd.name)
            spec_vot = {
                        "type": "EXTENDED_TRANSIT_STRATEGY_ANALYSIS",
                        "trip_components": {
                            "boarding": "@sline"
                        },
                        "sub_path_combination_operator": ".max.",
                        "sub_strategy_combination_operator": ".max.",
                        "selected_demand_and_transit_volumes": {
                            "sub_strategies_to_retain": "FROM_COMBINATION_OPERATOR",
                            "selection_threshold": {
                                "lower": 1,
                                "upper": 1
                            }
                        },
                        ## -- multiclass assignment so need to specify demand for each class -- ##
                        "analyzed_demand": "%s" %(matrices[i]),
                        "results": {
                            "selected_demand": "mf23"
                        },
                }
            spec_vot_hbw = {
                        "type": "EXTENDED_TRANSIT_STRATEGY_ANALYSIS",
                        "trip_components": {
                            "boarding": "@sline"
                        },
                        "sub_path_combination_operator": ".max.",
                        "sub_strategy_combination_operator": ".max.",
                        "selected_demand_and_transit_volumes": {
                            "sub_strategies_to_retain": "FROM_COMBINATION_OPERATOR",
                            "selection_threshold": {
                                "lower": 1,
                                "upper": 1
                            }
                        },
                        ## -- multiclass assignment so need to specify HBW demand for each class -- ##
                        "analyzed_demand": "%s" %(matrices[i+3]),
                        "results": {
                            "selected_demand": "mf24"
                        },
                }
            report = strategy(spec_vot, class_name=classLabel)      ## -- all demand
            report = strategy(spec_vot_hbw, class_name=classLabel)  ## -- HBW demand

            ## -- Accumulate time period and VOT class demand into daily total -- ##
            specMf21 = {"type": "MATRIX_CALCULATION", "result": "mf21", "expression": "mf21 + mf23", "constraint": {"by_zone": None,"by_value": None}}
            specMf22 = {"type": "MATRIX_CALCULATION", "result": "mf22", "expression": "mf22 + mf24", "constraint": {"by_zone": None,"by_value": None}}	
            report = compute_matrix([specMf21, specMf22])

        x += 1 

del_mtx = ("mf23", "mf24")
for m in del_mtx:
    delete_matrix(matrix=my_emmebank.matrix(m))
print("         Select Line Analysis Completed")  