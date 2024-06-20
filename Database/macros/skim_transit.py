'''
 skim_transit.py

 Performs transit network skimming to supply transit costs to the destination choice-mode choice model:
    -   transit fares
    -   transit headways
    -   transit in-vehicle time
    -   transfer walk time 
    -   first mode
    -   priority mode
    -   last mode
    -   Note: transit access and egress are simulated in the destination choice-mode choice model and are not provided through the transit skims.

 Transit skimming has been upgraded to use the extended transit assignment with journey levels to more accurately capture transfer costs. Script 
 also performs calculations to identify first, priority and last modes.    


 Arguments: 1= name of Emme project file
            2= 3-digit scenario number
            3= model Global Iteration number 0-2
            4= time period indicator: AM or MD 

 Craig Heither, 03-25-2024
 ==========================================================================================
'''

import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
currentScen = int(sys.argv[2])
globalIter = int(sys.argv[3])
timePeriod = sys.argv[4]
donorScen = currentScen*100 + (globalIter-1)*10 + 3                 ## -- used for Global Iterations 1 & 2

## -- Trasit base fares in cents (updated Oct. 2020) -- ## 
ctaBusFare= 225
ctaRailFare=250
metraFare=400
paceFare=200
pacePremiumFare=450
pacePremiumTransfer=250
regularTransfer=25
premiumTransfer=30
perceptionVOT = 1 / 27	   ## -- medium VOT*perception factor [$7.20/hr*2.25*100/60=27.00 cents/min], used to convert cost to generalized minutes i.e., minutes/cent


directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)
my_emmebank = my_modeller.emmebank

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
copy_att = my_modeller.tool("inro.emme.data.network.copy_attribute")
del_function = my_modeller.tool("inro.emme.data.function.delete_function")
create_function = my_modeller.tool("inro.emme.data.function.create_function")
extended_tranAsmt = my_modeller.tool("inro.emme.transit_assignment.extended_transit_assignment")
mtxResults = my_modeller.tool("inro.emme.transit_assignment.extended.matrix_results")
transit_strategy = my_modeller.tool("inro.emme.transit_assignment.extended.strategy_based_analysis")
tranAsmt_stnd = my_modeller.tool("inro.emme.transit_assignment.standard_transit_assignment")
init_partition = my_modeller.tool("inro.emme.data.zone_partition.init_partition")
change_partition = my_modeller.tool("inro.emme.data.zone_partition.change_partition_description")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
###########################################################################################################################

## -- Set primary scenario -- ##
if timePeriod == 'MD':
    currentScen = currentScen + 5
change_scenario(scenario=currentScen)


## -- Initialize matrices -- ##
mtx = ("mf803","mf804","mf805","mf806","mf807","mf808","mf809","mf810","mf811","mf812","mf813","mf815","mf816","mf817","mf818")
if timePeriod == 'MD':
    mtx = ("mf903","mf904","mf905","mf906","mf907","mf908","mf909","mf910","mf911","mf912","mf913","mf915","mf916","mf917","mf918")

new_mf1 = matrix_init(matrix_id="%s" %(mtx[0]), matrix_name="fmode_%s" %(timePeriod),
                        matrix_description="skimmed first mode - %s" %(timePeriod), overwrite=True, default_value=3) ##-- 3=value for bus
new_mf2 = matrix_init(matrix_id="%s" %(mtx[1]), matrix_name="pmode_%s" %(timePeriod),
                        matrix_description="skimmed priority mode - %s" %(timePeriod), overwrite=True, default_value=3) ##-- 3=value for bus
new_mf3 = matrix_init(matrix_id="%s" %(mtx[2]), matrix_name="lmode_%s" %(timePeriod),
                        matrix_description="skimmed last mode - %s" %(timePeriod), overwrite=True, default_value=3) ##-- 3=value for bus
new_mf4 = matrix_init(matrix_id="%s" %(mtx[3]), matrix_name="one",
                        matrix_description="dummy for assignment (ones) - %s" %(timePeriod), overwrite=True, default_value=1) 
new_mf5 = matrix_init(matrix_id="%s" %(mtx[4]), matrix_name="skimgc_%s" %(timePeriod),
                        matrix_description="skimmed generalized cost - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf6 = matrix_init(matrix_id="%s" %(mtx[5]), matrix_name="skimiveh_%s" %(timePeriod),
                        matrix_description="skimmed in-vehicle minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf7 = matrix_init(matrix_id="%s" %(mtx[6]), matrix_name="skimxfer_%s" %(timePeriod),
                        matrix_description="skimmed transfer link minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf8 = matrix_init(matrix_id="%s" %(mtx[7]), matrix_name="skmtwait_%s" %(timePeriod),
                        matrix_description="skimmed total wait minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf9 = matrix_init(matrix_id="%s" %(mtx[8]), matrix_name="skmfwait_%s" %(timePeriod),
                        matrix_description="skimmed first wait minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf10 = matrix_init(matrix_id="%s" %(mtx[9]), matrix_name="skimfare_%s" %(timePeriod),
                        matrix_description="skimmed initial fares - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf11 = matrix_init(matrix_id="%s" %(mtx[10]), matrix_name="skimmi_%s" %(timePeriod),
                        matrix_description="skimmed distance - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf12 = matrix_init(matrix_id="%s" %(mtx[11]), matrix_name="zonefare_%s" %(timePeriod),
                        matrix_description="Metra zonal fares - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf13 = matrix_init(matrix_id="%s" %(mtx[12]), matrix_name="xferdisc_%s" %(timePeriod),
                        matrix_description="transfer link fare discount - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf14 = matrix_init(matrix_id="%s" %(mtx[13]), matrix_name="xferbrd_%s" %(timePeriod),
                        matrix_description="transfer node boarding tally - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf15 = matrix_init(matrix_id="%s" %(mtx[14]), matrix_name="skimfare_%s" %(timePeriod),
                        matrix_description="skimmed final average fare - %s" %(timePeriod), overwrite=True, default_value=0)


## -- Fill @hwytm with congested highway time from auto scenario -- ##
## -- Use ul2 temporarily to store congested highway time from auto scenario -- ##
## -- Global Iteration 0: @hwytm has already been filled with pre-loaded congested times. -- ##
if globalIter > 0:
    if timePeriod == 'MD':
        donorScen = donorScen + 2
    print("currentScen={0}, timePeriod={1}, donorScen={2}".format(currentScen,timePeriod,donorScen))

    ## -- Initialize ul2 [link variable] and @hwytm [segment variable] -- ##
    calcSpec = {
                "type": "NETWORK_CALCULATION",
                "result": "ul2",
                "expression": "0",
                "aggregation": None,
                "selections": {"link": "all"}
                }
    calcSpec2 = {
                "type": "NETWORK_CALCULATION",
                "result": "@hwytm",
                "expression": "0",
                "aggregation": None,
                "selections": {"link": "all", "transit_line": "all"}
                }
    report=netcalc([calcSpec, calcSpec2], full_report=False)
    ## -- Import timau from donor scenario -- ##
    from_att = "timau"
    to_att = "ul2"
    from_scen = my_emmebank.scenario(donorScen)
    copy_att(from_scenario=from_scen,
             from_attribute_name=from_att,
             to_attribute_name=to_att,
             selection={"link":"all"}
            )
    ## -- Finalize calculations -- ##
    calcSpec3 = {
                "type": "NETWORK_CALCULATION",
                "result": "@hwytm",
                "expression": "ul2",
                "aggregation": None,
                "selections": {"link": "all", "transit_line": "all"}
                }
    report=netcalc([calcSpec3, calcSpec], full_report=False)


## -- Initialize transit time functions (ft1=normal  and ft2=brt) -- ##
del_function(my_emmebank.function("ft1"))
del_function(my_emmebank.function("ft2"))
ttf1 = create_function("ft1", "us1")
ttf2 = create_function("ft2", "us1")


## -- Calculate network costs -- ##
### -- Set line haul times and wait times --- ###
calcSpec4 = {
            "result": "us1",
            "expression": "0",
            "selections": {"link": "all", "transit_line": "all"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec5 = {
            "result": "us1",
            "expression": "(@ltime*(ttf.eq.2))+(@ltime.max.@hwytm)*(ttf.eq.1)", #ttf1=normal, ttf2=BRT
            "selections": {"link": "all", "transit_line": "all"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec6 = {
            "result": "us3",
            "expression": "0",
            "selections": {"link": "all", "transit_line": "all"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec7 = {
            "result": "us3",
            "expression": "hdw",
            "selections": {"link": "all", "transit_line": "all"},
            "type": "NETWORK_CALCULATION"
            }
report=netcalc([calcSpec4, calcSpec5, calcSpec6, calcSpec7], full_report=True)
### -- Set base fares and transfer fares --- ###
create_extra('TRANSIT_LINE', '@xfer_fare', 'Transfer Fare', overwrite=True)
create_extra('TRANSIT_LINE', '@xfer_fare_q', 'Transfer Fare from Pace Express', overwrite=True)
calcSpec8 = {
            "result": "ut1",
            "expression": "0",
            "selections": {"transit_line": "all"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec9 = {
            "result": "ut1",
            "expression": "%s" %(ctaBusFare),
            "selections": {"transit_line": "mod=BE"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec10 = {
            "result": "ut1",
            "expression": "%s" %(ctaRailFare),
            "selections": {"transit_line": "mod=C"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec11 = {
            "result": "ut1",
            "expression": "%s" %(paceFare),
            "selections": {"transit_line": "mod=LP"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec12 = {
            "result": "ut1",
            "expression": "%s" %(pacePremiumFare),
            "selections": {"transit_line": "mod=Q"},
            "type": "NETWORK_CALCULATION"
            }
calcSpec13 = {
            "result": "ut1",
            "expression": "%s" %(metraFare),
            "selections": {"transit_line": "mod=M"},
            "type": "NETWORK_CALCULATION"
            }
report=netcalc([calcSpec8, calcSpec9, calcSpec10, calcSpec11, calcSpec12, calcSpec13], full_report=False)
## -- Transfer fares -- ##
Metra_fare = {
            "result": "@xfer_fare",
            "expression": "ut1",
            "selections": {"transit_line": "mode=M"},
            "type": "NETWORK_CALCULATION"
        }
Pace_express_fare = {
            "result": "@xfer_fare",
            "expression": "%s" %(pacePremiumTransfer),
            "selections": {"transit_line": "mode=Q"},
            "type": "NETWORK_CALCULATION"
        }        
Other_fare = {
            "result": "@xfer_fare",
            "expression": "%s" %(regularTransfer),
            "selections": {"transit_line": "mode=BECPL"},
            "type": "NETWORK_CALCULATION"
        }        
netcalc([Metra_fare, Pace_express_fare, Other_fare])
Metra_fare = {
            "result": "@xfer_fare_q",
            "expression": "ut1",
            "selections": {"transit_line": "mode=M"},
            "type": "NETWORK_CALCULATION"
        }
Other_fare = {
            "result": "@xfer_fare_q",
            "expression": "%s" %(premiumTransfer),
            "selections": {"transit_line": "mode=BECPLQ"},
            "type": "NETWORK_CALCULATION"
        }        
netcalc([Metra_fare, Other_fare])


## -- Run extended transit assignment and obtain results -- ##
### --- This is to get transit costs between all zones so we will prohibit connector-to-connector paths --- ###
### --- Determining if walking/cycling is a better option than transit will be resolved in the mode choice model --- ###
exasmtSpec = {
            "type": "EXTENDED_TRANSIT_ASSIGNMENT",
            "modes": ["B","C","E","L","M","P","Q","b","c","d","k","m","r","t","u","v","w","x","y","z"],
            "demand": "%s" %(mtx[3]),
            "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2},
            "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
            "boarding_cost": {"on_lines": {"penalty": "ut1", "perception_factor": perceptionVOT}},
            "in_vehicle_time": {"perception_factor": 1},
            "in_vehicle_cost":{"penalty": "@zfare","perception_factor": perceptionVOT},
            "aux_transit_time": {"perception_factor": 2},
            "aux_transit_cost": None,
            "flow_distribution_at_origins": {"choices_at_origins": {"choice_points": "ALL_ORIGINS", "choice_set": "ALL_CONNECTORS",
                                                                    "logit_parameters": {"scale": 0.2,"truncation": 0.05}},
                                             "fixed_proportions_on_connectors": None},   ##-- OK
            "flow_distribution_at_regular_nodes_with_aux_transit_choices": {"choices_at_regular_nodes": "OPTIMAL_STRATEGY"},   ##-- OK
            "flow_distribution_between_lines": {"consider_total_impedance": True},   ##-- OK
            "connector_to_connector_path_prohibition": {"at_nodes": "ALL", "reassign_demand_to_alternate_path": True},   ##-- OK
            "circular_lines": {"stay": False},
            "od_results": {"total_impedance": "%s" %(mtx[4])},
            "results": {
                "aux_transit_volumes_by_mode": [
                    {"mode": "b", "volume": None},
                    {"mode": "c", "volume": None},
                    {"mode": "d", "volume": None},
                    {"mode": "k", "volume": None},
                    {"mode": "m", "volume": None},
                    {"mode": "r", "volume": None},
                    {"mode": "t", "volume": None},
                    {"mode": "u", "volume": None},
                    {"mode": "v", "volume": None},
                    {"mode": "w", "volume": None},
                    {"mode": "x", "volume": None},
                    {"mode": "y", "volume": None},
                    {"mode": "z", "volume": None},
                ]
            },
            "journey_levels": [
                {  # Never Boarded 0
                "description": "Never Boarded",
                "destinations_reachable": True,
                "transition_rules": [
                    {"mode": "B", "next_journey_level": 3},
                    {"mode": "C", "next_journey_level": 2},
                    {"mode": "E", "next_journey_level": 3},
                    {"mode": "L", "next_journey_level": 4},
                    {"mode": "M", "next_journey_level": 1},
                    {"mode": "P", "next_journey_level": 4},
                    {"mode": "Q", "next_journey_level": 6}
                ],
                "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                "boarding_cost": {"on_lines": {"penalty": "ut1", "perception_factor": perceptionVOT}},
                "waiting_time": {"headway_fraction": 0.5, "effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # Metra Only 1
                    "description": "Metra Only",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 3},
                        {"mode": "C", "next_journey_level": 2},
                        {"mode": "E", "next_journey_level": 3},
                        {"mode": "L", "next_journey_level": 4},
                        {"mode": "M", "next_journey_level": 1},
                        {"mode": "P", "next_journey_level": 4},
                        {"mode": "Q", "next_journey_level": 6}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "ut1", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5, "effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # CTA Rail 2
                    "description": "CTA, no Pace",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 3},
                        {"mode": "C", "next_journey_level": 2},
                        {"mode": "E", "next_journey_level": 3},
                        {"mode": "L", "next_journey_level": 5},
                        {"mode": "M", "next_journey_level": 2},
                        {"mode": "P", "next_journey_level": 5},
                        {"mode": "Q", "next_journey_level": 7}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5, "effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # CTA, No Pace 3
                    "description": "CTA, no Pace",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 3},
                        {"mode": "C", "next_journey_level": 3},
                        {"mode": "E", "next_journey_level": 3},
                        {"mode": "L", "next_journey_level": 5},
                        {"mode": "M", "next_journey_level": 3},
                        {"mode": "P", "next_journey_level": 5},
                        {"mode": "Q", "next_journey_level": 7}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # Pace No CTA 4
                    "description": "Pace, no CTA",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 5},
                        {"mode": "C", "next_journey_level": 5},
                        {"mode": "E", "next_journey_level": 5},
                        {"mode": "L", "next_journey_level": 4},
                        {"mode": "M", "next_journey_level": 4},
                        {"mode": "P", "next_journey_level": 4},
                        {"mode": "Q", "next_journey_level": 6}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # CTA and Pace 5
                    "description": "Boarded CTA and Pace",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 5},
                        {"mode": "C", "next_journey_level": 5},
                        {"mode": "E", "next_journey_level": 5},
                        {"mode": "L", "next_journey_level": 5},
                        {"mode": "M", "next_journey_level": 5},
                        {"mode": "P", "next_journey_level": 5},
                        {"mode": "Q", "next_journey_level": 7}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # Pace Express No CTA 6
                    "description": "Boarded Pace Express, no CTA",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 7},
                        {"mode": "C", "next_journey_level": 7},
                        {"mode": "E", "next_journey_level": 7},
                        {"mode": "L", "next_journey_level": 7},
                        {"mode": "M", "next_journey_level": 6},
                        {"mode": "P", "next_journey_level": 7},
                        {"mode": "Q", "next_journey_level": 6}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare_q", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                },
                {  # CTA and Pace Express 7
                    "description": "Boarded CTA and Pace Express",
                    "destinations_reachable": True,
                    "transition_rules": [
                        {"mode": "B", "next_journey_level": 7},
                        {"mode": "C", "next_journey_level": 7},
                        {"mode": "E", "next_journey_level": 7},
                        {"mode": "L", "next_journey_level": 7},
                        {"mode": "M", "next_journey_level": 7},
                        {"mode": "P", "next_journey_level": 7},
                        {"mode": "Q", "next_journey_level": 7}
                    ],
                    "boarding_time": {"global": {"penalty": 0, "perception_factor": 1},},
                    "boarding_cost": {"on_lines": {"penalty": "@xfer_fare_q", "perception_factor": perceptionVOT}},
                    "waiting_time": {"headway_fraction": 0.5,"effective_headways": "us3", "spread_factor": 1, "perception_factor": 2}
                }
            ],
            "performance_settings": {"number_of_processors": "max-1"},
            "save_strategies": True,
            }
mtxSpec = { 
            "type": "EXTENDED_TRANSIT_MATRIX_RESULTS",
            "by_mode_subset": {
                "modes": ["B","C","E","L","M","P","Q","b","c","d","k","m","r","t"],
                "distance": "%s" %(mtx[10]),
                "actual_in_vehicle_times": "%s" %(mtx[5]),
                "actual_aux_transit_times": "%s" %(mtx[6]),
                "actual_total_boarding_costs": "%s" %(mtx[9]),
                "actual_in_vehicle_costs": "%s" %(mtx[11])
            },
            "actual_first_waiting_times": "%s" %(mtx[8]),
            "actual_total_waiting_times": "%s" %(mtx[7]),
        }
mtx2Spec = { 
            "type": "EXTENDED_TRANSIT_MATRIX_RESULTS",
            "by_mode_subset": {
                "modes": ["B","E","L","P","Q"],
                "avg_boardings": "%s" %(mtx[13])
            },
        }
strat2Spec = {
            "type": "EXTENDED_TRANSIT_STRATEGY_ANALYSIS",
            "trip_components": {"aux_transit": "ul1",},
            "sub_path_combination_operator": ".min.",
            "sub_strategy_combination_operator": "average",
            "selected_demand_and_transit_volumes": {
                "sub_strategies_to_retain": "ALL",
                "selection_threshold": {"lower": -999999, "upper": 0}
            },
            "analyzed_demand": None,
            "constraint": None,
            "results": {"strategy_values": "%s" %(mtx[12])}
        }    
report = extended_tranAsmt(exasmtSpec)
report = mtxResults(mtxSpec)
report = mtxResults(mtx2Spec)
report = transit_strategy(strat2Spec)


## -- Calculate final average fares (boarding plus Metra zone fare) -- ##
fareSpec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[14]), "expression": "%s+%s" %(mtx[11],mtx[9]),
        "constraint": {"by_zone": None, "by_value": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(fareSpec) 

##==============================================================
## SECTION 2. GUESS WHICH MODE
##==============================================================

## -- Initialize transit access and egress partitions -- ##
init_partition(partition=my_emmebank.partition("gb")) 
change_partition(partition=my_emmebank.partition("gb"), partition_description="transit access modes")
init_partition(partition=my_emmebank.partition("gc")) 
change_partition(partition=my_emmebank.partition("gc"), partition_description="transit egress modes")

## -- Assign transit modes to zone groups based auxiliary mode (start by initializing variables) -- ##
### --- zone group key:  first position=mode, second position=access/egress --- ###
### ---     mode values: metra=7, cta rail=6, bus=3 --- ###
### ---     gb70=metra access, gb60=cta rail access, gb30=bus access, gb0=other access --- ###
### ---     gc71=metra egress, gb61=cta rail egress, gb31=bus egress, gc0=other access --- ###
calcUl1 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "0", "aggregation": None, "selections": {"link": "all"}}
calcUi1 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
calc30 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "30", "aggregation": None, "selections": {"link": "mod=u"}}
calc31 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "31", "aggregation": None, "selections": {"link": "mod=x"}}
calc60 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "60", "aggregation": None, "selections": {"link": "mod=v"}}
calc61 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "61", "aggregation": None, "selections": {"link": "mod=y"}}
calc70 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "70", "aggregation": None, "selections": {"link": "mod=w"}}
calc71 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "71", "aggregation": None, "selections": {"link": "mod=z"}}
## -- Apply access link values to centroids -- ##
calcAcc = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "ul1", "aggregation": ".max.", "selections": {"link": "mod=uvw"}}
## -- Assign ui1 to access zone groups (the centroid is the zone number) -- ##
accessSpec = {
        "type": "MATRIX_CALCULATION", "result": "gb", "expression": "ui1",
        "constraint": {"by_zone": None, "by_value": None},
        "aggregation": {"origins": None, "destinations": None},
        }
## -- Calculate uj1 based on ul1 (the jnode is the centroid) -- ##
calcEgr = {"type": "NETWORK_CALCULATION", "result": "uj1", "expression": "ul1", "aggregation": ".max.", "selections": {"link": "mod=xyz"}}
## -- Assign uj1 to egress zone groups -- ##
egressSpec = {
        "type": "MATRIX_CALCULATION", "result": "gc", "expression": "ui1",
        "constraint": {"by_zone": None, "by_value": None},
        "aggregation": {"origins": None, "destinations": None},
        }

report=netcalc([calcUl1, calcUi1], full_report=False)
report=netcalc([calc30, calc31, calc60, calc61, calc70, calc71], full_report=True)
report=netcalc(calcAcc, full_report=False)
report = compute_matrix(accessSpec) 
report=netcalc([calcUi1, calcEgr], full_report=False)
report = compute_matrix(egressSpec) 

## -- Calculate first/priority/last mode matrices -- ##
### --- First mode --- ###
fMode1Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[0]), "expression": "6",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb60", "destinations": "gc0-gc71"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
fMode2Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[0]), "expression": "7",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb70", "destinations": "gc0-gc71"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix([fMode1Spec, fMode2Spec]) 
### --- Priority mode --- ###
pMode1Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[1]), "expression": "6",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb30-gb60", "destinations": "gc61"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
pMode2Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[1]), "expression": "6",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb60", "destinations": "gc31-gc61"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
pMode3Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[1]), "expression": "7",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb30-gb70", "destinations": "gc71"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
pMode4Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[1]), "expression": "7",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb70", "destinations": "gc31-gc71"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix([pMode1Spec, pMode2Spec, pMode3Spec, pMode4Spec]) 
### --- Last mode --- ###
lMode1Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[2]), "expression": "6",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb0-gb70", "destinations": "gc61"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
lMode2Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[2]), "expression": "7",
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[5])},
                "by_zone": {"origins": "gb0-gb70", "destinations": "gc71"}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix([lMode1Spec, lMode2Spec]) 

print(' -- {0} transit skim done'.format(timePeriod))
