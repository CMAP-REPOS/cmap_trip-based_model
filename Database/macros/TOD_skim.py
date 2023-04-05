## TOD_skim.py
##
## Create travel time skims for each time period, which are used in the utility calculations in
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
##    ms97 = dummy operand for demand matrix in assignment setup
##    extra function parameter (el1) used in averaging vdfs
##
## This is the SOLA Python implementation of the Emme macro skimTOD5I_7c.mac.
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
sThreads = int(sys.argv[4])

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")
my_emmebank = my_modeller.emmebank

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
delete_scenario = my_modeller.tool("inro.emme.data.scenario.delete_scenario")
s90 = my_emmebank.scenario(90)

mtx = ["mf461","mf462","mf463","mf464","mf465","mf466","mf467","mf468"] ##-- storage matrices 
mtxIndex = tod-1

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
            "path_analyses": []
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

with _m.logbook_trace("Travel Time Skims for Time-of-Day Model: Scenario %s" % currentScen):
    report = assign_SOLA(skimSpec)

    ## -- Reset primary scenario -- ##
    change_scenario(scenario=currentScen)

    ## -- Delete temporary skim scenario (cannot be current scenario) -- ##
    delete_scenario(scenario=s90)
print("         Travel time skim Completed for Time Period {0}".format(tod))  
