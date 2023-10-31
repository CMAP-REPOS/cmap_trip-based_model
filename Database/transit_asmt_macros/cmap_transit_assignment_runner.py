## Code borrowed from the CMAP ActivitySim model, with minor adjustments
##  Craig Heither, rev. 12-19-2022

import sys
import os
import inro.emme.desktop.app as _app
import inro.modeller as _m
import cmap_transit_assignment as cmap_transit_assignment
import datetime
import traceback

empFl = sys.argv[1]
msa_iteration = int(sys.argv[2])
scenValue = int(sys.argv[3])
directory = os.getcwd().replace('\\Database\\transit_asmt_macros','')
empFile = os.path.join(directory,empFl)
emmme_out = os.getcwd().replace('transit_asmt_macros','report')

print("Starting Transit Skim Process at %s"%(datetime.datetime.now()))
EMME_OUTPUT = emmme_out

desktop = _app.start_dedicated(project = empFile, visible = True, user_initials = "cmap") 
modeller = _m.Modeller(desktop)
my_emmebank = modeller.emmebank
databank = desktop.data_explorer().active_database().core_emmebank
copy_att = _m.Modeller().tool("inro.emme.data.network.copy_attribute")
netcalc = _m.Modeller().tool("inro.emme.network_calculation.network_calculator")

scens = [{"periodNum": 1, "scenNum": scenValue+21, "period": "NT"},
   {"periodNum": 3, "scenNum": scenValue+23, "period": "AM"},
   {"periodNum": 5, "scenNum": scenValue+25, "period": "MD"},
   {"periodNum": 7, "scenNum": scenValue+27, "period": "PM"}
]

data_explorer = desktop.data_explorer()
database = data_explorer.active_database()
matrix_count = 0
for s in scens:
    print("time period: " + s['period'] + " and new matrix count: " + str(matrix_count) +  "...")

    current_scenario = my_emmebank.scenario(s['scenNum'])
    data_explorer.replace_primary_scenario(database.scenario_by_number(s['scenNum']))

    # copy congested auto travel time from highway scenarios
    from_att = "timau"
    to_att = "ul2"
    from_scen = _m.Modeller().emmebank.scenario(s['periodNum'])
    if not from_scen.has_traffic_results:
        raise Exception("missing traffic assignment results for scenario %s" % (str(scen-200)))
    copy_att(from_attribute_name=from_att,
            to_attribute_name=to_att,
            from_scenario=from_scen)

    spec1 = {
        "result": "us1",
        "expression": "(us1*(ttf.eq.2))+(us1.max.ul2)*(ttf.eq.1)", #ttf1=normal, ttf2=BRT
        "selections": {
            "link": "all",
            "transit_line": "all"
        },
        "type": "NETWORK_CALCULATION"
    }
    spec2 = {
        "result": "ul2",
        "expression": "0",
        "selections": {
            "link": "all",
        },        
        "type": "NETWORK_CALCULATION"
    }    
    netcalc([spec1,spec2])

        ## -- Delete mf1 and mf2 if they exist to prevent an error -- ##
    try:
        for i in range(1,3):
            m = "mf%s" % i
            delete_matrix(matrix=my_emmebank.matrix(m))
    except:
        print("MF1 and MF2 did not need to be removed")

    try:
        cmap_transit_assignment.TransitAssignment().__call__(str(s['periodNum']), matrix_count, current_scenario, ccr_periods = "AM,PM", num_processors = 60)
        if msa_iteration == 4:
            cmap_network.CMapNetwork().__call__(databank.scenario(s['scenNum']), runPrep = False, export = True, 
                                                output_directory = "%s\\scen%s" % (EMME_OUTPUT, s['scenNum']))          
        ##print("Export transit matrices to OMX for time period " + s['period'])      
        ##cmap_matrix.CMapMatrix().outputTransitSkimsToOMX(s['period'], databank.scenario(s['periodNum']), 
        ##                                                    "%s\\taz_skims_new.omx" % ASIM_INPUTS) #TODO change skim filename
    except:
        print("There was an error in the %s period"%s['period'])
        traceback.print_exc() 
                                                                
print("Completed Transit Skim Process at %s"%(datetime.datetime.now()))
