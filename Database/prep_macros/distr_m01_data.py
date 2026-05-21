#filename: distr_m01_data.py
#description: Python script uses the EMME Modeler API to punch transit network and itinerary data for DISTR and M01 files creation
#author: Karly Cazzato, 9/20/2023

import os
import sys
import inro.emme.desktop.app as _app
import inro.modeller as _m
import pandas as pd
    
def main():
    # Define the path to the Emme project (.emp file)
    empFl = sys.argv[1]
    directory = os.getcwd().replace('\\Database','')
    empFile = os.path.join(directory,empFl)

    # start a dedicated instance of Emme Desktop connected to the specified project
    desktop = _app.start_dedicated(
        visible=True,
        user_initials='KCC',
        project= empFile
    )
    
    # Connect to the Modeller
    modeller = _m.Modeller(desktop=desktop)
    emmebank = modeller.emmebank
    netcalc = modeller.tool('inro.emme.network_calculation.network_calculator')

    # define variables
    currentScen = int(sys.argv[2])
    scenAM = currentScen
    scenMD = scenAM + 5
    t1= r'prep_macros\temp'
    t2= 'transit.itin'
    t3= 'node.txt'
    itinOut = (t1 + '\\' + t2)
    nodeOut = (t1 + '\\' + t3)

    #load specs
    itinAM = '''{
        "result": null,
        "expression": "dwt + veh + hdw",
        "aggregation": null,
        "selections": {
            "link": "all",
            "transit_line": "all"
        },
        "type": "NETWORK_CALCULATION"
    }'''

    nodeAM = '''{
        "result": null,
        "expression": "@pspac+@pcost+xi+yi+@zone",
        "aggregation": null,
        "selections": {
            "node": "5000,99999"
        },
        "type": "NETWORK_CALCULATION"
    }'''
    #run and save AM
    itineraryAM = netcalc(itinAM, scenario = emmebank.scenario(scenAM), full_report = True)
    nodeAM = netcalc(nodeAM, scenario = emmebank.scenario(scenAM), full_report = True)

    itineraryAMDF = pd.DataFrame(columns = itineraryAM['table'][0], data = itineraryAM['table'][1:])
    with open(itinOut, 'a') as f:
        dfAsString = itineraryAMDF.to_string(header=True, index=False)
        f.write(dfAsString)

    nodeAMDF = pd.DataFrame(columns = nodeAM['table'][0], data = nodeAM['table'][1:])
    with open(nodeOut, 'a') as f:
        dfAsString = nodeAMDF.to_string(header=True, index=False)
        f.write(dfAsString)
    #load spec Midday
    itinMD = '''{
        "result": null,
        "expression": "dwt + veh + hdw",
        "aggregation": null,
        "selections": {
            "link": "mod=BEPLQ",
            "transit_line": "mod=BEPLQ"
        },
        "type": "NETWORK_CALCULATION"
    }'''

    nodeMD = '''{
        "result": null,
        "expression": "@pspac+@pcost+xi+yi+@zone",
        "aggregation": null,
        "selections": {
            "node": "5000,29999"
        },
        "type": "NETWORK_CALCULATION"
    }'''
    #run and save midday
    itineraryMD = netcalc(itinMD, scenario = emmebank.scenario(scenMD), full_report = True)
    nodeMD = netcalc(nodeMD, scenario = emmebank.scenario(scenMD), full_report = True)

    t1= r'prep_macros\temp'
    t2= 'transit_midday.itin'
    t3= 'node_midday.txt'
    itinOut = (t1 + '\\' + t2)
    nodeOut = (t1 + '\\' + t3)

    itineraryMDDF = pd.DataFrame(columns = itineraryMD['table'][0], data = itineraryMD['table'][1:])
    with open(itinOut, 'a') as f:
        dfAsString = itineraryMDDF.to_string(header=True, index=False)
        f.write(dfAsString)

    nodeMDDF = pd.DataFrame(columns = nodeMD['table'][0], data = nodeMD['table'][1:])
    with open(nodeOut, 'a') as f:
        dfAsString = nodeMDDF.to_string(header=True, index=False)
        f.write(dfAsString)
    desktop.close()
if __name__ == '__main__':
    main()