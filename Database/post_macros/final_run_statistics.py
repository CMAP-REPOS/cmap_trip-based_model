# a combo of old 'run_vmt_statistics.mac' and 'final_run_statisticsV2.mac' post_macros
# copies of macro descriptions kept below

# =====================================================================================================
# RUN_VMT_STATISTICS.MAC
# Craig Heither, 10-29-2018
#
#  ****************************************************************************************
#    Generate a file of model run VMT statistics for comparison to previous runs.
#    This creates detailed VMT numbers by district and vdf.
#    Includes calculating bus network vmt using @busveq  
#
#     --Heither 05-13-2021: remove @busveq from VMT calculation (already included in @vadt)
#     --Heither 08-21-2021: read Global Iteration value to automatically call appropriate scenario
#     --OLeary  10-10-2023: conversion to python 
#
#   Districts (revised for zone17 10-29-2018):
#     1: Chicago (zn 1-717)
#     2: Cook balance (zn 718-1732)
#     3: DuPage (zn 1733-2111)
#     4: Kane (zn 2112-2304)
#     5: Kendall (zn 2305-2325)
#     6: Lake (zn 2326-2583)
#     7: McHenry (zn 2584-2702)
#     8: Will (zn 2703-2926)
#     9: Illinois balance (zn 2927-3247)
#    10: Indiana (zn 3248-3467)
#    11: Wisconsin (zn 3468-3632)
#
# =====================================================================================================
#
# FINAL_RUN_STATISTICS.MAC
# Craig Heither, rev. 02-25-2015
#
#
# Generate a file of model run statistics for comparison to previous runs.
# This is a replacement for useful_macros\evaluate.run.
#  submit with 3-digit scenario number (i.e., " <post_macros\final_run_statistics.mac 100 " )
#
#  NRF revised 2-25-2015: @avhov replaced with @avh2v and @avh3v for 7 vehicle class version
#  CEB revised 3-27-2017: now an "8 class" version reading @busveq from the network too
#  CMH revised 5-04-2018: remove label written to file saying "excluding bus"
#  CMH revised 10-30-2018: revised non-attainment zone ranges for zone17
#  Heither 08-21-2021: read Global Iteration value to automatically call appropriate scenario
#  OLeary 10-10-2023: conversion to python
# =====================================================================================================

print('Final Run Statistics.py \nStarting...')

## INPUT INFO AND SETUP

#import libraries
import os
import numpy as np, pandas as pd
import re
import yaml

#input/output locations
workspace = os.getcwd() # 'Database' folder
run_name = workspace.split('\\')[-3] # model name folder (above 'cmap_trip-based_model')
output_vmtstats = workspace + '\\report\\vmt_statistics.csv' #output of RUN_VMT_STATISTICS
output_runstats = workspace + '\\report\\final_run_statistics.csv' #output of FINAL_RUN_STATISTICS

#import emme
import inro.emme.desktop.app as _app

#get emme project file path -- will be a parameter to cycle through for multiple projects
cwd = os.getcwd()
e = os.listdir(os.path.dirname(workspace))
empfile = [os.path.join(os.path.dirname(workspace), file) for file in e if file.endswith('.emp')][0] # <<-- 'emp' file of model run

print('Starting Emme...')
#start emme desktop
desktop = _app.start_dedicated(
    visible=False,
    user_initials="cmap",
    project=empfile
)

#import modeller
import inro.modeller as _m
modeller = _m.Modeller(desktop=desktop)
emmebank = modeller.emmebank

#grab model year, scenario number to use in tools
with open(workspace + r'\batch_file.yaml') as f:
    config = yaml.safe_load(f)
model = config['model_version']  # e.g., 'c23q4'
scenyear = config['scenario_code']  # e.g., '400'

#emme tools
net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
init_partition = modeller.tool('inro.emme.data.zone_partition.init_partition')
partition_transaction = modeller.tool('inro.emme.data.zone_partition.partition_transaction')
export_matrix = modeller.tool('inro.emme.data.matrix.export_matrices')
mtx_calc = modeller.tool('inro.emme.matrix_calculation.matrix_calculator')

#grab global iteration value to automatically call appropriate scenario (gonna be '2', as in s70029)
iter_num = mtx_calc(
    specification={
        "expression": "ms98",
        "result": None,
        "constraint": {
            "by_value": None,
            "by_zone": None
        },
        "aggregation": {
            "origins": None,
            "destinations": None
        },
        "type": "MATRIX_CALCULATION"
    }
)

iter_num = int(iter_num['result'] - 1)

#grab daily total accumulation scenario (e.g., 70029)
scen = emmebank.scenario(f'{scenyear}{iter_num}9')


## ------------------------ ##
## -- RUN VMT STATISTICS -- ##
## ------------------------ ##
print('Running VMT Statistics... ')
#GRAB DATA FROM MODEL

#create dictionary of zone structure
#will use in iterator later
geo = {
    'Chicago': [1,717], #zones 1-717
    'Cook balance': [718,1732], #zones 718-1732
    'DuPage': [1733,2111], #zones 1733-2111
    'Kane': [2112,2304], #zones 2112-2304/'
    'Kendall': [2305,2325], #zones 2305-2325
    'Lake': [2326,2583], #zones 2326-2583
    'McHenry': [2584,2702], #zones 2584-2702
    'Will': [2703,2926], #zones 2703-2926
    'Illinois balance': [2927,3247], #zones 2927-3247
    'Indiana': [3248,3467], #zones 3248-3467
    'Wisconsin': [3468,3632] #zones 3468-3632
}

#create dictionary to append calculations
data= {
    'Geography':[],
    'Road Type':[],
    f'{run_name}':[]
}

#iterate through each geography
for g in geo:
    
    print(f'    Calculating for {g}')
    # -- 
    #expressway vmt
    xwayvmt = net_calc(
        specification={
            "result": None,
            "expression": f"(@zone.ge.{geo[g][0]} .and. @zone.le.{geo[g][1]})*@vadt*length",
            "aggregation": None,
            "selections": {"link": "vdf=2 or vdf=4"},
            "type": "NETWORK_CALCULATION"
        },
        scenario=emmebank.scenario(scen),
    )
    #append results to dataframe shell
    data['Geography'].append(g)
    data['Road Type'].append('Expressway VMT')
    data[f'{run_name}'].append(xwayvmt['sum'])

    #--
    #arterial vmt
    artvmt = net_calc(
        specification={
            "result": None,
            "expression": f"(@zone.ge.{geo[g][0]} .and. @zone.le.{geo[g][1]})*@vadt*length",
            "aggregation": None,
            "selections": {"link": "vdf=1"},
            "type": "NETWORK_CALCULATION"
        },
        scenario=emmebank.scenario(scen),
    )
    #append to dataframe shell
    data['Geography'].append(g)
    data['Road Type'].append('Arterial VMT')
    data[f'{run_name}'].append(artvmt['sum'])

    #--
    #ramp-toll vmt
    ramptollvmt = net_calc(
        specification={
            "result": None,
            "expression": f"(@zone.ge.{geo[g][0]} .and. @zone.le.{geo[g][1]})*@vadt*length",
            "aggregation": None,
            "selections": {"link": "vdf=3 or vdf=5 or vdf=7 or vdf=8"},
            "type": "NETWORK_CALCULATION"
        },
        scenario=emmebank.scenario(scen),
    )
    #append to dataframe shell
    data['Geography'].append(g)
    data['Road Type'].append('Ramp/Toll VMT')
    data[f'{run_name}'].append(ramptollvmt['sum'])

    #--
    #centroid vmt
    centroidvmt = net_calc(
        specification={
            "result": None,
            "expression": f"(@zone.ge.{geo[g][0]} .and. @zone.le.{geo[g][1]})*@vadt*length",
            "aggregation": None,
            "selections": {"link": "vdf=6"},
            "type": "NETWORK_CALCULATION"
        },
        scenario=emmebank.scenario(scen),
    )
    #append to dataframe shell
    data['Geography'].append(g)
    data['Road Type'].append('Centroid VMT')
    data[f'{run_name}'].append(centroidvmt['sum'])

    #--
    #totals
    tot_district_vmt = xwayvmt['sum'] + artvmt['sum'] + ramptollvmt['sum'] + centroidvmt['sum']
    data['Geography'].append(g)
    data['Road Type'].append('Total District VMT')
    data[f'{run_name}'].append(tot_district_vmt)


## -- 
## -- create dataframe of results
## -- 

vmtstats = pd.DataFrame(data=data)
vmtstats.set_index(['Geography', 'Road Type'], inplace=True)

print('Done! Moving on...')
## -------------------------- ##
## -- FINAL RUN STATISTICS -- ##
## -------------------------- ##
print('Running Final Run Statistics...')
#initialize partitions
init_partition(partition='gn', scenario=scen)
init_partition(partition='gx', scenario=scen)

#partition transaction
partition_transaction(
    transaction_file = workspace+'\\data\\gn.in',
    scenario = scen
)


## --
## -- calculate trips info 
## -- 

# will use this dictionary to do matrix calculations
tripkey = {
    'B-Plate Truck' : 'mf4',
    'Light Truck' : 'mf5',
    'Medium Truck' : 'mf6',
    'Heavy Truck' : 'mf7',
    'POE Auto' : 'mf8',
    'POE Truck' : 'mf9',
    'POE Airport' : 'mf10'
}

#dictionary to store results
truck_poe_trips = {}

#matrix calculation for each vehicle type in trip key
for vtype in tripkey:
    print(f'    Calculating trips for {vtype}')
    result = mtx_calc(
        scenario=scen,
        specification= {
            "expression": f"{tripkey[vtype]}", #call corresponding matrix
            "result": None,
            "constraint": {
                "by_value": None,
                "by_zone": {
                    "origins": "gx0", 
                    "destinations": "gx0"
                }
            },
            "aggregation": {
                "origins": "+",
                "destinations": "+"
            },
            "type": "MATRIX_CALCULATION"
        }
    )
    result = result['result']
    truck_poe_trips[vtype] = result


## --
## -- calculate total vmt by vehicle type
## -- 

# will use this dictionary to do vmt network calculations
vclassvmt_key = {
    'Auto VMT': '(@avauv+@avh2v+@avh3v)',
    'B-Plate Truck VMT': '@avbqv',
    'Light Truck VMT': '@avlqv',
    'Medium Truck VMT': '(@avmqv/2)',
    'Heavy Truck VMT': '(@avhqv/3)',
    'Bus VMT': '(@busveq/3)'
}

#dictionary to store results
vclassvmt = {}

#selecting links in non-attainment area
link_selection = '''\
@zone=1,2304 or \
@zone=2326,2926 or \
@zone=2309,2313 or \
@zone=2317,2319 or \
@zone=2949 or \
@zone=2941 or \
@zone=2943,2944 \
'''

#network calculation for each vehicle type in vmt key
for vclass in vclassvmt_key:
    print(f'    Calculating {vclass}')
    result = net_calc(
        scenario = scen,
        specification = {
            "result": None,
            "expression": f"{vclassvmt_key[vclass]} * length",
            "aggregation": None,
            "selections": {
                "link": f"{link_selection}"
            },
            "type": "NETWORK_CALCULATION"
        }
    )
    vclassvmt[vclass] = result['sum']

totsum = np.sum(list(vclassvmt.values()))
vclassvmt['All VMT'] = totsum


## -- 
## put final run stats data together
## --

vclassvmt_s = pd.Series(vclassvmt)
truck_poe_trips_s = pd.Series(truck_poe_trips)
runstats = pd.concat([vclassvmt_s, truck_poe_trips_s], keys=['VMT', 'Trips'])
runstats = runstats.to_frame(name=run_name)

print('Done! Beginning export...')

## ------------ ##
## -- EXPORT -- ##
## ------------ ##

runstats.to_csv(output_runstats)
vmtstats.to_csv(output_vmtstats)

print(f'All done! Outputs exported to: \n    Final Run Statistics: {output_runstats} \n    VMT Statistics: {output_vmtstats}')