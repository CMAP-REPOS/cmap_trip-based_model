# rsp_evaluation.py
# author: Tim OLeary, 1/2025
#
# this script was created to run as part of the RCP evaluations for the 
# 2026 long-range transportation plan. the script uses parquet files,
# full matrices, and an EDA share file to calculate the following:
#
#   - total daily trips for auto, truck, transit, for 7-county and EDA-based
#   - average travel time for auto, transit, truck, for 7-county and EDA-based, for AM and PM peak
#   - total daily VMT by vehicle type and facility type
#   - total daily VHT by vehicle type and facility type
#   - vehicle hours of excessive delay, by vehicle type and facility type
#       - "excessive delay" defined as congested time minus free-flow time
#   - congested hours of travel, by vehicle type and facility type
#       - "congested hours" defined as number of vehicle hours on roads where v/c ratio > 0.9
#   - (vehicle type includes passenger vehicle, truck, and transit)
#   - (facility type includes highway, arterial, and other)
#   - for transit assignment runs:
#       - transit boardings by service type (CTA Rail, CTA Bus, Pace, or Metra)
#       - transit passenger miles traveled
#       - transit passenger hours traveled


print('begin rsp_evaluation.py...')

## --------- ##
## LIBRARIES ##
## --------- ##

import os, sys
import numpy as np, pandas as pd
import yaml
import subprocess
import csv
from pathlib import Path


## ---------------------- ##
## INPUT AND OUTPUT FILES ##
## ---------------------- ##

db = next(dbdir for dbdir in Path(__file__).parents if dbdir.name == "Database").resolve()
rsp_out_dir = os.path.join(db, 'rsp_evaluation/outputs')
if not os.path.exists(rsp_out_dir):
    os.makedirs(rsp_out_dir)
rsp_out_csv = os.path.join(rsp_out_dir, 'rsp_out.csv') #final csv outputted here

rsp_dict={} #where final metrics are stored

#grab info from batch_file.yaml
with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)

year = 2050
model = config['model_version']  # e.g., 'c23q4'
scenyear = config['scenario_code'] # e.g., '400'
run_name = str(model) + '_' + str(scenyear)
transit_asmt = config['runTransitAsmt'] #True or False
ejfile = config['ejFile'] #eda partial demand file name
eda_share_dir = os.path.join(db, f'rsp_evaluation/inputs/{ejfile}') ## eda share of demand input file
ccrfile = config['ccrFile'] #ccr partial demand file name
ccr_share_dir = os.path.join(db, f'rsp_evaluation/inputs/{ccrfile}') ## ccr share of demand input file 
emission_rates_file = os.path.join(db, 'rsp_evaluation/inputs/Emission_rate_tables_2024.xlsx')

#get select link file information, if exists
slk_file_list = config['selectLinkFile'] #list of select link text files
slk_file_names = []
for slk in slk_file_list.split(','):
    slk = slk.strip()
    if slk.lower() != 'none':
        slk_file_names.append(slk)
print(f'\t - select link files: {", ".join(slk_file_names)}') #files that aren't 'None'
slk_files_dir = dict([[slk_name, os.path.join(db, f'select_link/{slk_name}')] for slk_name in slk_file_names])
slinks={} #create dict with SL name as key and list of links as value 
for slk_name in slk_files_dir.keys():
    sldir = slk_files_dir[slk_name]
    slk_links=[]
    with open(sldir, 'r') as file:
        for line in file.readlines():
            if line.strip().startswith('l='):
                ij = line.replace('l=','').strip().replace(',','-')
                slk_links.append(ij)
    slinks[slk_name] = slk_links

#get select line file information, if exists
sln_file = config['transitSelectFile'] #select line text file       
sline_dir = os.path.join(db, f'select_line/{sln_file}')
slines = None
if os.path.exists(sline_dir):
    print(f"\t - select line file: {config['transitSelectFile']}")
    slines = [] #list of queries for select line
    with open(sline_dir, 'r') as file:
        for line in file.readlines():
            if line.strip()[0].isalpha():
                slines.append(line.strip())

congested_threshold = 0.9 #v/c ratio defining a "congested" roadway
hov_veh_occ = 2.3 #avg hov vehicle occupancy

#get eda share from file -- will merge with demand/trip dfs
with open(eda_share_dir, 'r') as file:
    eda_share_col_labels = ['o_zone', 'eda_share']
    eda_share = []
    for line in file.readlines():
        if line.strip()[0].isnumeric():
            line = line.replace('all:', '')
            line = line.replace('\n', '')
            line = line.split()
            eda_share.append(line)
eda_share_df = pd.DataFrame(data=eda_share, columns=eda_share_col_labels)
eda_share_df['eda_share'] = eda_share_df['eda_share'].astype('float32')
eda_share_df['o_zone'] = eda_share_df['o_zone'].astype('int32')
eda_share_df.set_index('o_zone', inplace=True)

#get ccr share from file -- will merge with demand/trip dfs
with open(ccr_share_dir, 'r') as file:
    ccr_share_col_labels = ['o_zone', 'ccr_share']
    ccr_share = []
    for line in file.readlines():
        if line.strip()[0].isnumeric():
            line = line.replace('all:', '')
            line = line.replace('\n', '')
            line = line.split()
            ccr_share.append(line)
ccr_share_df = pd.DataFrame(data=ccr_share, columns=ccr_share_col_labels)
ccr_share_df['ccr_share'] = ccr_share_df['ccr_share'].astype('float32')
ccr_share_df['o_zone'] = ccr_share_df['o_zone'].astype('int32')
ccr_share_df.set_index('o_zone', inplace=True)

#merge eda and ccr together
eda_share_df = pd.concat([eda_share_df, ccr_share_df], axis=1).reset_index()

#define infill csv/excel files
infill_share = os.path.join(db, 'rsp_evaluation/inputs/infill_support.csv')

#get pollutants through subprocess (rsp_evaluation_get_pollutants.py)
#needs cmap-trip2 environment
print(f'\t - getting emission rates...')
subprocess_pollutants = os.path.join(db, 'post_macros/rsp_evaluation_get_pollutants.py')
out_pkl = os.path.join(db, 'rsp_evaluation/temp_out.pkl')
if os.path.exists(out_pkl):
    os.remove(out_pkl)

subprocess.run(f"conda run -n CMAP-TRIP2 python {subprocess_pollutants} {emission_rates_file} {out_pkl}", shell=True, check=True, text=True)

pollutants = pd.read_pickle(out_pkl)

os.remove(out_pkl)

## ---------- ##
## START EMME ##
## ---------- ##

#initialize emme
cmap_tbm_dir =  next(tbmdir for tbmdir in Path(__file__).parents if tbmdir.name.endswith('cmap_trip-based_model')).resolve()
sys.path.append(os.path.join(cmap_tbm_dir, 'Scripts'))

from tbmtools import project as tbm
modeller = tbm.connect(cmap_tbm_dir)
emmebank = modeller.emmebank

#define emme tools
net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
init_partition = modeller.tool('inro.emme.data.zone_partition.init_partition')
partition_transaction = modeller.tool('inro.emme.data.zone_partition.partition_transaction')
export_matrix = modeller.tool('inro.emme.data.matrix.export_matrices')
mtx_calc = modeller.tool('inro.emme.matrix_calculation.matrix_calculator')

#grab daily accumulation scenario
iter_num = int(emmebank.matrix('ms98').data) - 1
scen = emmebank.scenario(f'{scenyear}{iter_num}9') #e.g., '40029'

## GEOGRAPHY ##
#zones by county/geography 
#used for vmt statistics and RSP Eval
geo = {
    'Chicago': [1,717], #zones 1-717
    'Cook balance': [718,1732], #zones 718-1732
    'DuPage': [1733,2111], #zones 1733-2111
    'Kane': [2112,2304], #zones 2112-2304
    'Kendall': [2305,2325], #zones 2305-2325
    'Lake': [2326,2583], #zones 2326-2583
    'McHenry': [2584,2702], #zones 2584-2702
    'Will': [2703,2926], #zones 2703-2926
    'Illinois balance': [2927,3247], #zones 2927-3247
    'Indiana': [3248,3467], #zones 3248-3467
    'Wisconsin': [3468,3632] #zones 3468-3632
}

#list of 7-county zones for rsp eval
zones_7county_minmax_lists = [geo[g] for g in geo.keys() if not any(st in g for st in ['Illinois','Indiana','Wisconsin'])]
zones_7county = [zn for minmax in zones_7county_minmax_lists for zn in list(range(minmax[0],minmax[1]+1))]

#list of zones that are not point-of-entry
notpoe_minmax_lists = [geo[g] for g in geo.keys()]
zones_notpoe = [zn for minmax in notpoe_minmax_lists for zn in list(range(minmax[0], minmax[1]+1))]

######################
## DEFINE FUNCTIONS ##
######################

# function for pulling related matrix data into the a single pandas dataframe
def get_matrices(mtx_list):
    '''
    a function to grab a list of matrices from the activated emmebank 
    and return a single pandas dataframe of the matrix data in long format
    proper format of mtx_list:
    [
        ['desired_column_name_1', 'matrix_id_1'],
        ...,
        ['desired_column_name_n', 'matrix_id_n']
    ]
    '''
    #error catching for `mtx_list`
    if any(len(lst)!=2 for lst in mtx_list): #format wrong
        raise ValueError('get_matrices() takes a list of sublists, where each sublist contains the name for the matrix and the id: e.g., ["bplate", "mf4"]')
    if any(lst[1][:2] != 'mf' for lst in mtx_list): #not all full matrices
        raise ValueError('get_matrices() takes a list of sublists, where each sublist contains the name for the matrix and the id: e.g., ["bplate", "mf4"]')
    if any(emmebank.matrix(lst[1]) is None for lst in mtx_list): #mtx does not exist
        mf_dne = [mf for mf in [lst[1] for lst in mtx_list] if emmebank.matrix(mf) is None]
        raise ValueError(f'these matrices do not exist in emmebank: {mf_dne}')

    dfs = []
    for des_mtx in mtx_list:
        mtx = emmebank.matrix(des_mtx[1])
        mtx_np = mtx.get_numpy_data() #returns matrix in long format (o_zone=>row indices, d_zone=>column indices)
        mtx_df = pd.DataFrame(mtx_np, index=range(1,len(mtx_np)+1), columns=range(1,len(mtx_np)+1)) #create pandas dataframe w o_zone as row, d_zone as column
        mtx_df.reset_index(inplace=True) #make o_zone its own column
        mtx_df.rename(columns={'index':'o_zone'}, inplace=True)
        mtx_df = mtx_df.melt(id_vars='o_zone', var_name='d_zone', value_name=des_mtx[0]) #melt d_zone value into 1 column, make legible value name
        for col in ['o_zone', 'd_zone']:
            mtx_df[col] = mtx_df[col].astype('int32') #simplify and conform datatypes
        mtx_df.set_index(['o_zone','d_zone'], inplace=True)
        dfs.append(mtx_df)
    output_table = pd.concat(dfs, axis=1)
    if len(output_table) != 13315201:
        raise ValueError(f'An an issue occurred with get_matrices() -- output not 3649*3649 rows (13,315,201)')
    output_table.reset_index(inplace=True)
    
    return output_table


def wgt_avg(df, spec_list, group_by):
    dfi = df.copy()
    out_df_list = []
    '''
    function for calculating weighted averages of pandas dataframes, 
    given a desired output column name, a value, a weight, and/or a groupby column
    
    `spec_list` needs to be dict, actually, of the format:
    {
        'desired_output_column_name_1': ['value_column_1', 'weight_column_1'],
        ...
        'desired_output_column_name_n': ['value_column_n', 'weight_column_n']
    }
    '''
    
    #do each weighted average separately, concat together later
    for spec in spec_list.keys():
        desired_colname = spec
        value = spec_list[spec][0]
        weight = spec_list[spec][1]
        
        dfi[f'{value}_{weight}'] = dfi[value] * dfi[weight]
        
        if group_by is None:
            grouped = dfi
        else:
            grouped = dfi.groupby(group_by)
            
        agg_df = grouped.agg({f'{value}_{weight}':'sum', weight:'sum'})
        agg_df[f'{value}_avg'] = agg_df[f'{value}_{weight}'] / agg_df[weight]
        agg_df.rename(columns={f'{value}_avg':desired_colname}, inplace=True)
        agg_df = agg_df[[desired_colname]]
        out_df_list.append(agg_df)
        
    #output weighted averages
    if len(out_df_list)>1:
        out_df = pd.concat(out_df_list, axis=1)
    elif len(out_df_list)==1:
        out_df = out_df_list[0]
    else:
        raise ValueError("The weighted average wasn't computed successfully. Check parameters and input df!")
    return out_df


##################################
## RSP EVAL - MEAT AND POTATOES ##
##################################


#### ---------- ####
#### AUTO TRIPS ####
#### ---------- ####

## --
## IMPORT PARQUET FILES 
## --

#get parquet through subprocess (final_run_statistics_read_pq.py)
#needs cmap-trip2 environment to read parquet
print(f'\t - reading parquet files...')
subprocess_py = os.path.join(db, 'post_macros/final_run_statistics_read_pq.py')

out_pkl = os.path.join(db, 'post_macros/temp.pkl')

if os.path.exists(out_pkl):
    os.remove(out_pkl)

subprocess.run(f"conda run -n CMAP-TRIP2 python {subprocess_py} {db} {out_pkl}", shell=True, check=True, text=True)

pq_df = pd.read_pickle(out_pkl)
print(f"\t - {len(pq_df)} rows and {pq_df['trips'].sum()} trips loaded")
os.remove(out_pkl)

trips_all = pd.merge(pq_df, eda_share_df, on='o_zone', how='left')
trips_all.eval('eda_trips = eda_share * trips', inplace=True)
trips_all.eval('ccr_trips = ccr_share * trips', inplace=True)

## CLEANUP PARQUET: generalize purpose and mode categories

## Purposes to 3 categories: home-work (HW), home-other (HO), non-home (NH)
# entries in `purpose` column include:
#   - HBWL (home-based work, low value of time)     --> HW
#   - HBWH (home-based work, high value of time)    --> HW
#   - HBS (home-based shopping)                     --> HO
#   - HBO (home-based other)                        --> HO
#   - NHB (non-home based)                          --> NH
#   - VISIT (visitor trips)                         --> NH
#   - DEAD ("deadhead" trips - cab w/ no passenger) --> NH

## Modes to 3 categories: auto, transit, and non-motorized
# entries in `mode` column include:
#   - 1 (SOV)       --> auto
#   - 2 (HOV 2)     --> auto
#   - 3 (HOV 3+)    --> auto
#   - 4 (Taxi)      --> auto
#   - 5 (TNC)       --> auto
#   - 6 (shared TNC)--> auto
#   - 7 (transit)   --> transit
#   - 8 (bike)      --> non-motorized
#   - 9 (walk)      --> non-motorized  

#generalize purposes into HW, HO, NH
def gen_purpose(purpose):
    if 'HBW' in purpose:
        return 'HW'
    elif purpose == 'HBO' or purpose == 'HBS':
        return 'HO'
    else:
        return 'NH'

trips_all['gen_purpose'] = trips_all['purpose'].map(gen_purpose).astype('category')

#generalize modes into auto, transit, and non-motorized
def gen_mode(mode):
    if mode <= 6:
        return 'Auto'
    elif mode==7:
        return 'Transit'
    elif mode==8 or mode==9:
        return 'Non-Motorized'
    else:
        return None

trips_all['gen_mode'] = trips_all['mode'].map(gen_mode).astype('category')

#error catch purpose
if len(trips_all.loc[trips_all['gen_purpose'].isnull()])>0:
    raise ValueError(f"{len(trips_all.loc[trips_all['gen_purpose'].isnull()])} trips exist in parquet that were not given HW, HO, or NH purpose. What purposes are those? Re-check coding.")
#error catch mode
if len(trips_all.loc[trips_all['gen_mode'].isnull()])>0:
    raise ValueError(f"{len(trips_all.loc[trips_all['gen_mode']].isnull())} trips exist in parquet that have modes not 1-9. What modes are those? Re-check coding.")

# flag 7-county region trips, and filter
o_in_7cty_bool = trips_all['o_zone'].astype(int).isin(zones_7county) #boolean id-ing origin in 7-county region
d_in_7cty_bool = trips_all['d_zone'].astype(int).isin(zones_7county) #boolean id-ing destination in 7-county region

trips = trips_all.loc[o_in_7cty_bool | d_in_7cty_bool].copy()

# ## --
# ## GRAB ROADWAY MATRIX SKIMS
# ## --

#time/distance skims
# mf463 - SOV hwy time skim period 3
# mf467 - SOV hwy time skim period 7
#
# mf473 - SOV hwy distance skim period 3
# mf477 - SOV hwy distance skim period 7

desired_matrices_hwy = [
    ['time_veh_tod3', 'mf463'],
    ['time_veh_tod7', 'mf467'],
    #
    ['dist_veh_tod3', 'mf473'],
    ['dist_veh_tod7', 'mf477']
]

print(f"\t - loading auto matrices: {', '.join(mtx[1] for mtx in desired_matrices_hwy)}")
skims = get_matrices(desired_matrices_hwy)
trips = pd.merge(trips, skims, on=['o_zone','d_zone'], how='left') #merge hwy time/dist mtxs to parquet (`trips`)


## --
## CALCULATE AM AND PM PEAK TRAVEL TIME & DISTANCE, AUTO
## --

auto_trips_tod3_tod7 = trips.loc[(trips['timeperiod'].isin(['AM2','PM2']))&(trips['gen_mode']=='Auto')].copy()
auto_trips_tod3_tod7['timeperiod'] = auto_trips_tod3_tod7['timeperiod'].cat.remove_unused_categories()

#tod3 == AM2, tod7 == PM2
auto_trips_tod3_tod7['time'] = np.where(auto_trips_tod3_tod7['timeperiod']=='AM2', auto_trips_tod3_tod7['time_veh_tod3'], auto_trips_tod3_tod7['time_veh_tod7'])
auto_trips_tod3_tod7['distance'] = np.where(auto_trips_tod3_tod7['timeperiod']=='AM2', auto_trips_tod3_tod7['dist_veh_tod3'], auto_trips_tod3_tod7['dist_veh_tod7'])

#average time and distance, all-purpose auto trips AM/PM peak
wgt_avg_auto = wgt_avg(
    df=auto_trips_tod3_tod7, 
    spec_list={
        #'result_column':['value_column', 'weight_column']
        'avg_time': ['time','trips'],
        'avg_distance': ['distance','trips'],
        'avg_time_eda': ['time','eda_trips'],
        'avg_distance_eda': ['distance','eda_trips'],
        'avg_time_ccr': ['time','ccr_trips'],
        'avg_distance_ccr': ['distance','ccr_trips']
    },
    group_by='timeperiod'
)

#average time and distance, HBW auto trips AM/PM peak
auto_trips_tod37_hbw = auto_trips_tod3_tod7.loc[auto_trips_tod3_tod7['purpose'].str.contains('HBW')].copy()

wgt_avg_auto_hbw = wgt_avg(
    df=auto_trips_tod37_hbw, 
    spec_list={
        'avg_time':['time','trips'],
        'avg_distance':['distance','trips'],
        'avg_time_eda':['time','eda_trips'],
        'avg_distance_eda':['distance','eda_trips'],
        'avg_time_ccr':['time','ccr_trips'],
        'avg_distance_ccr':['distance','ccr_trips']
    },
    group_by='timeperiod'
)

## -- 
## SORT AM AND PM PEAK TRAVEL TIME & DISTANCE (AUTO)

    ## ---------------------------------------
    ## VOLUME OF TRIPS BY TRAVEL TIME TO WORK
    ## ---------------------------------------
    ##
    ##  - Less than 30 min
    ##      - Passenger vehicles
    ##      - Transit
    ##      - EDA-based trips
    ##  - 30-45 min
    ##      - Passenger vehicles
    ##      - Transit
    ##      - EDA-based trips
    ##  - 45+ minutes
    ##      - Passenger vehicles 
    ##      - Transit
    ##      - EDA-based trips
    ##
    ## ---------------------------------------

def time_bin(x):
    if x<30:
        return '0-30 min'
    elif x<45:
        return '30-45 min'
    else:
        return '45 min or more'
    
auto_trips_tod37_hbw['time_bin'] = auto_trips_tod37_hbw['time'].map(time_bin).astype('category')

time_bin_auto = auto_trips_tod37_hbw.groupby(['timeperiod','time_bin']).agg(
    {'trips':'sum','eda_trips':'sum','ccr_trips':'sum'})
for col in time_bin_auto.columns:
    time_bin_auto[col] = time_bin_auto[col].astype(int)


  
#### ----------- ####
#### TRUCK TRIPS ####
#### ----------- ####

## -- 
## Daily Truck Trips
## -- 

desired_truck_daily_matrices = [
    ['bplate', 'mf04'],
    ['ltruck', 'mf05'],
    ['mtruck', 'mf06'],
    ['htruck', 'mf07'],
    ['truckpoe', 'mf09']
]

print(f'\t - loading daily truck matrices: {", ".join(x[1] for x in desired_truck_daily_matrices)}')
truck_daily_dfs = []
daily_truck = get_matrices(desired_truck_daily_matrices)

#confine to trips orig or dest for 7 county
o_in_7cty_bool = daily_truck['o_zone'].isin(zones_7county)
d_in_7cty_bool = daily_truck['d_zone'].isin(zones_7county)
daily_truck = daily_truck.loc[o_in_7cty_bool | d_in_7cty_bool].copy()

#merge with eda share and calculate eda truck trips
daily_truck = pd.merge(daily_truck, eda_share_df, on='o_zone', how='left')
daily_truck.eval('tot_truck = bplate + ltruck + mtruck + htruck + truckpoe', inplace=True)
daily_truck.eval('eda_truck = tot_truck * eda_share', inplace=True)
daily_truck.eval('ccr_truck = tot_truck * ccr_share', inplace=True)
daily_truck = daily_truck.agg({'tot_truck':'sum', 'eda_truck':'sum', 'ccr_truck':'sum'})


# -- 
# output total daily trips
# --

tot_psgr_veh = trips.loc[(trips['gen_mode']=='Auto'), 'trips'].sum()
tot_truck = daily_truck.at['tot_truck'] 
tot_transit = trips.loc[trips['gen_mode']=='Transit', 'trips'].sum()
eda_psgr_veh = trips.loc[(trips['gen_mode']=='Auto'), 'eda_trips'].sum() 
eda_truck = daily_truck.at['eda_truck']
eda_transit = trips.loc[trips['gen_mode']=='Transit', 'eda_trips'].sum()
ccr_psgr_veh = trips.loc[(trips['gen_mode']=='Auto'), 'ccr_trips'].sum()
ccr_truck = daily_truck.at['ccr_truck']
ccr_transit = trips.loc[trips['gen_mode']=='Transit', 'ccr_trips'].sum()

rsp_dict['Total Daily Trips'] = {
    'All Trips': [
        ['Passenger Vehicles', tot_psgr_veh],
        ['Truck', tot_truck],
        ['Transit', tot_transit]
    ],
    'EDA-Based Trips': [
        ['Passenger Vehicle', eda_psgr_veh],
        ['Truck', eda_truck],
        ['Transit', eda_transit]
    ],
    'CCR-Based Trips': [
        ['Passenger Vehicle', ccr_psgr_veh],
        ['Truck', ccr_truck],
        ['Transit', ccr_transit]
    ]
}

## --
## AM and PM Travel Time & Distance, Truck
## --

#grab tod truck demand
desired_truck_matrices = [
    [
        ['bplate_tod7', 'mf30'],
        ['ltruck_tod7', 'mf31'],
        ['mtruck_tod7', 'mf32'],
        ['htruck_tod7', 'mf33']
    ],
    [
        ['bplate_tod3', 'mf34'],
        ['ltruck_tod3', 'mf35'],
        ['mtruck_tod3', 'mf36'],
        ['htruck_tod3', 'mf37']
    ]
]

print(f'\t - loading tod truck matrices: {", ".join(x[1] for lst in desired_truck_matrices for x in lst)}')
truck_dfs=[]
#doing am and pm separately, then concat together
for tod_spec in desired_truck_matrices:
    ampm_truck = get_matrices(tod_spec)
    if any(col.endswith('_tod7') for col in ampm_truck.columns.tolist()):
        ampm_truck['timeperiod'] = 'PM2'
        ampm_truck.eval(f'tot_truck = {"+".join(lst[0] for lst in tod_spec)}', inplace=True)
    else:
        ampm_truck['timeperiod'] = 'AM2'
        ampm_truck.eval(f'tot_truck = {"+".join(lst[0] for lst in tod_spec)}', inplace=True)
    ampm_truck.drop(columns=[lst[0] for lst in tod_spec], inplace=True)
    truck_dfs.append(ampm_truck)

#concat together
truck = pd.concat(truck_dfs, axis=0)
#limit to 7 counties
truck = truck.loc[(truck['o_zone'].isin(zones_7county))|(truck['d_zone'].isin(zones_7county))].copy()

#use tod highway time and distance skims for truck
truck = pd.merge(truck, skims, on=['o_zone','d_zone'], how='left')

#make time and distance columns
truck['time'] = np.where(truck['timeperiod']=='AM2', truck['time_veh_tod3'], truck['time_veh_tod7'])
truck['distance'] = np.where(truck['timeperiod']=='AM2', truck['dist_veh_tod3'], truck['dist_veh_tod7'])
truck.drop(columns=[col for col in truck.columns if col.endswith('_tod3') or col.endswith('_tod7')], inplace=True)

#calculate eda trucks
truck = pd.merge(truck, eda_share_df, on='o_zone', how='left')
truck.eval('eda_truck = tot_truck * eda_share', inplace=True)
truck.eval('ccr_truck = tot_truck * ccr_share', inplace=True)

wgt_avg_truck = wgt_avg(
    df = truck,
    spec_list={
        'avg_time':['time','tot_truck'],
        'avg_distance':['distance','tot_truck'],
        'avg_time_eda':['time','eda_truck'],
        'avg_distance_eda':['distance','eda_truck'],
        'avg_time_ccr':['time','ccr_truck'],
        'avg_distance_ccr':['distance','ccr_truck']
    },
    group_by='timeperiod'
)

#### ------------- ####
#### TRANSIT TRIPS ####
#### ------------- ####

## --
## GRAB TRANSIT MATRIX SKIMS
## --

desired_trnt_matrices_ampm=[
    #AM trips
    [
        ['am_vot1', 'mf504'],       # all trips vot 1
        ['am_vot2', 'mf505'],       # all trips vot 2
        ['am_vot3', 'mf506'],       # all trips vot 3
        ['am_hbw_vot1', 'mf516'],   # hbw trips vot 1
        ['am_hbw_vot2', 'mf517'],   # hbw trips vot 2
        ['am_hbw_vot3', 'mf518'],   # hbw trips vot 3     
        ['am_trn_ivt', 'mf822'],    # trips in-vehicle time
        ['am_trn_trxfer', 'mf823'], # trips transfer time
        ['am_trn_wait', 'mf824'],   # trips total wait time
    ],
    #PM trips
    [
        ['pm_vot1', 'mf510'],
        ['pm_vot2', 'mf511'],
        ['pm_vot3', 'mf512'],
        ['pm_hbw_vot1', 'mf522'],
        ['pm_hbw_vot2', 'mf523'],
        ['pm_hbw_vot3', 'mf524'],
        ['pm_trn_ivt', 'mf872'],
        ['pm_trn_trxfer', 'mf873'],
        ['pm_trn_wait', 'mf874']
    ]
]

print(f'\t - loading transit matrices: {", ".join(mtx[1] for lst in desired_trnt_matrices_ampm for mtx in lst)}')

trnt_ampm = []
#do AM and PM separately, cleanup tables, then concat together
for mtxs_list in desired_trnt_matrices_ampm:

    ampm = get_matrices(mtxs_list)
    
    #for AM and PM time periods, process trips and trip times;
    
    #am and pm
    cols = ampm.columns.tolist()
    if any(col.startswith('am_') for col in cols):
        ampm['timeperiod'] = 'AM'
    else:
        ampm['timeperiod'] = 'PM'
    col_renam = dict([[col, col.replace('am_','').replace('pm_','')] for col in cols])
    ampm.rename(columns=col_renam, inplace=True)
    #boil down to "trips" and "total time"
    ampm.eval('trips = vot1 + vot2 + vot3', inplace=True)
    ampm.eval('hbw_trips = hbw_vot1 + hbw_vot2 + hbw_vot3', inplace=True)
    ampm.eval('time = trn_ivt + trn_trxfer + trn_wait', inplace=True)
    ampm.drop(columns=[col for col in ampm.columns if 'trn_' in col or 'vot' in col], inplace=True)
    trnt_ampm.append(ampm)
    
transit = pd.concat(trnt_ampm)
transit = pd.merge(transit, eda_share_df, on='o_zone', how='left')
transit.eval('eda_hbw_trips = hbw_trips * eda_share', inplace=True)
transit.eval('eda_trips = trips * eda_share', inplace=True)
transit.eval('ccr_hbw_trips = hbw_trips * ccr_share', inplace=True)
transit.eval('ccr_trips = trips * ccr_share', inplace=True)
#only keep valid transit trips
transit = transit.loc[(transit['time']>0)&(transit['time']<1000)].copy()
#restrict to region
transit = transit.loc[(transit['o_zone'].isin(zones_7county))|(transit['d_zone'].isin(zones_7county))].copy()

#do weighted average time for AM/PM peak
wgt_avg_transit = wgt_avg(
    df = transit,
    spec_list={
        'avg_time_hbw':['time','hbw_trips'], 
        'avg_time_all':['time','trips'],
        'avg_time_hbw_eda':['time','eda_hbw_trips'],
        'avg_time_all_eda':['time','eda_trips'],
        'avg_time_hbw_ccr':['time','ccr_hbw_trips'],
        'avg_time_all_ccr':['time','ccr_trips']
    },
    group_by='timeperiod'
)

def time_bin(x):
    if x<30:
        return '0-30 min'
    elif x<45:
        return '30-45 min'
    else:
        return '45 min or more'
    
transit['time_bin'] = transit['time'].map(time_bin).astype('category')

time_bin_transit = transit.groupby(['timeperiod','time_bin']).agg({'hbw_trips':'sum', 'eda_hbw_trips':'sum', 'ccr_hbw_trips':'sum'})
for col in time_bin_transit.columns:
    time_bin_transit[col] = time_bin_transit[col].astype(int)

# --
# output time/distance measures
# -- 

# average travel time during am peak
rsp_dict['Average Travel Time - AM Peak'] = {
    'All Trips': [
        ['Truck', wgt_avg_truck.at['AM2', 'avg_time']],
        ['Passenger Vehicle', wgt_avg_auto.at['AM2','avg_time']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['AM2','avg_time']],
        ['Transit', wgt_avg_transit.at['AM', 'avg_time_all']],
        ['Transit, HBW', wgt_avg_transit.at['AM', 'avg_time_hbw']]
    ],
    'EDA-Based Trips': [
        ['Truck', wgt_avg_truck.at['AM2', 'avg_time_eda']],
        ['Passenger Vehicle', wgt_avg_auto.at['AM2','avg_time_eda']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['AM2','avg_time_eda']],
        ['Transit', wgt_avg_transit.at['AM','avg_time_all_eda']],
        ['Transit, HBW', wgt_avg_transit.at['AM','avg_time_hbw_eda']]
    ],
    'CCR-Based Trips': [
        ['Truck', wgt_avg_truck.at['AM2', 'avg_time_ccr']],
        ['Passenger Vehicle', wgt_avg_auto.at['AM2','avg_time_ccr']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['AM2','avg_time_ccr']],
        ['Transit', wgt_avg_transit.at['AM','avg_time_all_ccr']],
        ['Transit, HBW', wgt_avg_transit.at['AM','avg_time_hbw_ccr']]
    ]
}

# average travel time during pm peak
rsp_dict['Average Travel Time - PM Peak'] = {
    'All Trips': [
        ['Truck', wgt_avg_truck.at['PM2', 'avg_time']],
        ['Passenger Vehicle', wgt_avg_auto.at['PM2','avg_time']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['PM2','avg_time']],
        ['Transit', wgt_avg_transit.at['PM', 'avg_time_all']],
        ['Transit, HBW', wgt_avg_transit.at['PM', 'avg_time_hbw']]
    ],
    'EDA-Based Trips': [
        ['Truck', wgt_avg_truck.at['PM2', 'avg_time_eda']],
        ['Passenger Vehicle', wgt_avg_auto.at['PM2','avg_time_eda']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['PM2','avg_time_eda']],
        ['Transit', wgt_avg_transit.at['PM','avg_time_all_eda']],
        ['Transit, HBW', wgt_avg_transit.at['PM','avg_time_hbw_eda']]
    ],
    'CCR-Based Trips': [
        ['Truck', wgt_avg_truck.at['PM2', 'avg_time_ccr']],
        ['Passenger Vehicle', wgt_avg_auto.at['PM2','avg_time_ccr']],
        ['Passenger Vehicle, HBW', wgt_avg_auto_hbw.at['PM2','avg_time_ccr']],
        ['Transit', wgt_avg_transit.at['PM','avg_time_all_ccr']],
        ['Transit, HBW', wgt_avg_transit.at['PM','avg_time_hbw_ccr']]
    ]
}

# volume of trips by travel time to work, am peak
rsp_dict['Volume of Trips By Travel Time To Work, AM Peak']= {
    'Less than 30 min': [
        ['Passenger Vehicle', time_bin_auto.at[('AM2','0-30 min'), 'trips']],
        ['Transit', time_bin_transit.at[('AM','0-30 min'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('AM2', '0-30 min'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('AM', '0-30 min'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('AM2', '0-30 min'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('AM', '0-30 min'), 'ccr_hbw_trips']]
    ],
    '30 to 45 min': [
        ['Passenger Vehicle', time_bin_auto.at[('AM2','30-45 min'), 'trips']],
        ['Transit', time_bin_transit.at[('AM','30-45 min'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('AM2', '30-45 min'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('AM', '30-45 min'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('AM2', '30-45 min'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('AM', '30-45 min'), 'ccr_hbw_trips']]
    ],
    '45 min or more': [
        ['Passenger Vehicle', time_bin_auto.at[('AM2','45 min or more'), 'trips']],
        ['Transit', time_bin_transit.at[('AM','45 min or more'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('AM2', '45 min or more'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('AM', '45 min or more'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('AM2', '45 min or more'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('AM', '45 min or more'), 'ccr_hbw_trips']]
    ]
}

# volume of trips by travel time to work, pm peak
rsp_dict['Volume of Trips By Travel Time To Work, PM Peak']= {
    'Less than 30 min': [
        ['Passenger Vehicle', time_bin_auto.at[('PM2','0-30 min'), 'trips']],
        ['Transit', time_bin_transit.at[('PM','0-30 min'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('PM2', '0-30 min'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('PM', '0-30 min'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('PM2', '0-30 min'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('PM', '0-30 min'), 'ccr_hbw_trips']]
    ],
    '30 to 45 min': [
        ['Passenger Vehicle', time_bin_auto.at[('PM2','30-45 min'), 'trips']],
        ['Transit', time_bin_transit.at[('PM','30-45 min'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('PM2', '30-45 min'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('PM', '30-45 min'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('PM2', '30-45 min'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('PM', '30-45 min'), 'ccr_hbw_trips']]
    ],
    '45 min or more': [
        ['Passenger Vehicle', time_bin_auto.at[('PM2','45 min or more'), 'trips']],
        ['Transit', time_bin_transit.at[('PM','45 min or more'), 'hbw_trips']],
        ['EDA Passenger Vehicle', time_bin_auto.at[('PM2', '45 min or more'), 'eda_trips']],
        ['EDA Transit', time_bin_transit.at[('PM', '45 min or more'), 'eda_hbw_trips']],
        ['CCR Passenger Vehicle', time_bin_auto.at[('PM2', '45 min or more'), 'ccr_trips']],
        ['CCR Transit', time_bin_transit.at[('PM', '45 min or more'), 'ccr_hbw_trips']]
    ]
}


#### ------------------------------------ ####
#### VMT, VHT, AND MEASURES OF CONGESTION ####
#### ------------------------------------ ####

# vmt and vht calculations

print(f'\t - calculate vmt and vht')

# vehicle miles traveled, vehicle hours traveled, congestion calculations

punch = pd.read_csv(os.path.join(db, 'data/punchlink.csv'))
punch = punch.loc[punch['zone'].isin(zones_7county)].copy()


#define roadways into Arterial, Highway, and Other

#VDF to Art, Hwy, Other: 
#   - arterials: vdf== 1,6
#   - expressways: everything else

punch['road_class'] = 'Expressway'
punch.loc[punch['vdf'].isin([1,6]), 'road_class'] = 'Arterial'


## capacity and speed calculations-- borrowed from 'create moves input files' script
hours = {1:5, 2:1, 3:2, 4:1, 5:4, 6:2, 7:2, 8:2}
punch['hours'] = punch['timeperiod'].map(hours)
punch.eval('capacity = lan * emcap * hours', inplace=True)

punch.eval('volau = avauv + avh2v + avh3v + avbqv + avlqv + avmqv + avhqv + busveq', inplace=True)
punch.eval('auto = avauv + avh2v + avh3v', inplace=True)
punch.eval('truck = avbqv + avlqv + avmqv/2 + avhqv/3', inplace=True)
punch.eval('vehicles = avauv + avh2v + avh3v + avbqv + avlqv + avmqv/2 + avhqv/3 + busveq/3', inplace=True)

# arterial speed adjustment due to LOS C used in VDF
punch['fmph'] = np.where((punch['ftime'] > 0), (punch['len']/(punch['ftime']/60)), 20) #if ftime empty make fmph 20 (redundant but nice)
punch['mph'] = np.where(punch['vdf']==1, punch['fmph'] * (1/((np.log(punch['fmph'])*0.249)+0.153*(punch['volau']/(punch['capacity']*0.75))**3.98)), punch['len']/(punch['timau']/60))
punch['time'] = np.where(punch['vdf']==1, (punch['len']/punch['mph'])*60, punch['timau']) #recalculate travel time for vdf==1 (in minutes)

#added round speed and apply emission rete for different link speed
punch['mph_r'] = np.where((punch['mph'] < 35), (round(punch['mph'],1)), (round(punch['mph'],0)))
punch = pd.merge(punch, pollutants, left_on=['mph_r'], right_on=['Speed'], how='left')

#define "congested" -- v/c ratio above threshold
punch['vc_ratio'] = np.where(punch['capacity']>0, punch['volau']/punch['capacity'], np.nan)
punch['congested'] = np.where(punch['vc_ratio'] >= congested_threshold, 1, 0)

#"delay" is congested time minus free time, converted to hours
punch.eval('delay = (time - ftime) / 60', inplace=True)
punch['delay'].clip(lower=0, inplace=True) #ensure delay is not negative

#vehicle types to be calculated
# vehicle_cols = ['auto', 'truck', 'vehicles', 'ejauto', 'ejtruck']
vehicle_cols = ['auto', 'truck', 'vehicles', 'ejauto', 'ejtruck', 'ccrauto', 'ccrtruck']

for c in vehicle_cols:
    #vmt and congested vmt:
    #   - vmt = vol * length
    #   - cvmt = vmt * congestion flag
    punch.eval(f'vmt_{c} = {c} * len', inplace=True)
    punch.eval(f'cvmt_{c} = vmt_{c} * congested', inplace=True)

    #   - emission = vmt * emission rate(speed)
    punch.eval(f'ghg_{c} = vmt_{c} * GHG_ER', inplace=True)
    punch.eval(f'nox_{c} = vmt_{c} * NOx_ER', inplace=True)
    punch.eval(f'pm_{c} = vmt_{c} * PM_ER', inplace=True)
    punch.eval(f'voc_{c} = vmt_{c} * VOC_ER', inplace=True)
    
    #vht and congested vht:
    #   - vht = vol * (time / 60)
    #   - cvht = vht * congestion flag
    punch.eval(f'vht_{c} = {c} * (time / 60)', inplace=True)
    punch.eval(f'cvht_{c} = vht_{c} * congested', inplace=True)
    
    #hours of excessive delay:
    #   - delay = vol * link_delay
    punch.eval(f'delay_{c} = delay * {c}', inplace=True) # delay -- ((timau - ftime)/ 60) * vol for each category

#EDA/CCR VMT share on project links

if len(slinks) == 0:
    rsp_dict['VMT on Project Links (Roadway RSPs Only)'] = {
        'Total VMT': [
            ['Total VMT', 0],
            ['EDA VMT', 0],
            ['CCR_VMT', 0]
        ],
        'Pct of Total': [
            ['EDA Share', 0],
            ['CCR Share', 0]
        ]
    }

else:
    punch['ij'] = punch['i_node'].astype(str) + '-' + punch['j_node'].astype(str) 
    
    for slk in slinks.keys():
        # print('links for selection: ' ,slinks[slk])
        proj_links = punch.loc[punch['ij'].isin(slinks[slk])].copy()
        # print(slk, ' proj link table: ', proj_links)
        proj_vmt = proj_links.agg({'vmt_auto':'sum', 'vmt_ejauto':'sum', 'vmt_ccrauto':'sum'})
        proj_vmt['vmt_eda_share'] = proj_vmt['vmt_ejauto'] / proj_vmt['vmt_auto']
        proj_vmt['vmt_ccr_share'] = proj_vmt['vmt_ccrauto'] / proj_vmt['vmt_auto']
        # print('vmt stats on proj link table -- ', proj_vmt)
        
        rsp_dict[f'VMT on Project Links ({slk})'] = {
            'Total VMT': [
                ['Total VMT', proj_vmt['vmt_auto']],
                ['EDA VMT', proj_vmt['vmt_ejauto']],
                ['CCR_VMT', proj_vmt['vmt_ccrauto']]
            ],
            'Pct of Total': [
                ['EDA Share', proj_vmt['vmt_eda_share']],
                ['CCR Share', proj_vmt['vmt_ccr_share']]
            ]
        }

#create aggregator dictionary for agg() function
#   for each vehicle type in `vehicle_cols`, calculate each of the measures (vmt, cvmt, vht, cvht, delay)
aggs_prefixes = ['vmt', 'cvmt', 'vht', 'cvht', 'delay', 'ghg', 'nox', 'pm','voc']
fried_aggs = dict([[f'{p}_{v}', 'sum'] for p in aggs_prefixes for v in vehicle_cols])

vmt_vht_summary = punch.groupby('road_class').agg(fried_aggs)

# -- 
# output vmt/vht measures
# -- 

# vehicle miles traveled   
rsp_dict['Total Daily Vehicle Miles Traveled'] = {
    'By Vehicle Type': [
        ['Passenger Vehicle', vmt_vht_summary['vmt_auto'].sum()],
        ['Truck', vmt_vht_summary['vmt_truck'].sum()],
        ['EDA Passenger Vehicle', vmt_vht_summary['vmt_ejauto'].sum()],
        ['EDA Truck', vmt_vht_summary['vmt_ejtruck'].sum()],
        ['CCR Passenger Vehicle', vmt_vht_summary['vmt_ccrauto'].sum()],
        ['CCR Truck', vmt_vht_summary['vmt_ccrtruck'].sum()]
    ],
    'By Facility Type': [
        ['Arterial', vmt_vht_summary.at['Arterial', 'vmt_vehicles']],
        # ['Highway', vmt_vht_summary.at['Highway','vmt_vehicles']],
        # ['Other', vmt_vht_summary.at['Other','vmt_vehicles']]
        ['Expressway', vmt_vht_summary.at['Expressway','vmt_vehicles']]
    ]
}

# vehicle running emission  
rsp_dict['Total Daily Vehicle Running Emission'] = {
    'By Pollutant Types': [
        ['GHG', vmt_vht_summary['ghg_vehicles'].sum()],
        ['NOx', vmt_vht_summary['nox_vehicles'].sum()],
        ['PM', vmt_vht_summary['pm_vehicles'].sum()],
        ['VOC', vmt_vht_summary['voc_vehicles'].sum()]
    ],
    'By Facility and Pollutant Type': [
        ['Arterial GHG', vmt_vht_summary.at['Arterial', 'ghg_vehicles']],
        ['Expressway GHG', vmt_vht_summary.at['Expressway','ghg_vehicles']],
        ['Arterial NOx', vmt_vht_summary.at['Arterial', 'nox_vehicles']],
        ['Expressway NOx', vmt_vht_summary.at['Expressway','nox_vehicles']],
        ['Arterial PM', vmt_vht_summary.at['Arterial', 'pm_vehicles']],
        ['Expressway PM', vmt_vht_summary.at['Expressway','pm_vehicles']],
        ['Arterial VOC', vmt_vht_summary.at['Arterial', 'voc_vehicles']],
        ['Expressway VOC', vmt_vht_summary.at['Expressway','voc_vehicles']],
    ]
}

# vehicle hours traveled
rsp_dict['Total Daily Vehicle Hours Traveled'] = {
    'By Vehicle Type': [
        ['Passenger Vehicle', vmt_vht_summary['vht_auto'].sum()],
        ['Truck', vmt_vht_summary['vht_truck'].sum()],
        ['EDA Passenger Vehicle', vmt_vht_summary['vht_ejauto'].sum()],
        ['EDA Truck', vmt_vht_summary['vht_ejtruck'].sum()],
        ['CCR Passenger Vehicle', vmt_vht_summary['vht_ccrauto'].sum()],
        ['CCR Truck', vmt_vht_summary['vht_ccrtruck'].sum()]
    ],
    'By Facility Type': [
        ['Arterial', vmt_vht_summary.at['Arterial', 'vht_vehicles']],
        # ['Highway', vmt_vht_summary.at['Highway', 'vht_vehicles']],
        # ['Other', vmt_vht_summary.at['Other', 'vht_vehicles']]
        ['Expressway', vmt_vht_summary.at['Expressway', 'vht_vehicles']]
    ]
}

# vehicle hours of excessive delay    
rsp_dict['Vehicle-Hours of Excessive Delay'] = {
    'By Vehicle Type': [
        ['Passenger Vehicle', vmt_vht_summary['delay_auto'].sum()],
        ['Truck', vmt_vht_summary['delay_truck'].sum()],
        ['EDA Passenger Vehicle', vmt_vht_summary['delay_ejauto'].sum()],
        ['EDA Truck', vmt_vht_summary['delay_ejtruck'].sum()],
        ['CCR Passenger Vehicle', vmt_vht_summary['delay_ccrauto'].sum()],
        ['CCR Truck', vmt_vht_summary['delay_ccrtruck'].sum()]  
    ],
    'By Facility Type': [
        ['Arterial', vmt_vht_summary.at['Arterial', 'delay_vehicles']],
        # ['Highway', vmt_vht_summary.at['Highway', 'delay_vehicles']],
        # ['Other', vmt_vht_summary.at['Other', 'delay_vehicles']]
        ['Expressway', vmt_vht_summary.at['Expressway', 'delay_vehicles']]
    ]
}

# congested hours of travel
rsp_dict['Congested Hours of Travel'] = {
    'By Vehicle Type': [
        ['Passenger Vehicle', vmt_vht_summary['cvht_auto'].sum()],
        ['Truck', vmt_vht_summary['cvht_truck'].sum()],
        ['EDA Passenger Vehicle', vmt_vht_summary['cvht_ejauto'].sum()],
        ['EDA Truck', vmt_vht_summary['cvht_ejtruck'].sum()],
        ['CCR Passenger Vehicle', vmt_vht_summary['cvht_ccrauto'].sum()],
        ['CCR Truck', vmt_vht_summary['cvht_ccrtruck'].sum()]
    ],
    'By Facility Type': [
        ['Arterial', vmt_vht_summary.at['Arterial', 'cvht_vehicles']],
        # ['Highway', vmt_vht_summary.at['Highway', 'cvht_vehicles']],
        # ['Other', vmt_vht_summary.at['Other','cvht_vehicles']]
        ['Expressway', vmt_vht_summary.at['Expressway', 'cvht_vehicles']]
    ]
}


#### --------------------------- ####
#### TRANSIT BOARDINGS, PMT, PHT ####
#### --------------------------- ####

## this only runs if transit assignment was run
if transit_asmt:
    
    print(f'\t - calculate transit boardings, pmt, and pht')
    
    tr_dfs = []
    
    #grab segment info from all transit assignment scenarios: x21, x23, x25, x27
    for sc in [int(scenyear)+x for x in [21, 23, 25, 27]]:
        scen = emmebank.scenario(sc)

        network = scen.get_network()
        links = network.links()

        trpunch_lists = []
        for link in network.links():
            segments = link.segments() #empty if link does not have transit segments
            inode = network.node(link.i_node)
            jnode = network.node(link.j_node)
            #for each segment (if it has any), add this info to table:
            for segment in segments:
                seg = [
                    link.id,
                    segment.id,
                    link.length,
                    segment['transit_boardings'],
                    segment['transit_volume'],
                    segment['data1'],
                    inode['@zone'],
                    jnode['@zone']
                ]

                trpunch_lists.append(seg)
                
        columns = ['id', 'segment_id', 'length', 'transit_boardings','transit_volume','ltime','inode_zone','jnode_zone']
        trpunch = pd.DataFrame(data=trpunch_lists, columns=columns)

        #ensure all segments are in 7-county region
        trpunch = trpunch.loc[(trpunch['inode_zone'].isin(zones_7county))|(trpunch['jnode_zone'].isin(zones_7county))].copy()

        trpunch.eval('pmt = transit_volume * length', inplace=True)
        trpunch.eval('pht = transit_volume * ltime / 60', inplace=True)

        #use transit line info to determine mode
        def mode(x):
            # b - cta local; e - cta express
            # p - pace regular; q - pace express; l - pace feeder
            # c - cta rail; m - metra
            
            line = x.split('-')[0]
            name = ''.join([char for char in line if char.isalpha()])
            if name.startswith('c'):
                return 'CTA Rail'
            elif name.startswith('m'):
                return 'Metra'
            elif any(name.startswith(x) for x in ['p','q','l']):
                return 'Pace Bus'
            elif any(name.startswith(x) for x in ['b','e']):
                return 'CTA Bus'
            else:
                return None

        trpunch['mode'] = trpunch['segment_id'].map(mode)

        if len(trpunch.loc[trpunch['mode'].isnull()]) > 0:
            print(f'SOME TRANSIT SEGMENTS WERE NOT MAPPED TO A MODE!')
            print(trpunch.loc[trpunch['mode'].isnull()])
            raise ValueError('Some transit segments were not mapped to a mode (must be c, m, p, q, l, b, or e). Look at printed output of errors above.')

        trpunch['tod'] = sc
        tr_dfs.append(trpunch)

    transitpunch = pd.concat(tr_dfs)
    
    transit_summary = transitpunch.groupby('mode').agg(
        total_boardings = ('transit_boardings', 'sum'),
        total_pmt = ('pmt', 'sum'),
        total_pht = ('pht', 'sum')
    )
    
    ## output transit boardings, pmt, pht measures 
    rsp_dict['Transit Boardings, PMT, PHT (Transit RSPs Only)'] = {
        'Transit Boardings': [
            ['Metra', transit_summary.at['Metra', 'total_boardings']],
            ['CTA Rail', transit_summary.at['CTA Rail', 'total_boardings']],
            ['CTA Bus', transit_summary.at['CTA Bus', 'total_boardings']],
            ['Pace Bus', transit_summary.at['Pace Bus', 'total_boardings']]
        ],
        'Transit Passenger Miles Traveled': [
            ['Metra', transit_summary.at['Metra', 'total_pmt']],
            ['CTA Rail', transit_summary.at['CTA Rail', 'total_pmt']],
            ['CTA Bus', transit_summary.at['CTA Bus', 'total_pmt']],
            ['Pace Bus', transit_summary.at['Pace Bus', 'total_pmt']]
        ],
        'Transit Passenger Hours Traveled':[
            ['Metra', transit_summary.at['Metra', 'total_pht']],
            ['CTA Rail', transit_summary.at['CTA Rail', 'total_pht']],
            ['CTA Bus', transit_summary.at['CTA Bus', 'total_pht']],
            ['Pace Bus', transit_summary.at['Pace Bus', 'total_pht']]
        ]
    }
        

# ----------------------------------------------------------------------------
#  Infill supportiveness
# ----------------------------------------------------------------------------
print('\t - calculate infill supportiveness')
sl_vols = [
    ['sov1_trips', 'mf61'],
    ['sov2_trips', 'mf62'],
    ['sov3_trips', 'mf63'],
    ['hov_trips', 'mf64']
]

sl_auto = get_matrices(sl_vols)
sl_auto_npoe = sl_auto.loc[
    (sl_auto['o_zone'].isin(zones_notpoe)) & 
    (sl_auto['d_zone'].isin(zones_notpoe))
].copy()
sl_auto_npoe.eval('sl_trips = sov1_trips + sov2_trips + sov3_trips + hov_trips', inplace=True)
sl_auto_npoe.eval(f'sl_person_trips = sov1_trips + sov2_trips + sov3_trips + (hov_trips * {hov_veh_occ})', inplace=True)

#for select link trips
sl_sum = []
for trips in ['sl_trips','sl_person_trips']:
    o_sum = sl_auto_npoe.groupby('o_zone')[trips].sum()
    d_sum = sl_auto_npoe.groupby('d_zone')[trips].sum()
    sl_sum.append((o_sum + d_sum).to_frame())
sl_sum = pd.concat(sl_sum, axis=1).reset_index(names='o_zone')

#for all trips
trips_notpoe = trips_all.loc[
    (trips_all['o_zone'].isin(zones_notpoe)) &
    (trips_all['d_zone'].isin(zones_notpoe)) &
    (trips_all['gen_mode']=='Auto')
].copy()

o_all_sum = trips_notpoe.groupby('o_zone')['trips'].sum()
d_all_sum = trips_notpoe.groupby('d_zone')['trips'].sum()
all_sum = (o_all_sum + d_all_sum).to_frame().reset_index(names='o_zone')

sl_sum = pd.merge(sl_sum, all_sum, on='o_zone', how='outer')
infill_share_df = pd.read_csv(infill_share).rename(columns={'zone':'o_zone'})
sl_sum = pd.merge(sl_sum, infill_share_df, on='o_zone', how='left')
sl_sum['ratio'] = np.where(
    sl_sum['trips']>0,
    sl_sum['sl_person_trips']/sl_sum['trips'],
    0
)
for infl in ['infill1','infill2','infill3']:
    sl_sum.eval(f'{infl}_acres = ratio * {infl}', inplace=True)

acres_summary = sl_sum.agg(
    {'infill1_acres': 'sum',
     'infill2_acres': 'sum',
     'infill3_acres': 'sum'}
).round(3)

# output infill supportiveness measure
rsp_dict['Infill Supportiveness'] = [
    ['Infill 1 Acres', acres_summary['infill1_acres']],
    ['Infill 2 Acres', acres_summary['infill2_acres']],
    ['Infill 3 Acres', acres_summary['infill3_acres']]
]


## --
## export rsp evaluation work  
## --  
with open(rsp_out_csv, 'w') as file:
    ''' writes rsp_out to a csv in the format we want-- e.g.,
    [category, subcategory, data1, data2, ...]
    (and, if no subcategory exists)
    [category, '', data1, data2, ...]
    '''
    writer = csv.writer(file, lineterminator='\n')
    for category in rsp_dict.keys():
        writer.writerow([category.upper()])
        if type(rsp_dict[category])==dict:
            for subcategory in rsp_dict[category]:
                # writer.writerow([category, subcategory])
                for data in rsp_dict[category][subcategory]:
                    writer.writerow([subcategory] + data)
        else:
            rows = rsp_dict[category]
            writer.writerows(rows)

print(f'RSP Evaluation complete, stored at {rsp_out_csv}')