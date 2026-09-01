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
#     --DWells  08-04-2026: full rework, switch from modeller tools to reading emmebank directly,
#                           aggregate final scenarios 1-8 instead of using X0029 scenario
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
#  DWells 07-15-2026: full rework, switch from modeller tools to reading emmebank directly,
#                     add missing statistics for comparison (person trips, transit share, trip distance, trip duration)
# =====================================================================================================

## INPUT INFO AND SETUP

import pandas as pd
import numpy as np
import os
import math
import fnmatch
import inro.emme.database.emmebank as _emmebank


############################################################################################################
#
# Helper functions
#
############################################################################################################

# 
# mappers (functions that map the value stored in the emmebank to the value we want for analysis purposes)
#

# zone to analysis geo (Chicago, Cook balanace, counties, state balances, etc.)
def zone2geo(zone):
    # Chicago - zones 1-717
    if zone <= 717:
        return "Chicago"
    # Cook balance - zones 718-1732
    elif zone <= 1732:
        return "Cook balance"
    # DuPage - zones 1733-2111
    elif zone <= 2111:
        return "DuPage"
    # Kane - zones 2112-2304
    elif zone <= 2304:
        return "Kane"
    # Kendall - zones 2305-2325
    elif zone <= 2325:
        return "Kendall"
    # Lake - zones 2326-2583
    elif zone <= 2583:
        return "Lake"
    # McHenry - zones 2584-2702
    elif zone <= 2702:
        return "McHenry"
    # Will - zones 2703-2926
    elif zone <= 2926:
        return "Will"
    # Illinois balance - zones 2927-3247
    elif zone <= 3247:
        return "Illinois balance"
    # Indiana - zones 3248-3467
    elif zone <= 3467:
        return "Indiana"
    # Wisconsin - zones 3468-3632
    elif zone <= 3632:
        return "Wisconsin"
    # POEs (zones 3633-3649) or external connenctor links (zone 9999)
    elif zone <= 3649 or zone == 9999:
        return "External"
    else: 
        print(f"    Warning: unrecognized zone for conversion to summary geography: {zone}")
        return None

# map vdf to road type
def vdf2type(vdf):
    # Arterials - vdf 1
    if vdf == 1:
        return "Arterial"
    # Expressways - vdf 2 and 4
    elif vdf in [2, 4]:
        return "Expressway"
    # Centroids - vdf 6
    elif vdf == 6:
        return "Centroid"
    # Ramps/Tolls - vdf 3, 5, 7, and 8
    elif vdf in [3, 5, 7, 8]:
        return "Ramp/Toll"
    else:
        print(f"    Warning: unrecognized vdf: {vdf}")

# boolean map if the zone is in the non-attainment area
def zone2naa(zone):
    # zones 1-2304
    if zone <= 2304:  # noqa: SIM114
        return True
    # zones 2326-2926
    elif zone >= 2326 and zone <= 2926:  # noqa: SIM114
        return True
    # zones 2309-2313
    elif zone >= 2309 and zone <= 2313:  # noqa: SIM114
        return True
    # zones 2317-2319
    elif zone >= 2317 and zone <= 2319:  # noqa: SIM114
        return True
    # zone 2949
    elif zone == 2949:  # noqa: SIM114
        return True
    # zone 2941
    elif zone == 2941:  # noqa: SIM114
        return True
    # zone 2943-2944
    elif zone >= 2943 and zone <= 2944:  # noqa: SIM103
        return True
    else:
        return False

#
# Misc helpers
#

# sort a dataframe
def sort_df(df: pd.DataFrame, sort_dict: dict):
    map_dict = {}
    for sorted_values in sort_dict.values():
        for i in range(len(sorted_values)):
            map_dict[sorted_values[i]] = i
    df = df.sort_values(
        by = list(sort_dict.keys()),
        key = lambda s: s.apply(lambda x: map_dict.get(x, max(map_dict.values()) + 1))
    )
    return df

# load a emme full matrix as an numpy array
def load_mf(path):
    mf = np.fromfile(path, np.dtype("float32"))
    n = int(math.sqrt(mf.shape[0]))
    mf = np.reshape(mf, (n, n))
    return mf

# creates labels/spacers that will cleanly combine all the other dataframes into one
def spacer_df(labels):
    if type(labels) is not list:
        labels = [labels]
    return pd.DataFrame(
        data = {
            "type": labels,
            "value": [" " for i in range(len(labels))]
        }
    )

# calculating weighted averages
def wgt_avg(df, spec_dict, group_by):
    dfi = df.copy()
    out_df_list = []
    '''
    function for calculating weighted averages of pandas dataframes, 
    given a desired output column name, a value, a weight, and/or a groupby column
    
    `spec_dict` needs to be of the format:
    {
        'desired_output_column_name_1': ['value_column_1', 'weight_column_1'],
        ...
        'desired_output_column_name_n': ['value_column_n', 'weight_column_n']
    }
    '''
    
    #do each weighted average separately, concat together later
    for spec in spec_dict.keys():
        desired_colname = spec
        value = spec_dict[spec][0]
        weight = spec_dict[spec][1]
        
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
    return out_df

# convert numbers to strings with nice formatting
def format_numbers(x):
    try:
        y = float(x)
        if y.is_integer():
            return f"{int(x):,}"
        else: 
            return f"{round(x, 2):,}"
    except ValueError:
        return str(x)

############################################################################################################
#
# Load Data Functions
#
############################################################################################################
    
# Load all needed data from emmebank
def load_emmebank(database_folder):
    if os.path.exists(os.path.join(database_folder, "emmebank")):
        emmebank_path = os.path.join(database_folder, "emmebank")
    else:
        raise FileNotFoundError("Couldn't find emmebank file. This script expects it to be in the \"Database\" and for the database folder to be the working directory of the script.")

    print('    Loading link data directly from emmebank...')
    emmebank = _emmebank.Emmebank(emmebank_path)
    link_attrs = []
    for tp in range(1, 9):
        scen = emmebank.scenario(tp)
        network = scen.get_network()
        for l in network.links():
            link_attrs.append([tp, l.i_node, l.j_node, l.i_node["@zone"], l.length, l.volume_delay_func, l["@avauv"], l["@avh2v"], l["@avh3v"], l["@avbqv"], l["@avlqv"], l["@avmqv"], l["@avhqv"], l["@busveq"]])
    link_attr_df = pd.DataFrame(data = link_attrs, columns = ["timeperiod", "i_node", "j_node", "zone", "length", "vdf", "avauv", "avh2v", "avh3v", "avbqv", "avlqv", "avmqv", "avhqv", "busveq"])

    return link_attr_df

# helper function to load trips from parquet files and join them with travel skims read directly from matrix files
def load_trips_with_skims(database_folder):
    #
    # Load trips from parquet files
    #
    print("    Loading trips from parquet files...")
    pq_folder = os.path.join(database_folder, "cache", "choice_simulator_trips_out")
    pq_files = fnmatch.filter(os.listdir(pq_folder), 'choice_simulator_trip*.pq')
    pq_df = pd.concat(
        pd.read_parquet(os.path.join(pq_folder, parquet_file)).reset_index()
        for parquet_file in pq_files
    )

    for col in ['o_zone','d_zone','a_zone','trips','hh_autos']:
        pq_df[col] = pq_df[col].astype('int32')
    for col in ['purpose','hh_inc5','timeperiod','mode']:
        pq_df[col] = pq_df[col].astype('category')

    ## Purposes to 3 categories: home-work (HW), home-other (HO), non-home (NH)
    # entries in `purpose` column include:
    #   - HBWL (home-based work, low value of time)     --> HW
    #   - HBWH (home-based work, high value of time)    --> HW
    #   - HBS (home-based shopping)                     --> HO
    #   - HBO (home-based other)                        --> HO
    #   - NHB (non-home based)                          --> NH
    #   - VISIT (visitor trips)                         --> NH
    #   - DEAD ("deadhead" trips - cab w/ no passenger) --> NH
    gen_purp = {
        'HBW': ['HBWL', 'HBWH'],
        'HBO': ['HBS', 'HBO'],
        'NHB': ['NHB', 'VISIT', 'DEAD']
    }

    for g_purp, purp_list in gen_purp.items():
        pq_df.loc[pq_df['purpose'].isin(purp_list), 'gen_purp'] = g_purp

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

    gen_mode = {x: 'Auto' for x in range(1,7)}
    gen_mode[7] = 'Transit'
    gen_mode[8] = 'Non-Motorized'
    gen_mode[9] = 'Non-Motorized'
    pq_df['gen_mode'] = pq_df['mode'].map(gen_mode)

    #
    # Load skims from matrix files
    #
    print("    Reading traffic skims directly from matrices...")
    timeperiods = dict(zip(["EA", "AM1", "AM2", "AM3", "MD", "PM1", "PM2", "PM3"], range(8)))
    time_skim_start = 461
    dist_skim_start = 471

    time_skims = []
    dist_skims = []

    for tp in range(8):
        time_skims.append(load_mf(os.path.join(database_folder, "emmemat", f"mf{time_skim_start + tp}.emx")))
        dist_skims.append(load_mf(os.path.join(database_folder, "emmemat", f"mf{dist_skim_start + tp}.emx")))
    time_skims.append(np.full(time_skims[0].shape, np.nan))
    dist_skims.append(np.full(dist_skims[0].shape, np.nan))

    time_skims = np.array(time_skims)
    dist_skims = np.array(dist_skims)

    # 
    # Join trips with skims
    # 
    print("    Joining trips with skims...")
    pq_df["time"] = time_skims[pq_df["timeperiod"].apply(lambda x: timeperiods.get(x, 8)).to_numpy(), pq_df["o_zone"].to_numpy()-1, pq_df["d_zone"].to_numpy()-1]
    pq_df["dist"] = dist_skims[pq_df["timeperiod"].apply(lambda x: timeperiods.get(x, 8)).to_numpy(), pq_df["o_zone"].to_numpy()-1, pq_df["d_zone"].to_numpy()-1]

    return pq_df

############################################################################################################
#
# Statistics Helper Functions
#
############################################################################################################

# calculate vmt by road type and geography
def get_vmt_by_geo(link_df: pd.DataFrame):
    print("    Calculating VMT for each summary geography...")
    # map zone to summary geographies
    link_df["Geography"] = link_df["zone"].apply(zone2geo)
    # drop links in external area
    link_df = link_df[~((link_df["Geography"] == "External") | (link_df["Geography"].isna()))].copy()

    # map vdf to road type
    link_df["Road Type"] = link_df["vdf"].apply(vdf2type) + " VMT"
    
    # calculate VMT
    link_df["VMT"] = (link_df["avauv"] + link_df["avh2v"] + link_df["avh3v"] + link_df["avbqv"] + link_df["avlqv"] + (link_df["avmqv"]/2) + (link_df["avhqv"]/3) + (link_df["busveq"]/3))*link_df["length"]
    
    # aggregate links by geography and road type
    vmt_by_type = link_df.groupby(["Geography", "Road Type"], as_index = False).agg({"VMT": "sum"})
    # calculate total vmt by geography
    vmt_totals = link_df.groupby("Geography", as_index = False).agg({"VMT": "sum"})
    vmt_totals["Road Type"] = "Total District VMT"

    # join with totals and sort
    vmt_df = pd.concat([vmt_by_type, vmt_totals])
    vmt_df = sort_df(
        df = vmt_df,
        sort_dict = {
            "Geography": ["Chicago", "Cook balance", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will", "Illinois balance", "Indiana", "Wisconsin"],
            "Road Type": ["Expressway VMT", "Arterial VMT", "Ramp/Toll VMT", "Centroid VMT", "Total District VMT"]
        }
    )

    return vmt_df

# calculate trips by vehicle type
def get_trips_by_vehtype(database_folder):
    print("    Calculating number of trips for each vehicle type...")
    if os.path.exists(os.path.join(database_folder, "emmemat")):
        matrix_folder = os.path.join(database_folder, "emmemat")
    else:
        raise FileNotFoundError("Couldn't find the emmemat folder. This script expects it to be in the \"Database\" and for the \"Database\" folder to be the working directory.")

    # get the total sum for each matrix
    trips_dict = {
        "B-Plate Truck": sum(sum(load_mf(os.path.join(matrix_folder, "mf4.emx")))),
        "Light Truck": sum(sum(load_mf(os.path.join(matrix_folder, "mf5.emx")))),
        "Medium Truck": sum(sum(load_mf(os.path.join(matrix_folder, "mf6.emx")))),
        "Heavy Truck": sum(sum(load_mf(os.path.join(matrix_folder, "mf7.emx")))),
        "POE Auto": sum(sum(load_mf(os.path.join(matrix_folder, "mf8.emx")))),
        "POE Truck": sum(sum(load_mf(os.path.join(matrix_folder, "mf9.emx")))),
        "POE Airport": sum(sum(load_mf(os.path.join(matrix_folder, "mf10.emx")))),
    }

    trips_df = pd.DataFrame(data = {
        "type": trips_dict.keys(),
        "value": trips_dict.values()
    })

    return trips_df

# calculate vmt by vehicle type (in the non-attainment area)
def get_vmt_by_vehtype(links_df: pd.DataFrame):
    print("    Calculating VMT for each vehicle type...")
    # only look at links in the non-attainment area
    naa_df = links_df[links_df["zone"].apply(zone2naa)].copy()

    # calculate VMT for each vehicle type
    vmt_dict = {
        "Auto VMT": sum((naa_df["avauv"] + naa_df["avh2v"] + naa_df["avh3v"])*naa_df["length"]),
        "B-Plate Truck VMT": sum(naa_df["avbqv"]*naa_df["length"]),
        "Light Truck VMT": sum(naa_df["avlqv"]*naa_df["length"]),
        "Medium Truck VMT": sum((naa_df["avmqv"]/2)*naa_df["length"]),
        "Heavy Truck VMT": sum((naa_df["avhqv"]/3)*naa_df["length"]),
        "Bus VMT": sum((naa_df["busveq"]/3)*naa_df["length"]),
    }
    # calculate total vMT
    vmt_dict["All VMT"] = sum(vmt_dict.values())

    vmt_df = pd.DataFrame(data = {
        "type": vmt_dict.keys(),
        "value": vmt_dict.values()
    })

    return vmt_df

# statistics based on trips data with skims (person trips by purpose, transit share, trip duration, trip distance)
# takes the dataframe from load_trips_w_skims()
def get_trips_w_skims_statistics(trips, area):
    # person trips
    print(f"    Calculating Person Trips and Transit Share for {area}...")
    person_trips_by_mode = trips.groupby(["gen_purp", "gen_mode"], as_index=False).agg({"trips": "sum"})
    person_trips_total = trips.groupby("gen_purp", as_index=False).agg({"trips": "sum"})
    person_trips_total["gen_mode"] = "Total"
    person_trips = pd.concat([person_trips_total, person_trips_by_mode])
    person_trips = person_trips.sort_values(
        by = ["gen_purp", "gen_mode"],
        key = lambda s: s.apply(lambda x: {
            "HBW": 1,
            "HBO": 2,
            "NHB": 3,
            "Total": 1,
            "Auto": 2,
            "Transit": 3,
            "Non-Motorized": 4
        }.get(x))
    )
    person_trips["type"] = person_trips["gen_purp"] + " " + person_trips["gen_mode"] + " Person Trips"
    person_trips["value"] = person_trips["trips"]
    person_trips = person_trips[["type", "value"]]

    # transit share
    transit_share_by_purpose = person_trips_by_mode[person_trips_by_mode["gen_mode"]=="Transit"].merge(
        person_trips_total,
        on = "gen_purp",
        suffixes = ["_transit", "_total"]
    )
    transit_share_total = transit_share_by_purpose.groupby("gen_mode_transit").agg({"trips_transit": "sum", "trips_total": "sum"})
    transit_share_total["gen_purp"] = "Overall"
    transit_share = pd.concat([transit_share_by_purpose, transit_share_total])
    transit_share = transit_share.sort_values(
        by = "gen_purp",
        key = lambda s: s.apply(lambda x: {
            "HBW": 1,
            "HBO": 2,
            "NHB": 3,
            "Overall": 4
        }.get(x))
    )
    transit_share["type"] = transit_share["gen_purp"] + " Transit Share"
    transit_share["value"] = transit_share["trips_transit"]/transit_share["trips_total"]
    transit_share = transit_share[["type", "value"]]

    print(f"    Calculating Average Trip Distance and Duration for {area}...")
    # calculate skims with weighted averages
    avg_skims = wgt_avg(
        df = trips[trips["gen_mode"]=="Auto"],
        spec_dict = {
            "avg_time": ["time", "trips"],
            "avg_dist": ["dist", "trips"]
        },
        group_by = "gen_purp"
    ).reset_index().sort_values(
        by = "gen_purp",
        key = lambda s: s.apply(lambda x: {
            "HBW": 1,
            "HBO": 2,
            "NHB": 3
        }.get(x))
    )

    # trip distance
    trip_distance = avg_skims.copy()
    trip_distance["type"] = trip_distance["gen_purp"] + " Trip Average Miles"
    trip_distance["value"] = trip_distance["avg_dist"]
    trip_distance = trip_distance[["type", "value"]]

    # trip duration
    trip_duration = avg_skims.copy()
    trip_duration["type"] = trip_duration["gen_purp"] + " Trip Average Minutes"
    trip_duration["value"] = trip_duration["avg_time"]
    trip_duration = trip_duration[["type", "value"]]

    return person_trips, transit_share, trip_distance, trip_duration

############################################################################################################
#
# Calculate statistics
#
############################################################################################################

if __name__ == "__main__":
    print('Final Run Statistics.py \nStarting...')

    #input/output locations
    workspace = os.getcwd() # 'Database' folder
    run_name = workspace.split('\\')[-3] # model name folder (above 'cmap_trip-based_model')
    output_vmtstats = workspace + '\\report\\vmt_statistics.csv' #output of RUN_VMT_STATISTICS
    output_runstats = workspace + '\\report\\final_run_statistics.csv' #output of FINAL_RUN_STATISTICS

    # get link data from emmebank
    link_data = load_emmebank(workspace)

    # get VMT by geography
    vmt_by_geo = get_vmt_by_geo(link_data)

    # get trips by vehicle type
    trips_by_vehtype = get_trips_by_vehtype(workspace)

    # get vmt by vehicle type
    vmt_by_vehtype = get_vmt_by_vehtype(link_data)

    # load trips with skims
    trips = load_trips_with_skims(workspace)

    # calculate trips_w_skims stats for the entire network
    en_person_trips, en_transit_share, en_trip_distance, en_trip_duration = get_trips_w_skims_statistics(trips, "ENTIRE NETWORK")

    # calculate stats for the non-attainment area
    naa_trips = trips[
        (trips["o_zone"].apply(zone2naa)) |
        (trips["d_zone"].apply(zone2naa))
    ].copy()
    naa_person_trips, naa_transit_share, naa_trip_distance, naa_trip_duration = get_trips_w_skims_statistics(naa_trips, "NON-ATTAINMENT AREA")

    print('    Done! Formatting data...')

    runstats = pd.concat([
        spacer_df(["ENTIRE NETWORK", "Person Trips"]),
        en_person_trips,
        spacer_df("Transit Share"),
        en_transit_share,
        spacer_df("Trip Distance"),
        en_trip_distance,
        spacer_df("Trip Duration"),
        en_trip_duration,
        spacer_df("Other Trips"),
        trips_by_vehtype,
        spacer_df([" ", "NON-ATTAINMENT AREA", "Person Trips"]),
        naa_person_trips,
        spacer_df("Transit Share"),
        naa_transit_share,
        spacer_df("Trip Distance"),
        naa_trip_distance,
        spacer_df("Trip Duration"),
        naa_trip_duration,
        spacer_df("Vehicle Class VMT"),
        vmt_by_vehtype
    ])

    runstats.reset_index(drop = True, inplace = True)

    ## ------------ ##
    ## -- EXPORT -- ##
    ## ------------ ##

    runstats["value"] = runstats["value"].apply(format_numbers)
    vmt_by_geo["VMT"] = vmt_by_geo["VMT"].apply(format_numbers)

    runstats.to_csv(output_runstats, index = False)
    vmt_by_geo.to_csv(output_vmtstats, index = False)

    print(f'All done! Outputs exported to: \n    Final Run Statistics: {output_runstats} \n    VMT Statistics: {output_vmtstats}')