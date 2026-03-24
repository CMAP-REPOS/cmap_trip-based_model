# coding=utf-8

# this flags tbm people as usualwfh or tc14
import pandas as pd
import numpy as np
from pathlib import Path
import yaml
import sys
import os

# pathways
savedir = sys.argv[2]
assert os.path.exists(savedir) == True, "savedir not valid"
os.chdir(sys.argv[1])
synpoppath = "synthetic_persons.zip"
synhhpath = "synthetic_households.zip"
popsynhhpath = savedir + "/POPSYN_HH.csv"
indpxwalkpath = "indp_naics.csv"
geoinpath = savedir + "/GEOG_IN.TXT"

db = Path(__file__).resolve().parents[3]  # database folder
with open(os.path.join(db, 'Telework.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    wfh_data = yaml.safe_load(lines_without_backslashes)

with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    batch_data = yaml.safe_load(lines_without_backslashes)

# find and read config file
config_file = Path(__file__).resolve().parents[4].joinpath('Scripts','prepare',
                                                           'conformity_scenario',
                                                           'hand','config.yaml')    
with open(config_file) as f:
    config = yaml.safe_load(f)

# save additional output files?
savefiles = sys.argv[3]

# scenario code and year
scen_code = batch_data['scenario_code']
real_year = config['scenario_years'][scen_code]

# Assume decline_rate from 2026 to 2050, 
decline_rate = wfh_data['declinerate']

# Based on year gap calculate the adjust rate from 2026 to scenaior year
scen_yr_adj_rate = 1 - (real_year-2026)/(2050-2026) * decline_rate
print('Telework decline rate applied: {0:.4f}'.format(scen_yr_adj_rate))
wfhl = wfh_data['wfhpctlow'] * scen_yr_adj_rate
wfhm = wfh_data['wfhpctmedium'] * scen_yr_adj_rate
wfhh = wfh_data['wfhpcthigh'] * scen_yr_adj_rate

# Combine to list for easy loop process
wfhpctlist = [wfhl, wfhm, wfhh]

# Read income and eduction portion for different WFH rate group from YAML file
incdist_dict = wfh_data['inc']
edudist_dict = wfh_data['edu']

# set seedvalue
seedvalue = 2
np.random.seed(seed=seedvalue)

# read in files
dfpop = pd.read_csv(synpoppath)
dfhh = pd.read_csv(synhhpath, dtype={'MV': object})
indpxwalk = pd.read_csv(indpxwalkpath)

# merge income2, edu2, trc get workers
dfpop = dfpop.merge(indpxwalk, on='INDP', how='left')
dfpop = dfpop.merge(dfhh[['household_id', 'HINCP19']], on='household_id', how='left')

dfpop['inccat2'] = pd.cut(dfpop.HINCP19, bins=[-99999, 100000, 2000000], right=False, labels=[1, 2])

workers = dfpop[dfpop['JWTR'] != 'bb'].copy()
workers.JWTR = workers.JWTR.astype(float).astype(int)
workers.SCHL = workers.SCHL.astype(float).astype(int)

workers.loc[workers.SCHL.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), 'edu'] = 1
workers.loc[workers.SCHL.isin([21, 22, 23, 24]), 'edu'] = 2

workers.loc[:, 'selected'] = 0
workers = workers[workers['ESR'] != '4']

def samplingworkers(df, wfhpctlist):
    df = df.copy()
    
    for idx, trc in enumerate(["low", "medium", "high"]):
        filtered_df = df[df["trc"] == trc]
        targetgrouptotal = int(len(filtered_df) * wfhpctlist[idx])
        
        for i in [1, 2]:
            for e in [1, 2]:
                inc_factor = incdist_dict[trc][i]
                edu_factor = edudist_dict[trc][e]
                targetwfhworkers = int(targetgrouptotal * inc_factor * edu_factor)
                
                sub_filtered_df = filtered_df[(filtered_df["inccat2"] == i) & (filtered_df["edu"] == e)]
                if targetwfhworkers > 0 and len(sub_filtered_df) >= targetwfhworkers:
                    sample_idx = sub_filtered_df.sample(n=targetwfhworkers, random_state=2).index
                    df.loc[sample_idx, "selected"] = 1  # mark selected rows
    
    return df


workers = samplingworkers(workers, wfhpctlist)

##########################################
# format and save
##########################################
print('preparing and saving HH_WFH_STATUS.csv...')
popsynhh = pd.read_csv(popsynhhpath, names=['sz', 'hhtype', 'vehicles',
                                               'serialno', 'stpuma5', 'rowcol', 'adults', 'workers',
                                               'children', 'iq', 'age', 'hhvtype', 'income'])

hhsummary = workers.groupby("household_id").agg(finalflag=("selected", lambda x: int(x.sum() > 0)),wfhworkers=("selected", "sum")).reset_index()

final1 = dfhh[['household_id','SERIALNO']].merge(hhsummary[['household_id','finalflag','wfhworkers']],on='household_id', how='left')
final1.finalflag.fillna(0, inplace=True)
try:
    final1['SERIALNO'] = final1['SERIALNO'].str.replace('HU','99')
except AttributeError:
    pass
final1sort = final1.sort_values('SERIALNO').reset_index()

pssort = popsynhh.sort_values('serialno').reset_index()
final1sort['newindex'] = pssort['index']
final1sort['sn2'] = pssort['serialno']
final1sort.sort_values('newindex', inplace=True)
final1sort.set_index('newindex', inplace=True)
final1sort['SERIALNO'] = final1sort.SERIALNO.astype('int64')
final1sort['diffcheck'] = final1sort['SERIALNO'] - final1sort['sn2']
assert (final1sort.diffcheck == 0).all(), "file not aligned with popsyn_hh"
final1sort.wfhworkers.fillna(0, inplace=True)
final1sort['finalflag'] = final1sort.finalflag.astype('int')
final1sort['wfhworkers'] = final1sort.wfhworkers.astype('int')
final1sort[['SERIALNO', 'finalflag','wfhworkers']].to_csv(savedir + "/HH_WFH_STATUS.CSV", index=False, header=False)

# Red GEO data get puma to county
pumacross = pd.read_csv(geoinpath, sep=",", usecols=[1, 2, 3, 4], names=["fips", "cnty_name", "state", "puma5"], header=None )
pumacross = pumacross.drop_duplicates()
pumacross["puma5"] = pumacross["puma5"].astype(str)

# Merge to new data prepare the county status
final2 = pd.merge(popsynhh, final1sort, left_index=True, right_index=True, how='inner')
final2['State'] = final2['stpuma5'].astype(str).str[0:2]
final2['PUMA'] = final2['stpuma5'].astype(str).str[2:]
final2['PUMA'] = final2['PUMA'].str.lstrip('0')
final2["PUMA"] = final2["PUMA"].astype(str)
final2_all = pd.merge(final2, pumacross, left_on="PUMA", right_on="puma5")

# Convert children to numeric first
final2_all["children"] = pd.to_numeric(final2_all["children"], errors="coerce")
final2_all["wfhworkers"] = pd.to_numeric(final2_all["wfhworkers"], errors="coerce")

# --- COUNTY MERGES & FILTERS --- 
# Combine Boone + Winnebago → Winnebago-Boone 
final2_all.loc[ 
    final2_all["cnty_name"].isin(["BOONE", "WINNEBAGO"]), "cnty_name" 
    ] = "WINNEBAGO-BOONE" 

# Combine Kane + Kendall → Kane-Kendall 
final2_all.loc[ 
    final2_all["cnty_name"].isin(["KANE", "KENDALL"]), "cnty_name" 
    ] = "KANE-KENDALL" 

# Remove Lee + Ogle 
final2_all = final2_all[~final2_all["cnty_name"].isin(["LEE", "OGLE"])]

county_worker = final2_all.groupby(['State','cnty_name'])[['workers','wfhworkers']].sum().reset_index()

county_worker.to_csv(savedir + "/PERSON_COUNTY_STATUS.CSV", index=False)

# Now collapse 3+ into "3+"
final2_all["children"] = final2_all["children"].apply(
    lambda x: "3+" if x >= 3 else x
)
final2_all["wfhworkers"] = final2_all["wfhworkers"].apply(
    lambda x: "3+" if x >= 3 else x
)

# Group and compute counts
hh_child_work = (
    final2_all
    .groupby(['cnty_name','State','children','wfhworkers'])
    .size()
    .reset_index(name='HH')
)

# Totals per county/state
hh_child_work['HH_county'] = (
    hh_child_work.groupby(['cnty_name','State'])['HH']
    .transform('sum')
)

# Shares
hh_child_work['HH_county_share'] = (
    hh_child_work['HH'] / hh_child_work['HH_county']
)

hh_child_work.to_csv(savedir + "/HH_CHILD_WORK.CSV", index=False)

##########################################
# save additional files
##########################################

if savefiles == "Y":
    print('saving additional files...')
    workers.to_csv("workers.csv", index=False)