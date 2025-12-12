# coding=utf-8

# this flags tbm people as usualwfh or tc14
import pandas as pd
import numpy as np
import sys
import os

# Current trip_gen.bat
# python wfhflag.py %filedir% %savedir% %wfhFile% %wfh% %tc14%

# Adj trip_gen.bat
# python wfhflag.py %filedir% %savedir% %wfhFile% %wfhl% %wfhm% %wfhh%

# pathways
savedir = sys.argv[2]
assert os.path.exists(savedir) == True, "savedir not valid"
os.chdir(sys.argv[1])
synpoppath = "synthetic_persons.zip"
synhhpath = "synthetic_households.zip"
popsynhhpath = savedir + "/POPSYN_HH.csv"
indpxwalkpath = "indp_naics.csv"

#telework worker distribution by income, edu level and children
incdistpath = "incdist.csv"
edudistpath = "edudist.csv"
chidistpath = "chidist.csv"

# save additional output files?
savefiles = sys.argv[3]

# major parameters - source: mdt + nirpc survey (which is higher than PUMS data...)
# percent of all workers

''' This data will be separated from batch_file.yaml to Telework.yaml including Tuesday-Thursday telework rates'''

wfhl = float(sys.argv[4])
wfhm = float(sys.argv[5])
wfhh = float(sys.argv[6])

wfhpctlist = [wfhl, wfhm, wfhh]

# set seedvalue
seedvalue = 2
np.random.seed(seed=seedvalue)

# read in files
dfpop = pd.read_csv(synpoppath)
dfhh = pd.read_csv(synhhpath, dtype={'MV': object})
indpxwalk = pd.read_csv(indpxwalkpath)

# Need to change if plan to get data from Telework.yaml
incdist = pd.read_csv(incdistpath)
edudist = pd.read_csv(edudistpath)
chidist = pd.read_csv(chidistpath)

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

# telework distribution
inc_values = incdist.iloc[:, 2].astype(float).tolist()
edu_values = edudist.iloc[:, 2].astype(float).tolist()

# organize into dicts for easier access
incdist_dict = {
    "high": {1: inc_values[0], 2: inc_values[1]},
    "medium": {1: inc_values[2], 2: inc_values[3]},
    "low": {1: inc_values[4], 2: inc_values[5]},
}

edudist_dict = {
    "high": {1: edu_values[0], 2: edu_values[1]},
    "medium": {1: edu_values[2], 2: edu_values[3]},
    "low": {1: edu_values[4], 2: edu_values[5]},
}

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

##########################################
# save additional files
##########################################

if savefiles == "Y":
    print('saving additional files...')
    workers.to_csv("workers.csv", index=False)