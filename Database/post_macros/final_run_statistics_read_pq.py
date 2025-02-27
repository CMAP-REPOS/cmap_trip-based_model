import os
import sys
import re
import fnmatch
import pyarrow
import pandas as pd, numpy as np

# grab data
## ----------------------------------------------------------------
## NEED TO GRAB PARQUET FILES, WHICH STORE TRIP-LEVEL INFORMATION
## can find these in ..\cmap_trip-based_model\Database\cache
## ----------------------------------------------------------------

db = sys.argv[1]
pqpth = 'cache/choice_simulator_trips_out'
pqpths = os.listdir(os.path.join(db, pqpth))
pqfiles = fnmatch.filter(pqpths, 'choice_simulator_trip*.pq')

out_pkl = sys.argv[2]

def pq_files():

    pq_df = pd.concat(
        pd.read_parquet(os.path.join(db, pqpth, parquet_file)).reset_index()
        for parquet_file in pqfiles
    )
    
    for col in ['o_zone','d_zone','a_zone','trips','hh_autos']:
        pq_df[col] = pq_df[col].astype('int32')
    for col in ['purpose','hh_inc5','timeperiod','mode']:
        pq_df[col] = pq_df[col].astype('category')
        
    pq_df.to_pickle(out_pkl, protocol=4)

if __name__ == '__main__':
    pq_files()