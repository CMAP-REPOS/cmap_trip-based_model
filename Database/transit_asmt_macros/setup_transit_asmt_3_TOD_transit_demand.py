'''
#####################################################################################
SETUP_TRANSIT_ASMT_3_TOD_TRANSIT_DEMAND.PY
  Craig Heither, rev. 12-19-2022

    Script reads the parquet files created by the model. 
     - Separate transit trips from auto trips, enumerate transit trips
     - Determine TOD share of auto trips by time period, purpose, P-A vs. O-D
     - Attach value of time shares
     - Add two random variables & determine transit trip attributes
     - Write TOD demand to matrices
     - For now move trips from Origin zone to Boarding zone in this script so demand
       matrices are ready to use without needed matrix convolution in Emme.
     
#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os, pandas as pd, numpy as np
import fnmatch																##-- filter files in directory
import time

maxZone = 3649													             ## -- maximum zone number
np.random.seed(seed=5478)
t_start = time.process_time()

print("-- Creating time-of-day transit demand --")	
print("{0:=^50}".format('=',))


# ----------------------------------------------------------------------------
#  Input files.
# ----------------------------------------------------------------------------  
PQpth = os.getcwd() + "/cache/choice_simulator_trips_out"
dirListing = fnmatch.filter(os.listdir(PQpth), 'choice_simulator_trips*.pq')##-- create a list of .pq trip files in this directory
VOT = os.getcwd() + "/value_of_time_buckets.csv"                               ##-- Value of time bins
Mpth = os.getcwd() + "/emmemat"
tranTime = Mpth + "/mf822.emx"   				                               ##-- Indexed peak in-vehicle minutes
tranKzoneWk = Mpth + "/mf837.emx"   				                           ##-- Transit station boarding zone (HW trips)


# ----------------------------------------------------------------------------
#  Output files.
# ----------------------------------------------------------------------------  
Mtxpth = os.getcwd() + "/emmemat/mf"
a = list(range(501, 513))       ##-- matrices
newMatrices = []
for item in a:
    newMatrices.append(Mtxpth+str(item)+".emx")
print(newMatrices[0])   
#
Rptpth = os.getcwd() + "/transit_asmt_macros/report"
tmShare = Rptpth + "/todShare.csv" 
test1 = Rptpth + "/transit_trips.csv" 
test2 = Rptpth + "/test2.csv" 


#############################################################################################
# ----------------------------------------------------------------------------
#  Read the parquet files.
# ----------------------------------------------------------------------------  
newFiles = []
for item in dirListing:
    newFiles.append(PQpth+"/"+item)
print(" --> Trip files in directory: {0}".format(len(newFiles)))	
print("   -- Loading parquet files ... --")
a = pd.concat(
    pd.read_parquet(parquet_file).reset_index()
    for parquet_file in newFiles
)


# ----------------------------------------------------------------------------
#  Enumerate transit trips.
# ----------------------------------------------------------------------------
## -- TBM update modes: 1=SOV, 2=HOV2, 3=HOV3+, 4=taxi, 5=TNC, 6=shared ride TNC, 7=transit, 8=Bike, 9=Walk -- ##

## -- Direction flag -- ## 
a.loc[a['d_zone'] == a['a_zone'], 'dir'] = 1                                   ##-- 1=P-A format (equivalent to O-D)
a.loc[a['d_zone'] != a['a_zone'], 'dir'] = 2                                   ##-- 2=A-P format (equivalent to D-O)
a.loc[(a['purpose'] == 'NHB') | (a['purpose'] == 'VISIT') , 'dir'] = 0                                        ##-- 0=NHB (no directional split)

## -- Isolate transit trips -- ##
trnTrips = a[a['mode'] == 7].copy()
print("   --> Transit trips: {0}".format(trnTrips['trips'].sum()))
#
tr1 = trnTrips[trnTrips['trips'] == 1].copy()
tr2 = trnTrips[trnTrips['trips'] > 1].copy()                                   ##-- trips toenumerate
#print("   --> Transit trips: {0}, Rows in dataframe: {1}".format(tr2['trips'].sum(), tr2.shape[0]))
#
## -- Enumerate transit trips (i.e., one per row) -- ##
tr3 = pd.DataFrame(tr2.values.repeat(tr2.trips, axis=0), columns=tr2.columns)
tr3['trips'] = 1  
#print("   --> Transit trips: {0}, Rows in dataframe: {1}".format(tr3['trips'].sum(), tr3.shape[0]))
#tr3.to_csv(test1, index=False)
#
trnTrips = pd.concat([tr1, tr3], ignore_index=True, sort=False)
print("   --> After Enumeration - Transit trips: {0}, Rows in dataframe: {1}".format(trnTrips['trips'].sum(), trnTrips.shape[0]))

## -- Set random values -- ##
trnTrips['tm'] = np.random.uniform(size=len(trnTrips))
trnTrips['vt'] = np.random.uniform(size=len(trnTrips))


# ----------------------------------------------------------------------------
#  Determine TOD trip shares.
# ----------------------------------------------------------------------------
## -- Isolate auto trips to determine TOD shares -- ##
autoTrips = a[(a['mode'] < 7) & ((a['purpose'].str[:1] == 'H') | (a['purpose'] == 'NHB'))].copy()

## -- Set TOD values -- ##
autoTrips.loc[autoTrips['timeperiod'] == 'EA', 'TOD'] = 'TOD1' 
autoTrips.loc[autoTrips['timeperiod'] == 'AM1', 'TOD'] = 'TOD2'
autoTrips.loc[autoTrips['timeperiod'] == 'AM2', 'TOD'] = 'TOD3'
autoTrips.loc[autoTrips['timeperiod'] == 'AM3', 'TOD'] = 'TOD4'
autoTrips.loc[autoTrips['timeperiod'] == 'MD', 'TOD'] = 'TOD5'
autoTrips.loc[autoTrips['timeperiod'] == 'PM1', 'TOD'] = 'TOD6'
autoTrips.loc[autoTrips['timeperiod'] == 'PM2', 'TOD'] = 'TOD7'
autoTrips.loc[autoTrips['timeperiod'] == 'PM3', 'TOD'] = 'TOD8'
#
## -- Calculate Transit TOD Shares -- ##
atSumry = autoTrips.groupby(['purpose', 'dir']).agg({'trips': 'sum'}).reset_index()
atSumry.rename(columns={'trips': 'total_trips'}, inplace=True)
Sumry = autoTrips.groupby(['purpose', 'dir', 'TOD']).agg({'trips': 'sum'}).reset_index()
Sumry = Sumry.merge(atSumry, how='left', on=['purpose', 'dir'], copy=False)
Sumry['tShare'] = (Sumry['trips']/Sumry['total_trips']).round(4)
Sumry['todShare'] = Sumry.groupby(['purpose','dir'])['tShare'].cumsum().round(4)
Sumry.to_csv(tmShare, index=False)
## -- convert dataframe from long to wide -- ##
shares = Sumry.pivot(index=['purpose','dir'], columns='TOD', values='todShare').reset_index()
#
## -- Apply NHB rates to Visitor trips -- ##
share2 = shares[shares['purpose'] == 'NHB' ].copy()
share2['purpose'] = 'VISIT'
#print(share2.head())
shares = pd.concat([shares, share2], ignore_index=True, sort=False)
#print(shares.tail())
trnTrips = trnTrips.merge(shares, how='left', on=['purpose', 'dir'], copy=False)
#temp = trnTrips[trnTrips['o_zone'] < 7].copy()
#temp.to_csv(test1, index=False)
#print(trnTrips.head())


# ----------------------------------------------------------------------------
#  Attach Value of Time bin shares.
# ----------------------------------------------------------------------------
val1 = pd.read_csv(VOT, sep=',', comment='#')	
val1.rename(columns={'Purpose': 'purpose', 'Income Group': 'hh_inc5', }, inplace=True)
trnTrips = trnTrips.merge(val1, how='left', on=['purpose','hh_inc5'], copy=False)
#print(val1.head())


# ----------------------------------------------------------------------------
#  Assign Time Period and Value of Time Bins.
# ----------------------------------------------------------------------------
trnTrips['time0'] = 1
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD1'], 'time0'] = 2
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD2'], 'time0'] = 3
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD3'], 'time0'] = 4
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD4'], 'time0'] = 5
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD5'], 'time0'] = 6
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD6'], 'time0'] = 7
trnTrips.loc[trnTrips['tm'] > trnTrips['TOD7'], 'time0'] = 8
#
## -- Set new TOD values -- ##
trnTrips['time'] = 3                                                           ##-- 3 MD: 9am-4pm (after logic)
trnTrips.loc[(trnTrips['time0'] == 1) | (trnTrips['time0'] == 8), 'time'] = 1  ##-- 1 NT: 6pm-6am
trnTrips.loc[(trnTrips['time0'] == 2) | (trnTrips['time0'] == 3), 'time'] = 2  ##-- 2 AM: 6am-9am
trnTrips.loc[trnTrips['time0'] == 7, 'time'] = 4                               ##-- 4 PM: 4pm-6pm
#
## -- Set VOT bins -- ##
trnTrips['valuTm'] = 1
trnTrips.loc[trnTrips['vt'] > trnTrips['Low VOT'], 'valuTm'] = 2
trnTrips.loc[trnTrips['vt'] > (trnTrips['Low VOT'] + trnTrips['Mid VOT']), 'valuTm'] = 3
##temp = trnTrips[trnTrips['o_zone'] < 7].copy()
##temp.to_csv(test2, index=False) -- QC


# ----------------------------------------------------------------------------
#  Create a template of all zonal interchanges to write to matrix files -- ##
# ----------------------------------------------------------------------------
## -- Create matrix origins and destinations -- ##
mtxdest = np.arange(1,maxZone+1)								## -- array of consecutive numbers representing matrix destinations
dest = np.tile(mtxdest,maxZone)									## -- array of repeating destination zone pattern
orig = np.repeat(mtxdest,maxZone)								## -- repeated in ascending order for origins
origdf = pd.DataFrame(orig, columns = ['o_zone'])
origdf.insert(loc=0, column='A',value=np.arange(len(origdf)))
destdf = pd.DataFrame(dest, columns = ['d_zone'])
destdf.insert(loc=0, column='A',value=np.arange(len(destdf)))
tmplt = origdf.merge(destdf, how='left', on='A', copy=False)


# ----------------------------------------------------------------------------
#  Move trips from Origin zone to Boarding zone (may be temporary).
# ----------------------------------------------------------------------------
transit = np.fromfile(tranTime, dtype='f4')						## -- float, 4 bytes
kzWk = np.fromfile(tranKzoneWk, dtype='f4')					    ## -- float, 4 bytes
transitdf = pd.DataFrame(transit, columns = ['InVehicle'])
transitdf.insert(loc=0, column='A',value=np.arange(len(transitdf)))
wkKZonedf = pd.DataFrame(kzWk, columns = ['kZoneWrk'])
wkKZonedf.insert(loc=0, column='A',value=np.arange(len(wkKZonedf)))
moveTrip = tmplt.merge(transitdf, how='left', on='A', copy=False)
moveTrip = moveTrip.merge(wkKZonedf, how='left', on='A', copy=False)
#
## -- Attach K zones to trips -- ##
trTrips =  trnTrips.merge(moveTrip, how='left', on=['o_zone', 'd_zone'], copy=False)
## -- Determine assignment origin zone -- ##
trTrips['asmtOrig'] = trTrips['o_zone']
trTrips.loc[trTrips['InVehicle'] > 1000, 'asmtOrig'] = trTrips['kZoneWrk']
trTrips1 = trTrips[['purpose','o_zone','d_zone','hh_autos','hh_inc5','trips','dir','time','valuTm','kZoneWrk','asmtOrig']].copy()
trTrips1.to_csv(test1, index=False)
## -- Move from Origin to Boarding zone -- ##
trnTrips = trTrips[['purpose','d_zone','trips','time','valuTm','asmtOrig']].copy()
trnTrips.rename(columns={'asmtOrig': 'o_zone'}, inplace=True)


# ----------------------------------------------------------------------------
#  Calculate TOD transit trips.
# ----------------------------------------------------------------------------
y = 1                                                                          ##-- TOD counter
dt = np.dtype('float32') 
for x in range(0,10,3):
    ## -- Separate time period trips by VOT class -- ##
    tmv1 = trnTrips[(trnTrips['time'] == y) & (trnTrips['valuTm'] == 1)].copy()
    tmv2 = trnTrips[(trnTrips['time'] == y) & (trnTrips['valuTm'] == 2)].copy()
    tmv3 = trnTrips[(trnTrips['time'] == y) & (trnTrips['valuTm'] == 3)].copy()
    ## -- Summarize by O-D -- ##
    t1 = tmv1.groupby(['o_zone', 'd_zone']).agg({'trips': 'sum'}).reset_index()
    t2 = tmv2.groupby(['o_zone', 'd_zone']).agg({'trips': 'sum'}).reset_index()
    t3 = tmv3.groupby(['o_zone', 'd_zone']).agg({'trips': 'sum'}).reset_index()
    ## -- Merge with template -- ##  
    tmplt1 = tmplt.merge(t1, how='left', on=['o_zone', 'd_zone'], copy=False)
    tmplt1['trips'] = tmplt1['trips'].fillna(0)	
    tmplt2 = tmplt.merge(t2, how='left', on=['o_zone', 'd_zone'], copy=False)
    tmplt2['trips'] = tmplt2['trips'].fillna(0)
    tmplt3 = tmplt.merge(t3, how='left', on=['o_zone', 'd_zone'], copy=False)
    tmplt3['trips'] = tmplt3['trips'].fillna(0)
    if x == 0:
        print("   --> Rows in template: {0} -- it should be 13,315,201".format(tmplt1.shape[0]))
    print(" --> TOD {0} trips: VOT1 - {1:.0f}, VOT2 - {2:.0f}, VOT3 - {3:.0f}".format(y, tmplt1['trips'].sum(), tmplt2['trips'].sum(), tmplt3['trips'].sum())) 
    ## -- Write to matrices -- ##
    trp_array1 = tmplt1[['trips']].to_numpy(copy=True).astype(dt)
    trp_array1.tofile(newMatrices[x])
    trp_array2 = tmplt2[['trips']].to_numpy(copy=True).astype(dt)
    trp_array2.tofile(newMatrices[x+1])
    trp_array3 = tmplt3[['trips']].to_numpy(copy=True).astype(dt)
    trp_array3.tofile(newMatrices[x+2])
    y += 1

t_stop = time.process_time()
print("Elapsed time in minutes: {0:.2f}".format((t_stop-t_start)/60))
