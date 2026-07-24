'''
#####################################################################################
URBANSIM_SKIMS.PY
  Craig Heither, rev. 08-02-2021

    Create the highway and transit skim file needed by UrbanSim. 
	Transit skim time is in-vehicle time plus walk transfer time plus wait time.

#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os, numpy as np, pandas as pd

hwyTime = os.getcwd() + "\\emmemat\\mf44.emx"   				## -- Congested highway skim time
tranTime = os.getcwd() + "\\emmemat\\mf822.emx"   				## -- Indexed peak in-vehicle minutes
tranXfer = os.getcwd() + "\\emmemat\\mf823.emx"   				## -- Indexed peak walk transfer minutes
tranWait = os.getcwd() + "\\emmemat\\mf824.emx"   				## -- Indexed wait time minutes

outFolder = os.getcwd() + "\\data\\UrbanSim_skims"				## -- output folder for skims
outFile = outFolder + "\\skims.csv"

maxZone = 3649													## -- maximum zone number
maxInternal = 3632												## -- maximum non-POE zone

if not os.path.exists(outFolder):
	os.makedirs(outFolder)

# ---------------------------------------------------------------
# Import matrices.
# ---------------------------------------------------------------
auto = np.fromfile(hwyTime, dtype='f4')							## -- float, 4 bytes
transit = np.fromfile(tranTime, dtype='f4')						## -- float, 4 bytes
walk = np.fromfile(tranXfer, dtype='f4')						## -- float, 4 bytes
wait = np.fromfile(tranWait, dtype='f4')						## -- float, 4 bytes

## -- Create matrix origins and destinations
mtxdest = np.arange(1,maxZone+1)								## -- array of consecutive numbers representing matrix detinations
dest = np.tile(mtxdest,maxZone)									## -- array of repeating destination zone pattern
orig = np.repeat(mtxdest,maxZone)								## -- repeated in ascending order for origins										

autodf = pd.DataFrame(auto, columns = ['am_peak_travel_time'])
autodf.insert(loc=0, column='A',value=np.arange(len(autodf)))	## -- insert key for joining
origdf = pd.DataFrame(orig, columns = ['from_zone_id'])
origdf.insert(loc=0, column='A',value=np.arange(len(origdf)))
destdf = pd.DataFrame(dest, columns = ['to_zone_id'])
destdf.insert(loc=0, column='A',value=np.arange(len(destdf)))
transitdf = pd.DataFrame(transit, columns = ['InVehicle'])
transitdf.insert(loc=0, column='A',value=np.arange(len(transitdf)))
walkdf = pd.DataFrame(walk, columns = ['Walk_time'])
walkdf.insert(loc=0, column='A',value=np.arange(len(walkdf)))
waitdf = pd.DataFrame(wait, columns = ['Wait_time'])
waitdf.insert(loc=0, column='A',value=np.arange(len(waitdf)))

# ---------------------------------------------------------------
# Merge into dataframe.
# ---------------------------------------------------------------
skims = origdf.merge(destdf, how='left', on='A', copy=False)
skims = skims.merge(autodf, how='left', on='A', copy=False)
skims = skims.merge(transitdf, how='left', on='A', copy=False)
skims = skims.merge(walkdf, how='left', on='A', copy=False)
skims = skims.merge(waitdf, how='left', on='A', copy=False)
##skims = skims[skims.from_zone_id.le(maxInternal) & skims.to_zone_id.le(maxInternal)]	##-- exclude POEs from skim file - NOT necessary
skims['Wait_time'] = skims['Wait_time'] * 0.5					## -- multiply headway by 0.5 for wait time 
skims['am_peak_transit_time'] = skims['InVehicle'] + skims['Walk_time'] + skims['Wait_time']
skims['am_peak_transit_time'] = np.where(skims['am_peak_transit_time'] > 1000.0, 1000000.0, skims['am_peak_transit_time'])	##-- replace 1.00E20 with 1000000 to show no transit path exists
skims = skims.round({'am_peak_travel_time': 2, 'am_peak_transit_time': 2})
skims.to_csv(outFile, columns=['from_zone_id','to_zone_id','am_peak_travel_time','am_peak_transit_time'], index=False)
