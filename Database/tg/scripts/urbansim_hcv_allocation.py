'''
#####################################################################################
URBANSIM_HCV_ALLOCATION.PY
  Craig Heither, rev. 07-21-2025
  Karly Cazzato, rev. 01-21-2026

    Script reads subzone employment by NAICS from UrbanSim files and uses it to develop 
	allocation weights for heavy commercial vehicle trips. NAICS-level employment 
    is paired with trips/employee by NAICS estimates and weighted by the zonal share
    of 2020 building squarefeet. If no buildings exist in the zone in 2020 then a county average
    zonal share of building sqft is applied to ensure future employment in the zones is 
    considered. External zones were assigned a zonal share of building sqft based on 
    an internal zone with a similar trip truck generation rate based on the ATRI
    calibration dataset. These weighted values are then scaled up to realistic trip volumes. 
    A file of estimated 2018 truck trips from rail intermodal facilities is also included to 
    address a gap in the methodology. This ensures the allocation is consistent with the 
    forecast growth in HCV-dependent land uses.

#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os, pandas as pd, csv
import fnmatch																##-- filter files in directory
import numpy as np
cmapSubzone = 16426															##-- maximum internal CMAP subzone 
maxZone = 2926
total_wgt = 564043.02617679   # From ATRI calibration dataset, total weight value for all internal and external truck trips; used to scale final weights to realistic value so intermodal can be added in
                                                                      
# ----------------------------------------------------------------------------
#  Input files.
# ----------------------------------------------------------------------------  
USpth ="..\\UrbanSim_inputs"
## -- create a list of the UrbanSim files to be processed -- ##
d1 = fnmatch.filter(os.listdir(USpth), '*subzonetm*')                          ##-- subzone files
newFiles = []
for item in d1:
    newFiles.append(USpth+"\\"+item)
#
geog = "..\\fortran\\GEOG_IN.TXT"
imx = "..\\..\\data\\hcv_intermodal.csv"                                       ##-- estimate of 2018 truck trips for intermodal facilities
in_naicsRates = "..\\..\\data\\tg_rates.csv"                                   ##-- estimated trips/employee by NAICS code                              
in_build_props = "..\\..\\data\\land_use_proportions.csv"                      ##-- proportion of building sqft in zone of all building sqft in CMAP region

# ----------------------------------------------------------------------------
#  Output files.
# ----------------------------------------------------------------------------  
mo20 = "..\\..\\data\\mo20.txt"

print("Subzone files: {0}".format(newFiles))

# ----------------------------------------------------------------------------
#  Read subzone employment, geographic correspondence and truck trip rates.
# ----------------------------------------------------------------------------  
## -- Read in GEOG_IN to attach Zones -- ##
geo = pd.read_csv(geog, sep=',', header=None, usecols=[0, 5])
geo.columns=['subzone_id','zone']
print(" --> GEOG_IN subzone summary Rows: {0}".format(geo.shape[0]))
#
## -- Read in trips/employee by NAICS -- ##        
naicsRates = pd.read_csv(in_naicsRates, sep=',')              
#
## -- Read in zonal share of building sqft -- ##        
landUse = pd.read_csv(in_build_props, sep=',')            
#
intmod = pd.read_csv(imx, usecols=['zone','hcvWgt'], sep=',')
#          
#
## -- Read in subzone employment -- ##
goodCols = ['subzone_id','num_jobs_sector_11','num_jobs_sector_21','num_jobs_sector_22','num_jobs_sector_23',
    'num_jobs_sector_31','num_jobs_sector_42','num_jobs_sector_44','num_jobs_sector_48','num_jobs_sector_51','num_jobs_sector_52','num_jobs_sector_53',
    'num_jobs_sector_54','num_jobs_sector_55','num_jobs_sector_56','num_jobs_sector_61','num_jobs_sector_62','num_jobs_sector_71','num_jobs_sector_72',
    'num_jobs_sector_81','num_jobs_sector_92']
cmap = pd.read_csv(newFiles[0], usecols=goodCols, sep=',')	                ##-- CMAP 7-county subzones
cmap = cmap[cmap.subzone_id.le(cmapSubzone)]			                    ##-- ensure only CMAP 7-county subzones
print(" --> CMAP subzones: {0:,}".format(cmap.shape[0]))
print('     --> QC: CMAP subzone id is unique: {0}'.format(cmap.subzone_id.is_unique))
#
extern = pd.read_csv(newFiles[1], usecols=goodCols, sep=',')                ##-- external subzones
extern = extern[extern.subzone_id.gt(cmapSubzone)]				            ##-- Ensure only external subzones
print(" --> External subzones: {0:,}".format(extern.shape[0]))
print('     --> QC: External subzone id is unique: {0}'.format(extern.subzone_id.is_unique))
sz = pd.concat([cmap,extern], ignore_index=True, sort=False)	
print(" --> Total subzones: {0:,}".format(sz.shape[0]))
print('     --> QC: subzone id is unique: {0}'.format(sz.subzone_id.is_unique))

#
# ----------------------------------------------------------------------------
#  Calculate allocation weights for internal and external area. 
# ----------------------------------------------------------------------------  
emp = pd.melt(sz, id_vars='subzone_id')
emp['NAICS'] = emp['variable'].str[-2:].astype(int)      ##-- extract NAICS code

# Merge subzones with zone crosswalk
internal = emp.merge(geo, how='left', on='subzone_id', copy=False)

# Aggregate employment by NAICS to zone level
zn_internal=internal.groupby(['zone', 'NAICS']).agg({'value': 'sum'}).reset_index()

# Apply trips/employee rates by NAICS
naics_emp = zn_internal.merge(naicsRates, how='left', on=['NAICS'])
naics_emp['hcvWgt_1']=naics_emp['trips_emp']*naics_emp['value'].round(4)

# Apply land use adjustment 
naics_emp=naics_emp.merge(landUse, how='left', left_on='zone', right_on='zone17')
naics_emp['propArea'] = np.where(naics_emp['propArea'].isnull(), naics_emp['propArea'].min(), naics_emp['propArea'])
naics_emp['hcvWgt_o']=naics_emp['hcvWgt_1']*naics_emp['propArea']
naics_emp=naics_emp[['zone', 'NAICS', 'value', 'hcvWgt_o']].copy()

# Find shares of developed weights, and apply to total weight to find adjusted weight
naics_emp['total'] = naics_emp['hcvWgt_o'].sum()
naics_emp['prop'] = naics_emp['hcvWgt_o']/naics_emp['total']
naics_emp['hcvWgt'] = naics_emp['prop'] *total_wgt

zn_wt = naics_emp[['zone', 'hcvWgt']].copy()
zn_wt = zn_wt.groupby(['zone']).agg({'hcvWgt': 'sum'}).round(4).reset_index()

# ----------------------------------------------------------------------------
#  Create allocation weight file.
# ----------------------------------------------------------------------------  
## -- Add estimate of truck trips for intermodal facilities since building sqft won't quite cover these -- ##
a = pd.concat([intmod, zn_wt])
hcv = a.groupby(['zone']).agg({'hcvWgt': 'sum'}).round(4).reset_index()
#
## -- Create a template with all zones -- ##
geo.drop_duplicates(['zone'], inplace=True) 
geo1 = geo.merge(hcv, how='left', on='zone', copy=False)
geo1['hcvWgt'] = geo1['hcvWgt'].fillna(0)	
print(" --> Zones: {0}".format(geo1.shape[0]))
#
hcv0 = geo1[geo1['hcvWgt'] == 0] 
print(" --> Zones with zero HCV weights: {0}".format(hcv0.shape[0]))
hcv2 = geo1[geo1['hcvWgt'] > 0] 
print(" --> Zones with non-zero HCV weights: {0}".format(hcv2.shape[0]))
print(" --> Zonal HCV weights: HCV Minimum = {0:.2f}, HCV Maximum = {1:.2f}, HCV Mean = {2:.2f}, HCV Median = {3:.2f}".format(geo1['hcvWgt'].min(), geo1['hcvWgt'].max(),
        geo1['hcvWgt'].mean(), geo1['hcvWgt'].median())) 

geo1['a1'] = 'all:'
geo1.sort_values(by=['zone'], inplace=True)

f = open(mo20,'w')
print("t matrices \nd matrix=mo20 \na matrix=mo20 htrkseed 0 heavy truck allocation seed matrix", file=f) 
f.close()
geo1.to_csv(mo20, columns=['zone','a1','hcvWgt'], sep=' ', header=False, index=False, quoting=csv.QUOTE_NONE, escapechar=' ', mode='a')
