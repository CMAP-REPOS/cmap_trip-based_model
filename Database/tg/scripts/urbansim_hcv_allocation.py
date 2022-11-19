'''
#####################################################################################
URBANSIM_HCV_ALLOCATION.PY
  Craig Heither, rev. 03-28-2022

    Script reads the scenario buildings file from UrbanSim and uses it to develop 
	allocation weights for heavy commercial vehicle trips. For the external area, 
    subzone employment by NAICS is used (along with sqft per worker and truck trip
    rates) to develop heavy truck trips. A file of estimated 2018 truck trips from 
    rail intermodal facilities is also included to address a gap in the methodology.
    This ensures the allocation is consistent with the forecast growth in HCV-dependent
    land uses.

#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os, pandas as pd, csv
import fnmatch																##-- filter files in directory

cmapSubzone = 16426															##-- maximum internal CMAP subzone 

                                                                            
# ----------------------------------------------------------------------------
#  Input files.
# ----------------------------------------------------------------------------  
USpth ="..\\UrbanSim_inputs"
## -- create a list of the UrbanSim files to be processed -- ##
d1 = fnmatch.filter(os.listdir(USpth), '*buildings*')                           ##-- 7 county
d2 = fnmatch.filter(os.listdir(USpth), '*xsubzonetm*')                          ##-- external area
dirListing = d1 + d2
newFiles = []
for item in dirListing:
    newFiles.append(USpth+"\\"+item)
#
geog = "..\\fortran\\GEOG_IN.TXT"
#
rates = "..\\..\\data\\hcv_tg_rates.txt"                                       ##-- heavy truck trip rates
corresp = "..\\..\\data\\hcv_building_naics_corresp.csv"                       ##-- NAICS-building type correspondence
jobsqft = "..\\..\\data\\hcv_sqft_per_job.csv"                                 ##-- average square feet per job
imx = "..\\..\\data\\hcv_intermodal.csv"                                       ##-- estimate of 2018 truck trips for intermodal facilities


# ----------------------------------------------------------------------------
#  Output files.
# ----------------------------------------------------------------------------  
chk2 = "..\\..\\data\\chk2.csv"  
mo20 = "..\\..\\data\\mo20.txt"

print("Building files: {0}".format(newFiles))


# ----------------------------------------------------------------------------
#  Calculate allocation weights for CMAP 7 counties.
# ----------------------------------------------------------------------------  
## -- Start with CMAP 7-county buildings -- ##
bldg1 = pd.read_csv(newFiles[0], sep=',')									##-- CMAP buildings
print(" --> CMAP Buildings: {0}".format(bldg1.shape[0]))
test = bldg1[bldg1['subzone_id'] < 5] 
##test.to_csv(chk1,index=False)

## -- Read in GEOG_IN to attach Zones -- ##
geo = pd.read_csv(geog, sep=',', header=None, usecols=[0, 5])
geo.columns=['subzone_id','zone']
print(" --> GEOG_IN subzone summary Rows: {0}".format(geo.shape[0]))

## -- Read in truck trip rates file -- ##
rate = pd.read_csv(rates, sep=',')
rate.to_csv(chk2,index=False)
bldg1 = bldg1.merge(geo, how='left', on='subzone_id', copy=False)
bldg1 = bldg1.merge(rate, how='left', on='building_type_id', copy=False)
#
## -- Calculate HCV allocation weights -- ##
bldg1['hcvWgt'] = bldg1['non_residential_sqft'] * bldg1['hcv_rate']  / 1000     ##-- rates per 1000 sqft
hcv1 = bldg1.groupby(['zone']).agg({'hcvWgt': 'sum'}).round(4).reset_index()


# ----------------------------------------------------------------------------
#  Calculate allocation weights for external area.
# ----------------------------------------------------------------------------  
extern = pd.read_csv(newFiles[1], usecols=['subzone_id','num_jobs_sector_11','num_jobs_sector_21','num_jobs_sector_22','num_jobs_sector_23',
    'num_jobs_sector_31','num_jobs_sector_42','num_jobs_sector_44','num_jobs_sector_48','num_jobs_sector_51','num_jobs_sector_52','num_jobs_sector_53',
    'num_jobs_sector_54','num_jobs_sector_55','num_jobs_sector_56','num_jobs_sector_61','num_jobs_sector_62','num_jobs_sector_71','num_jobs_sector_72',
    'num_jobs_sector_81','num_jobs_sector_92'], sep=',')									##-- external subzones
ext1 = extern[extern['subzone_id'] > cmapSubzone]                              ##-- Ensure only external subzones
print(" --> External subzone summary Rows: {0}".format(ext1.shape[0]))
## -- Convert from wide to long -- ##
external = pd.melt(ext1, id_vars='subzone_id')
external['NAICS1'] = external['variable'].str[-2:].astype(int)                 ##-- extract NAICS code
#
## -- Attach likely building types to NAICS employment (Cartesian join)-- ##
bldgType = pd.read_csv(corresp, usecols=['building_type_id', 'NAICS'], sep=',')
externalCart = external.merge(bldgType, how='cross')
external = externalCart[externalCart['NAICS1'] == externalCart['NAICS']]       ##-- keep only true NAICS matches
print(" --> External subzone summary Rows after building types attached: {0}".format(external.shape[0]))
#
## -- Calculate square feet by industry for each subzone -- ## 
jbsqft = pd.read_csv(jobsqft, usecols=['building_type_id', 'building_sqft_per_job'], sep=',')
external = external.merge(jbsqft, how='left', on='building_type_id', copy=False)
external['non_residential_sqft'] = external['building_sqft_per_job'] * external['value']
#
## -- Use NAICS-building type correspondence file to determine average truck trip rate per industry -- ##
external = external.merge(rate, how='left', on='building_type_id', copy=False)
external['hcvWgt'] = external['non_residential_sqft'] * external['hcv_rate']  / 1000     ##-- rates per 1000 sqft
#
## -- We do not know what the actual building types are so use the average trip rate per subzone-NAICS combination to estimate -- ##
hcv2a = external.groupby(['subzone_id','NAICS']).agg({'hcvWgt': 'mean'}).round(4).reset_index()
#
## -- Now calculate zonal values -- ##
hcv2a = hcv2a.merge(geo, how='left', on='subzone_id', copy=False)
hcv2 = hcv2a.groupby(['zone']).agg({'hcvWgt': 'sum'}).round(4).reset_index()


# ----------------------------------------------------------------------------
#  Create allocation weight file.
# ----------------------------------------------------------------------------  
## -- Add estimate of truck trips for intermodal facilities since building sqft won't quite cover these -- ##
intmod = pd.read_csv(imx, usecols=['zone','hcvWgt'], sep=',')
a = pd.concat([hcv1,hcv2,intmod])
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
##geo1.to_csv(chk2,index=False)

f = open(mo20,'w')
print("t matrices \nd matrix=mo20 \na matrix=mo20 htrkseed 0 heavy truck allocation seed matrix", file=f) 
f.close()
geo1.to_csv(mo20, columns=['zone','a1','hcvWgt'], sep=' ', header=False, index=False, quoting=csv.QUOTE_NONE, escapechar=' ', mode='a')
