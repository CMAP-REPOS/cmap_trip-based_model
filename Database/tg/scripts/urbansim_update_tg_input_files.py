'''
########################################################################################################
URBANSIM_UPDATE_TG_INPUT_FILES.PY
  Craig Heither, rev. 01-31-2026

    Script reads output files from UrbanSim and uses the data to create new versions 
	of ATTR_IN.TXT, HH_IN.TXT, GQ_IN.TXT and POPSYN_HH.CSV that the Trip Generation 
	model will use.

	Additonal data cleanup steps for 2026 LRTP:
	 - if necessary, set household_id to unique values across the CMAP & external files
	 - if necessary, remove persons from the files that are not in households in the household files
	 - if necessary, updates hhtype and hhvtype for households with no report_fileadults
	 
########################################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os
import pandas as pd
import numpy as np
import zipfile										##-- to write zip objects
import fnmatch										##-- filter files in directory
import yaml
import math
from pathlib import Path

with open(Path.cwd().parent.parent.joinpath('batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)

puma = config['pumaVersion'] 
print("PUMA version: {0}".format(puma))

cmapSubzone = 16426									##-- maximum internal CMAP subzone 
                                                                            
# ----------------------------------------------------------------------------
#  Input files.
# ----------------------------------------------------------------------------  
UrbanSim_path = Path(__file__).resolve().parents[1].joinpath('UrbanSim_inputs')
#
fortran_path = Path(__file__).resolve().parents[1].joinpath('fortran')
hhtypeCat_file = fortran_path / 'tg_hhtype_lookup.csv'
hhvtypeCat_file = fortran_path / 'tg_hhvtype_lookup.csv'
geog_file  = fortran_path / 'GEOG_IN.TXT'
# -- files renamed
attr_r = fortran_path / 'ATTR_INr.TXT'
hh_r = fortran_path / 'HH_INr.TXT'
popsyn_r = fortran_path / 'POPSYN_HHr.CSV'

# ----------------------------------------------------------------------------
#  Output files.
# ---------------------------------------------------------------------------- 
noHHID_file = fortran_path / 'persons_missing_HHID_CMAP.csv'
noHHIDext_file = fortran_path / 'persons_missing_HHID_External.csv'
noHHIDany_file = fortran_path / 'persons_missing_HHID.csv'
noHHTYPE_file = fortran_path / 'missing_hhtype.csv'
attr_file = fortran_path / 'ATTR_IN.TXT'
hh_file = fortran_path / 'HH_IN.TXT'
gq_file = fortran_path / 'GQ_IN.TXT'
popsyn_file = fortran_path / 'POPSYN_HH.CSV'
#
wfh_path = Path(__file__).resolve().parents[1].joinpath('fortran','wfhmodule')
hhZip_file = wfh_path / 'synthetic_households.zip'
temp1_file = wfh_path / 'synthetic_households.csv'
perZip_file = wfh_path / 'synthetic_persons.zip'
temp2_file = wfh_path / 'synthetic_persons.csv'
#
report_file = UrbanSim_path / 'UrbansSim_file_report.txt'

# ----------------------------------------------------------------------------
#  Get the UrbanSim input file names.
# ----------------------------------------------------------------------------  
## -- create a list of the UrbanSim files to be processed -- ##
d1 = fnmatch.filter(os.listdir(UrbanSim_path), '*hhtm*')
d2 = fnmatch.filter(os.listdir(UrbanSim_path), '*persons*')
d3 = fnmatch.filter(os.listdir(UrbanSim_path), '*subzone*')
dirListing = d1 + d2 + d3

if os.path.exists(report_file):
	os.remove(report_file)
newFiles = []
for item in dirListing:
    newFiles.append(UrbanSim_path / item)

print("{0} \n{1} \n{2} \n{3} \n{4} \n{5} \n".format(newFiles[0], newFiles[1], newFiles[2],
										  newFiles[3], newFiles[4], newFiles[5]))

# ----------------------------------------------------------------------------
#  Rename existing TG files prior to update.
# ----------------------------------------------------------------------------  
origFiles = (attr_file, hh_file, popsyn_file)
renamedFiles = (attr_r, hh_r, popsyn_r)
x = 0
for f in origFiles:
	if os.path.exists(renamedFiles[x]):
		os.remove(renamedFiles[x])
	os.rename(origFiles[x],renamedFiles[x])
	x += 1

if os.path.exists(hhZip_file):
	os.remove(hhZip_file)
if os.path.exists(perZip_file):
	os.remove(perZip_file)
	
# ----------------------------------------------------------------------------
#  Read in the UrbanSim household files.
# ----------------------------------------------------------------------------  
## -- HH files are 0 & 1, Person files are 2 & 3, Subzone files are 4 & 5 
hh1 = pd.read_csv(newFiles[0], sep=',')							##-- CMAP households
##hh1 = hh1[hh1.subzone_id.le(cmapSubzone)]						##-- limit to CMAP 7-county subzones
print(" --> CMAP Households: {0:,}".format(hh1.shape[0]))
print('  --> QC: Household id is unique: {0}'.format(hh1.household_id.is_unique))
print('  --> QC: CMAP household_id Minimum = {0:,}, Maximum = {1:,}'.format(
							hh1['household_id'].min(), hh1['household_id'].max()))
print('  --> QC: CMAP subzone_ID Minimum = {0:,}, Maximum = {1:,}'.format(
							hh1['subzone_id'].min(), hh1['subzone_id'].max()))
#
hh2=pd.read_csv(newFiles[1], sep=',')							##-- external households
##hh2 = hh2[hh2.subzone_id.gt(cmapSubzone)]						##-- limit to external subzones
print(" --> External Households: {0:,}".format(hh2.shape[0]))
print('  --> QC: Household id is unique: {0}'.format(hh2.household_id.is_unique))
print('  --> QC: External household_id Minimum = {0:,}, Maximum = {1:,}'.format(
								hh2['household_id'].min(), hh2['household_id'].max()))
print('  --> QC: External subzone_ID Minimum = {0:,}, Maximum = {1:,}'.format(
								hh2['subzone_id'].min(), hh2['subzone_id'].max()))

## -- Read in Person files and remove persons not in households in the modeling area -- ##
per1 = pd.read_csv(newFiles[2], sep=',')	                     ##-- CMAP persons
print(" --> CMAP Persons in households: {0:,}".format(per1.shape[0]))
inRegion = hh1[['household_id','subzone_id']].copy()
per1 = per1.merge(inRegion, how='left', on='household_id', copy=False)
##per1 = per1[per1.subzone_id.gt(0)]
print('  --> QC: CMAP household_id Minimum = {0:,}, Maximum = {1:,}'.format(
								per1['household_id'].min(), per1['household_id'].max()))
print('  --> QC: CMAP subzone_ID Minimum = {0:,}, Maximum = {1:,}'.format(
								per1['subzone_id'].min(), per1['subzone_id'].max()))
print(" --> Confirmed CMAP Persons in households: {0:,}".format(per1.shape[0]))
#
per2 = pd.read_csv(newFiles[3], sep=',')	                      ##-- external persons
print(" --> External Persons in households: {0:,}".format(per2.shape[0]))
inExternal = hh2[['household_id','subzone_id']].copy()
per2 = per2.merge(inExternal, how='left', on='household_id', copy=False)
##per2 = per2[per2.subzone_id.gt(0)]
print('  --> QC: External household_id Minimum = {0:,}, Maximum = {1:,}'.format(
								per2['household_id'].min(), per2['household_id'].max()))
print('  --> QC: External subzone_ID Minimum = {0:,}, Maximum = {1:,}'.format(
								per2['subzone_id'].min(), per2['subzone_id'].max()))
print(" --> Confirmed External Persons in households: {0:,}".format(per2.shape[0]))

##
## -- If needed, implement adjustment so all Household_ids are unique -- ##
maxCmapHH = hh1['household_id'].max()
minExternalHH = hh2['household_id'].min()
print('  --> QC: Maximum CMAP Household id: {0:,}, Minimum external Household id: {1:,}'.format(
								maxCmapHH, minExternalHH))

if minExternalHH < maxCmapHH:
	print('  --> FIXING_ISSUE: Implementing adjustment to external files so all Household_ids are unique')
	hh2['household_id'] = hh2['household_id'] + maxCmapHH 
	per2['household_id'] = per2['household_id'] + maxCmapHH 
	print('  --> FIXING_ISSUE: Done')

#
## -- Finish processing household files -- ##
hhs = pd.concat([hh1,hh2], ignore_index=True, sort=False)			##-- concatenate the household files
print('  --> QC: Household id is unique: {0}'.format(hhs.household_id.is_unique))
# Trip Generation model Fortran code expects this sort order 
hhs.sort_values(by=['subzone_id', 'hhtype'], ignore_index=True, inplace=True) 
# index location in POPSYN_HH (new HH_ID based on row number)
hhs['HH_id'] = hhs.index + 1						
#---------------------------------------------------------------------------------------------------#
### --- Missing hhtype and hhvtype --- ###
# The TBM trip generation model requires households to have at least one adult (age 16+).
# In some PUMS records, the head of household is 15. This code block corrects the issue for the TBM.
nullHHTYPE = hhs[hhs['hhtype'].isnull()]
if nullHHTYPE.shape[0] > 0:
	nullHHTYPE.to_csv(noHHTYPE_file, index=False)			##-- file of households where hhtype & hhvtype is corrected 
### --- Adjust household types -- ###
hhs.loc[hhs['adults'] == 0, 'children_15_under'] = hhs['children_15_under'] - 1	##-- reclassify one child as an adult
hhs.loc[hhs['adults'] == 0, 'adults'] = 1
### --- Update hhtype --- ###
hhtype = pd.read_csv(hhtypeCat_file, sep=',')						##-- hhtype definitions
### --- set category values --- ###
hhs['ADULT'] = hhs['adults']
hhs.loc[hhs['adults'] > 4, 'ADULT'] = 4
hhs['WORKER'] = hhs['adult_workers']
hhs.loc[hhs['adult_workers'] > 3, 'WORKER'] = 3
hhs['CHILD'] = hhs['children_15_under']
hhs.loc[hhs['children_15_under'] > 3, 'CHILD'] = 3
hhs['INCOME'] = hhs['income_category']
hhs['AGE_INDEX'] = hhs['age_of_head_category']
hhs['VEH'] = hhs['cars']
hhs.loc[hhs['cars'] > 3, 'VEH'] = 3
#
hhs = hhs.merge(hhtype, how='left', on=['ADULT','WORKER','CHILD','AGE_INDEX','INCOME'], copy=False)
hhs.loc[hhs['hhtype'].isnull(), 'hhtype'] = hhs['HHTYPE']
### --- Update hhtype --- ###
hhvtype = pd.read_csv(hhvtypeCat_file, sep=',')									##-- hhvtype definitions
hhs = hhs.merge(hhvtype, how='left', on=['ADULT','WORKER','CHILD','AGE_INDEX','VEH'], copy=False)
hhs.loc[hhs['hhvtype'].isnull(), 'hhvtype'] = hhs['HHVTYPE']
hhs.drop(['ADULT','WORKER','CHILD','AGE_INDEX','INCOME','VEH','HHTYPE','HHVTYPE'], axis=1, inplace=True)
#---------------------------------------------------------------------------------------------------------------------#

### --- Convert SERIALNO to integers --- ###
hhs['serialno'] = hhs['serialno'].astype(str).str.replace('HU', '99', regex=False)
hhs['serialno'] = hhs['serialno'].astype(np.int64) 
print(" --> Total Households: {0:,}".format(hhs.shape[0]))
hhs['income_2019_usd'] = round(hhs['income_2019_usd'],2)

## -- Read in GEOG_IN to fill in RowCol -- ##
geo = pd.read_csv(geog_file, sep=',', header=None)
geo.columns=['subzone_id','county','cntyname','state','stpuma5','zone','chicago','cbd',
			 'rowcol','area','cmap']
# get stpuma5 and rowcol to attach to UrbanSim data
geoSumry = geo.groupby('subzone_id')[['rowcol']].mean()						
print(" --> GEOG_IN subzone summary Rows: {0:,}".format(geoSumry.shape[0]))
szTemplate = geo[['subzone_id']].copy()
## -- Merge files -- ##
hhsData = hhs.merge(geoSumry, how='left', on='subzone_id', copy=False)
hhsData['rowcol'] = hhsData['rowcol'].astype(int) 
hhsData['adults'] = hhsData['adults'].astype(int) 
hhsData['adult_workers'] = hhsData['adult_workers'].astype(int) 
hhsData['children_15_under'] = hhsData['children_15_under'].astype(int)

#urbansim data on or after 2026 has puma_id column
if 'puma_id' in hhsData.columns:
	hhsData['puma_id'] = hhsData['puma_id'].astype(int)
 
## -- Set 2010 PUMA values, if needed -- ##
if puma == 2010:
    geo.eval('puma_id_2010 = floor(county/1000) * 100000 + stpuma5', inplace=True)
    geo['puma_id_2010'] = geo['puma_id_2010'].astype(int) 
    geo2 = geo[['subzone_id', 'puma_id_2010']].copy()
    hhsData = hhsData.merge(geo2, how='left', on='subzone_id', copy=False)
    
    if 'puma_id' in hhsData.columns:
        puma_mismatch = hhsData.loc[hhsData['puma_id'].astype(int) != hhsData['puma_id_2010']] 
        if len(puma_mismatch>0):
            print('puma in urbansim input files is 2020. converting to 2010.')
            hhsData['puma_id'] = hhsData['puma_id_2010']
    else:
    	hhsData['puma_id'] = hhsData['puma_id_2010']

# Trip Generation model Fortran code expects this sort order
hhsData.sort_values(by=['HH_id'], inplace=True)              				
print('  --> QC: Household file contains null values: {0}'.format(hhsData.isnull().values.any()))
print('  --> QC: Household Adults Minimum = {0:,}, Maximum = {1:,}'.format(hhsData['adults'].min(),
																		   hhsData['adults'].max()))
print('  --> QC: Household Workers Minimum = {0:,}, Maximum = {1:,}'.format(hhsData['adult_workers'].min(),
																			hhsData['adult_workers'].max()))
print('  --> QC: Household Children Minimum = {0:,}, Maximum = {1:,}'.format(hhsData['children_15_under'].min(),
																			 hhsData['children_15_under'].max()))
print('  --> QC: Household type categories Minimum = {0:,}, Maximum = {1:,}'.format(
															hhsData['hhtype'].min(), hhsData['hhtype'].max()))
hhsData.to_csv(popsyn_file, columns=['subzone_id','hhtype','cars','serialno','puma_id','rowcol','adults',
									'adult_workers','children_15_under','income_category',
									'age_of_head_category','hhvtype','income_2019_usd'], header=False, index=False)	

## -- Create synthetic HH file ... -- ##
hhsData['HINCP19'] = hhsData['income_2019_usd']
# save household_id & HH_id to link to person file
ids = hhsData[['household_id','HH_id']].copy()
# reset household_id to reflect index position in POPSYN_HH
hhsData['household_id'] = hhsData['HH_id']						
hhsData['SERIALNO'] = hhsData['serialno']	
hhsData.to_csv(temp1_file , columns=['household_id','HINCP19','SERIALNO'], header=True, index=False)
## -- ... and write zipped file into WFH module. -- ##
with zipfile.ZipFile(hhZip_file , "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(temp1_file , os.path.basename(temp1_file ))
if os.path.exists(temp1_file ):
	os.remove(temp1_file )

## -- Create synthetic Person file ... -- ##
### --- person files already opened
nullDf = per1[per1['household_id'].isnull()]
if nullDf.shape[0] > 0:
	nullDf.to_csv(noHHID_file, index=False)
#
nullDf = per2[per2['household_id'].isnull()]
if nullDf.shape[0] > 0:
	nullDf.to_csv(noHHIDext_file, index=False)
#
# concatenate the person files 
pers = pd.concat([per1,per2],ignore_index=True,sort=False)					
print(" --> Total Persons in households: {0:,}".format(pers.shape[0]))
print('  --> QC: Household id is unique: {0}'.format(ids.household_id.is_unique))
# attach HH_id
persData = pers.merge(ids, how='left', on='household_id', copy=False)		
nullDf = persData[persData['HH_id'].isnull()]
if nullDf.shape[0] > 0:
	nullDf.to_csv(noHHIDany_file, index=False)
#
persData['household_id'] = persData['HH_id']
persData['per_num'] = persData['member_id']
persData['SEX'] = persData['sex']
persData['AGEP'] = persData['age']
persData['SCHL'] = persData['education']
persData['RAC1P'] = persData['race_id']
persData['INDP'] = persData['INDP'].fillna(169)	
persData.loc[persData.JWTR.isnull(),'JWTR'] = 'bb'
persData.loc[persData.ESR.isnull(),'ESR'] = 'b'
persData.loc[persData.SCHL.isnull(),'SCHL'] = 'bb'
persData['INDP'] = persData['INDP'].astype(int)
# change in schema post-2026, some of these no longer exist -- change to keeping some cols instead of dropping
# persData.drop(['person_id','member_id','age','hh_relationship','education','race_id','sex',
# 			   'hours','income','student_status','worker_status'], axis=1, inplace=True)
keepcols = [
    'household_id', 'per_num', 'JWTR', 
    'INDP', 'ESR', 'SEX', 'AGEP', 
    'SCHL', 'RAC1P'
]
persData = persData[keepcols].copy()

persData = persData[~persData.household_id.isnull()]
print('  --> QC: Person file contains null values: {0}'.format(persData.isnull().values.any()))
persData.sort_values(by=['household_id','per_num'], inplace=True)
print(" --> Total Persons with indexed HH id: {0:,}".format(persData.shape[0]))
persData.to_csv(temp2_file, columns=['household_id','per_num','JWTR','INDP','ESR','SEX','AGEP',
									 'SCHL','RAC1P'], header=True, index=False)
## -- ... and write zipped file into WFH module. -- ##
with zipfile.ZipFile(perZip_file, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(temp2_file, os.path.basename(temp2_file))
if os.path.exists(temp2_file):
	os.remove(temp2_file)

# ----------------------------------------------------------------------------
#  Read in the subzone files.
# ---------------------------------------------------------------------------- 
# CMAP subzones
sz1 = pd.read_csv(newFiles[4], sep=',')										
#sz1 = sz1[sz1.subzone_id.le(cmapSubzone)]						##-- limit to CMAP 7-county subzones
print(" --> CMAP Subzones with UrbanSim data: {0:,}".format(sz1.shape[0]))
# external subzones
sz2=pd.read_csv(newFiles[5], sep=',')										
#sz2 = sz2[sz2.subzone_id.gt(cmapSubzone)]						##-- limit to external subzones
print(" --> External Subzones with UrbanSim data: {0:,}".format(sz2.shape[0]))
subzs = pd.concat([sz1,sz2], ignore_index=True, sort=False)	
## -- Clean things up if external file has data in CMAP subzones -- ##
szsCols = subzs.columns.tolist()
szsCols.remove('subzone_id') 
szs = subzs.groupby('subzone_id')[szsCols].sum()	
#
print(" --> Subzones with UrbanSim data: {0:,}".format(szs.shape[0]))
# merge UrbanSim data to full template of subzones
szs = szTemplate.merge(szs, how='left', on='subzone_id', copy=False)		
print(" --> Total Subzones: {0:,}".format(szs.shape[0]))
print('  --> QC: Households in subzone file: {0:,}'.format(szs['total_households'].sum()))
## -- Read in original ATTR_IN to get highEarn -- ##
jobs = pd.read_csv(attr_r, sep=',', header=None, usecols=[0, 1])	
jobs.columns=['subzone_id','highEarn']
## -- Read in original HH_IN to get private auto commute share and sidewalk density-- ##
# read the HH_IN file in to determine its vintage
test = pd.read_csv(hh_r, sep=',', header=None)								
if len(test.columns) == 9:
	# ON TO 2050 plan HH_IN.TXT file format (pre-c22q2)
	szhh = pd.read_csv(hh_r, sep=',', header=None, usecols=[0, 7, 8])		
elif len(test.columns) == 36:
	# ON TO 2050 Plan Update HH_IN.TXT file format (begin c22q2)
	szhh = pd.read_csv(hh_r, sep=',', header=None, usecols=[0, 34, 35])		
else:
	# 2026 LRTP HH_IN.TXT file format (begin c26q2)	
	szhh = pd.read_csv(hh_r, sep=',', header=None, usecols=[0, 1, 2])		

szhh.columns=['subzone_id','commuteShare','pef']
## -- Merge files -- ##
szData = szs.merge(jobs, how='left', on='subzone_id', copy=False)
szData = szData.merge(szhh, how='left', on='subzone_id', copy=False)
szData.sort_values(by=['subzone_id'], inplace=True)
szData.fillna(0, inplace=True)
szData['jobs_retail_44_45'] = szData['jobs_retail_44_45'].round().astype(int)
szData['total_households'] = szData['total_households'].round().astype(int)
szData['total_jobs'] = szData['total_jobs'].round().astype(int)
szData['highEarn'] = szData['highEarn'].round(3)
szData['pef'] = szData['pef'].round(2)
szData.to_csv(attr_file, columns=['subzone_id','jobs_retail_44_45','total_jobs',
								  'highEarn'], header=False, index=False)
print(" --> {0:,} records written to {1}".format(szData.shape[0], attr_file))
#accommodate change in schema post-2026 -- fix "househols" typo to "households", if exists
szData.rename(columns={c: c.replace('househols','households') for c in szData.columns}, inplace=True)

szData.to_csv(hh_file, columns=['subzone_id','total_households','total_adults',
								'total_adult_workers','total_children_15_under',
								'households_income_30k_less','income_category2_hhs',
								'income_category3_hhs','income_category4_hhs',
								'households_head_35_less','age_householder_category2_hhs',
								'age_householder_category3_hhs','commuteShare','pef'],
								header=False, index=False)

## -- Group Quarters population -- ##
szData['total_mil'] = szData['total_mil'].astype(int)	
szData['total_uni'] = szData['total_uni'].astype(int)	
szData['total_non_inst_16_64'] = szData['total_non_inst_16_64'].astype(int)
szData['total_non_inst_65_plus'] = szData['total_non_inst_65_plus'].astype(int)
szData.to_csv(gq_file, columns=['subzone_id','total_mil','total_uni',
								'total_non_inst_16_64','total_non_inst_65_plus'],
								header=False, index=False)
print(" --> Group Quarters population")
print("     - In military barracks: {0:,}".format(szData['total_mil'].sum()))
print("     - In dormitories: {0:,}".format(szData['total_uni'].sum()))
print("     - In other non-institutionalized group quarters ages 16-64: {0:,}".format(
												szData['total_non_inst_16_64'].sum()))
print("     - In other non-institutionalized group quarters ages 65+: {0:,}".format(
												szData['total_non_inst_65_plus'].sum()))
print(" --> {0:,} records written to {1}".format(szData.shape[0], gq_file))
print("-----")
print("Households: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}, " \
		"Subzone Mean = {2:.1f}".format(szData['total_households'].min(),
								  szData['total_households'].max(),
								  szData['total_households'].mean())) 
print("Total Employment: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}," \
		"Subzone Mean = {2:.1f}".format(szData['total_jobs'].min(), 
								  szData['total_jobs'].max(),szData['total_jobs'].mean()))     
print("Retail Employment: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}," \
		"Subzone Mean = {2:.1f}".format(szData['jobs_retail_44_45'].min(),
								  szData['jobs_retail_44_45'].max(),
								  szData['jobs_retail_44_45'].mean()))      
    
x = 0
for f in renamedFiles:
	if os.path.exists(renamedFiles[x]):
		os.remove(renamedFiles[x])
	x += 1

# ----------------------------------------------------------------------------
#  Write report file.
# ---------------------------------------------------------------------------- 
f = open(report_file,'w')
print("Subzone File: Subzone Minimum = {0}, Subzone Maximum = {1}".format(
							szData['subzone_id'].min(), szData['subzone_id'].max()), file=f) 
print("Household File: Subzone Minimum = {0}, Subzone Maximum = {1}".format(
							hhsData['subzone_id'].min(), hhsData['subzone_id'].max()), file=f) 
print("Household File: Puma Minimum = {0}, Puma Maximum = {1}".format(
							hhsData['puma_id'].min(), hhsData['puma_id'].max()), file=f) 
print("Household File: Rowcol Minimum = {0}, Rowcol Maximum = {1}".format(
							hhsData['rowcol'].min(), hhsData['rowcol'].max()), file=f) 
print("QC: Total households: {0:,}".format(hhs.shape[0]), file=f) 
print("QC: Total people: {0:,}".format(persData.shape[0]), file=f) 
print("QC: Total jobs: {0:,}".format(szData['total_jobs'].sum()), file=f) 
print("QC: Household file contains null values: {0}".format(hhs.isnull().values.any()), file=f) 
print("QC: Household id is unique: {0}".format(hhs.household_id.is_unique), file=f) 
print("QC: Person file contains null values: {0}".format(persData.isnull().values.any()), file=f) 
f.close()