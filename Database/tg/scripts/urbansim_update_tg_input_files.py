'''
#####################################################################################
URBANSIM_UPDATE_TG_INPUT_FILES.PY
  Craig Heither, rev. 01-11-2022

    Script reads output files from UrbanSim and uses the data to create new versions 
	of ATTR_IN.TXT, HH_IN.TXT and POPSYN_HH.CSV that the Trip Generation model will  
	use.

#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import os, pandas as pd
import zipfile																##-- to write zip objects
import fnmatch																##-- filter files in directory

cmapSubzone = 16426															##-- maximum internal CMAP subzone 
dollar2010 = 0.855															##-- CPI value to convert 2019 dollars from UrbanSim to 2010 [https://www.bls.gov/data/inflation_calculator.htm]
                                                                            ##-- (not used for new trip-based model)
                                                                            
# ----------------------------------------------------------------------------
#  Input files.
# ----------------------------------------------------------------------------  
USpth ="..\\UrbanSim_inputs"
rptFile =USpth + "\\UrbansSim_file_report.TXT"
#
pth2 = "..\\fortran"
attr = pth2 + "\\ATTR_IN.TXT"
hh = pth2 + "\\HH_IN.TXT"
popsyn = pth2 + "\\POPSYN_HH.CSV"
noHH = pth2 + "\\persons_missing_HHID_CMAP.CSV"
noHH2 = pth2 + "\\persons_missing_HHID_External.CSV"
noHH3 = pth2 + "\\persons_missing_HHID.CSV"
# -- files renamed
attr_r = pth2 + "\\ATTR_INr.TXT"
hh_r = pth2 + "\\HH_INr.TXT"
popsyn_r = pth2 + "\\POPSYN_HHr.CSV"
#
geog  = pth2 + "\\GEOG_IN.TXT"
#
pth3 = "..\\fortran\\wfhmodule\\"
hhZip = pth3 + "\\synthetic_households.zip"
temp1 = pth3 + "synthetic_households.csv"
perZip = pth3 + "\\synthetic_persons.zip"
temp2 = pth3 + "synthetic_persons.csv"


# ----------------------------------------------------------------------------
#  Get the UrbanSim input file names.
# ----------------------------------------------------------------------------  
## -- create a list of the UrbanSim files to be processed -- ##
d1 = fnmatch.filter(os.listdir(USpth), '*hhtm*')
d2 = fnmatch.filter(os.listdir(USpth), '*persons*')
d3 = fnmatch.filter(os.listdir(USpth), '*subzone*')
dirListing = d1 + d2 + d3

if os.path.exists(rptFile):
	os.remove(rptFile)
newFiles = []
for item in dirListing:
    newFiles.append(USpth+"\\"+item)

print("{0} {1} {2} {3} {4} {5} \n".format(newFiles[0], newFiles[1], newFiles[2], newFiles[3], newFiles[4], newFiles[5]))


# ----------------------------------------------------------------------------
#  Rename existing TG files prior to update.
# ----------------------------------------------------------------------------  
origFiles = (attr, hh, popsyn)
renamedFiles = (attr_r, hh_r, popsyn_r)
x = 0
for f in origFiles:
	if os.path.exists(renamedFiles[x]):
		os.remove(renamedFiles[x])
	os.rename(origFiles[x],renamedFiles[x])
	x += 1

if os.path.exists(hhZip):
	os.remove(hhZip)
if os.path.exists(perZip):
	os.remove(perZip)
	
    
# ----------------------------------------------------------------------------
#  Read in the UrbanSim household files.
# ----------------------------------------------------------------------------  
## -- HH files are 0 & 1, Person files are 2 & 3, Subzone files are 4 & 5 
hh1 = pd.read_csv(newFiles[0], sep=',')										##-- CMAP households
print(" --> CMAP Households: {0}".format(hh1.shape[0]))
hh2=pd.read_csv(newFiles[1], sep=',')										##-- external households
print(" --> External Households: {0}".format(hh2.shape[0]))
hhs = pd.concat([hh1,hh2],ignore_index=True,sort=False)						##-- concatenate the household files 
print(" --> Total Households: {0}".format(hhs.shape[0]))
##hhs['income_2019_usd'] = hhs['income_2019_usd'] * dollar2010				##-- For Current set up, convert 2019 dollars to 2010 - commented out for new TBM
hhs['income_2019_usd'] = round(hhs['income_2019_usd'],2)

## -- Read in GEOG_IN to fill in PUMA nd RowCol -- ##
geo = pd.read_csv(geog, sep=',', header=None)
geo.columns=['subzone_id','county','cntyname','state','stpuma5','zone','chicago','cbd','rowcol','area','cmap']	##-- updated for new TBM FY22
geoSumry = geo.groupby('subzone_id')[['rowcol']].mean()						##-- get stpuma5 and rowcol to attach to UrbanSim data
print(" --> GEOG_IN subzone summary Rows: {0}".format(geoSumry.shape[0]))
## -- Merge files -- ##
hhsData = hhs.merge(geoSumry, how='left', on='subzone_id', copy=False)
hhsData['rowcol'] = hhsData['rowcol'].astype(int) 
hhsData['adults'] = hhsData['adults'].astype(int) 
hhsData['adult_workers'] = hhsData['adult_workers'].astype(int) 
hhsData['children_15_under'] = hhsData['children_15_under'].astype(int)
hhsData.sort_values(by=['subzone_id', 'hhtype'], inplace=True)              ##-- Trip Generation model Fortran code expects this sort order
hhsData.to_csv(popsyn, columns=['subzone_id','hhtype','cars','serialno','puma_id','rowcol','adults','adult_workers','children_15_under','income_category','age_of_head_category','hhvtype','income_2019_usd'], header=False, index=False)	

## -- Create synthetic HH file ... -- ##
hhsData['HH_id'] = hhsData.reset_index().index + 1							##-- index location in POPSYN_HH 
hhsData['HINCP19'] = hhsData['income_2019_usd']						
ids = hhsData.filter(['household_id','HH_id'], axis=1)						##-- save household_id & HH_id to link to person file
hhsData['household_id'] = hhsData['HH_id']									##-- reset household_id to reflect index position in POPSYN_HH
hhsData['SERIALNO'] = hhsData['serialno']	
hhsData.to_csv(temp1, columns=['household_id','HINCP19','SERIALNO'], header=True, index=False)
## -- ... and write zipped file into WFH module. -- ##
with zipfile.ZipFile(hhZip, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(temp1, os.path.basename(temp1))
if os.path.exists(temp1):
	os.remove(temp1)

## -- Create synthetic Person file ... -- ##
per1 = pd.read_csv(newFiles[2], sep=',')	                               ##-- CMAP persons
per1.drop(['Unnamed: 0'], axis=1, inplace=True)
print(" --> CMAP Persons: {0}".format(per1.shape[0]))
nullDf = per1[per1['household_id'].isnull()]
nullDf.to_csv(noHH, index=False)

per2 = pd.read_csv(newFiles[3], sep=',')	                               ##-- external persons
per2.drop(['Unnamed: 0'], axis=1, inplace=True)
print(" --> External Persons: {0}".format(per2.shape[0]))
nullDf = per2[per2['household_id'].isnull()]
nullDf.to_csv(noHH2, index=False)

pers = pd.concat([per1,per2],ignore_index=True,sort=False)					##-- concatenate the person files 
print(" --> Total Persons: {0}".format(pers.shape[0]))
persData = pers.merge(ids, how='left', on='household_id', copy=False)		##-- attach HH_id
nullDf = persData[persData['HH_id'].isnull()]
nullDf.to_csv(noHH3, index=False)


persData['household_id'] = persData['HH_id']
persData['per_num'] = persData['member_id']
persData['SEX'] = persData['sex']
persData['AGEP'] = persData['age']
persData['SCHL'] = persData['education']
persData['RAC1P'] = persData['race_id']

persData['INDP'] = persData['INDP'].fillna(169)								##-- replace Nulls with 169
persData.loc[persData.JWTR.isnull(),'JWTR'] = 'bb'
persData.loc[persData.ESR.isnull(),'ESR'] = 'b'
persData.loc[persData.SCHL.isnull(),'SCHL'] = 'bb'
persData['INDP'] = persData['INDP'].astype(int)

persData = persData[~persData.household_id.isnull()]

persData.sort_values(by=['household_id','per_num'], inplace=True)
print(" --> Total Persons with indexed HH id: {0}".format(persData.shape[0]))
persData.to_csv(temp2, columns=['household_id','per_num','JWTR','INDP','ESR','SEX','AGEP','SCHL','RAC1P'], header=True, index=False)
## -- ... and write zipped file into WFH module. -- ##
with zipfile.ZipFile(perZip, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(temp2, os.path.basename(temp2))
if os.path.exists(temp2):
	os.remove(temp2)


# ----------------------------------------------------------------------------
#  Read in the subzone files.
# ---------------------------------------------------------------------------- 
sz1 = pd.read_csv(newFiles[4], sep=',')										##-- CMAP subzones
sz1 = sz1[sz1.subzone_id.le(cmapSubzone)]									##-- limit to internal CMAP households
print(" --> CMAP Subzones: {0}".format(sz1.shape[0]))
sz2=pd.read_csv(newFiles[5], sep=',')										##-- external subzones
print(" --> External Subzones: {0}".format(sz2.shape[0]))

## -- External subzones do not contain household type distribution so add Zeroes -- ##
l = ['building_type_1_hhs','building_type_2_hhs','building_type_3_hhs','building_type_4_hhs','building_type_5_hhs','building_type_6_hhs','building_type_7_hhs',
'building_type_8_hhs','building_type_9_hhs','building_type_10_hhs',]
for col in l:
	sz2[col] = 0

szs = pd.concat([sz1,sz2],ignore_index=True,sort=False)						##-- concatenate the subzone files 
print(" --> Total Subzones: {0}".format(szs.shape[0]))
## -- Read in original ATTR_IN -- ##
jobs = pd.read_csv(attr_r, sep=',', header=None, usecols=[0, 3])	
jobs.columns=['subzone_id','highEarn']
## -- Read in original HH_IN -- ##
test = pd.read_csv(hh_r, sep=',', header=None)								##-- read file in to check if it is in the old or new format
if len(test.columns)==9:
	szhh = pd.read_csv(hh_r, sep=',', header=None, usecols=[0, 7, 8])		##-- old format
else:
	szhh = pd.read_csv(hh_r, sep=',', header=None, usecols=[0, 34, 35])		##-- new format
szhh.columns=['subzone_id','commuteShare','pef']
## -- Merge files -- ##
szData = szs.merge(jobs, how='left', on='subzone_id', copy=False)
szData = szData.merge(szhh, how='left', on='subzone_id', copy=False)
szData.sort_values(by=['subzone_id'], inplace=True)
szData['jobs_retail_44_45'] = szData['jobs_retail_44_45'].round().astype(int)
szData['total_households'] = szData['total_households'].round().astype(int)
szData['total_jobs'] = szData['total_jobs'].round().astype(int)
szData['highEarn'] = szData['highEarn'].round(3)
szData['pef'] = szData['pef'].round(2)
szData.to_csv(attr, columns=['subzone_id','jobs_retail_44_45','total_jobs','highEarn'], header=False, index=False)
szData.to_csv(hh, columns=['subzone_id','total_households','total_adults','total_adult_workers','total_children_15_under','households_income_30k_less','income_category2_hhs','income_category3_hhs',
'income_category4_hhs','households_head_35_less','age_householder_category2_hhs','age_householder_category3_hhs',
'households_size1','households_size2','households_size3','households_size4','households_size5','households_size6','households_size7','building_type_1_hhs','building_type_2_hhs',
'building_type_3_hhs','building_type_4_hhs','building_type_5_hhs','building_type_6_hhs','building_type_7_hhs','building_type_8_hhs','building_type_9_hhs','building_type_10_hhs',
'race_ethnicity_category1_hhs','race_ethnicity_category2_hhs','race_ethnicity_category3_hhs','race_ethnicity_category4_hhs','race_ethnicity_category5_hhs','commuteShare','pef'], header=False, index=False)

print("Households: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}, Subzone Mean = {2:.1f}".format(szData['total_households'].min(), szData['total_households'].max(),
    szData['total_households'].mean())) 
print("Total Employment: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}, Subzone Mean = {2:.1f}".format(szData['total_jobs'].min(), szData['total_jobs'].max(),
    szData['total_jobs'].mean()))     
print("Retail Employment: Subzone Minimum = {0:.1f}, Subzone Maximum = {1:.1f}, Subzone Mean = {2:.1f}".format(szData['jobs_retail_44_45'].min(), szData['jobs_retail_44_45'].max(),
    szData['jobs_retail_44_45'].mean()))      
    
x = 0
for f in renamedFiles:
	if os.path.exists(renamedFiles[x]):
		os.remove(renamedFiles[x])
	x += 1


# ----------------------------------------------------------------------------
#  Write report file.
# ---------------------------------------------------------------------------- 
f = open(rptFile,'w')
print("Subzone File: Subzone Minimum = {0}, Subzone Maximum = {1}".format(szData['subzone_id'].min(), szData['subzone_id'].max()), file=f) 
print("Household File: Subzone Minimum = {0}, Subzone Maximum = {1}".format(hhsData['subzone_id'].min(), hhsData['subzone_id'].max()), file=f) 
print("Household File: Puma Minimum = {0}, Puma Maximum = {1}".format(hhsData['puma_id'].min(), hhsData['puma_id'].max()), file=f) 
print("Household File: Rowcol Minimum = {0}, Rowcol Maximum = {1}".format(hhsData['rowcol'].min(), hhsData['rowcol'].max()), file=f) 
f.close()
