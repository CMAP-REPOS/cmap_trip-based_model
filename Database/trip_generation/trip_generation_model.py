"""
Filename: trip_generation_model.py
Author: Craig Heither
Description: This is the Python translation of CMAP's Trip Generation model,
             originally written by Ronald W. Eash. It creates the subzone-
             level trip productions and attractions by trip type used in the
             trip-based model. Trip generation has seven major steps:
                1. Load the input files
                2. Run the vehicle availability model to estimate the number
                    of vehicles for each synthetic household
                3. Run the household trip enumeration process to create 
                    productions for each synthetic household
                4. Calculate non-institutional group quarters trip productions
                5. Allocate trip productions and attractions for non-home trip
                    ends
                6. Factor external home-based productions and attractions
                7. Finalize productions and attractions
Input files:
    - POPSYN_HH.CSV: contains information about each synthetic household
    - GEOG_IN.TXT: defines various geographies for each zone and subzone
    - HH_IN.TXT: contains subzone totals of household and person level attributes
    - HH_WFH_STATUS.CSV: contains the telework status for each synthetic household
    - tg_hhvtype_lookup.csv: defines the attributes in household vehicle type categories
    - HHID_choices1.csv: defines the range of survey households available for trip
            enumeration for non-telework households based on synthetic household attributes
    - HHID_choices2.csv: lists the survey households in each group defined in HHID_choices1.csv
    - HHID_wfh1.csv: defines the range of survey households available for trip
            enumeration for telework households based on synthetic household attributes
    - HHID_wfh2.csv: lists the survey households in each group defined in HHID_wfh1.csv
    - HI_HHENUM_IN.TXT: contains travel survey households including the number of weekday
            household trips by purpose 
    - GQ_IN.TXT: contains the number of workers and nonworking adults in non-institutionalized
            group quarters for each subzone
    - GQ_TRIPRATES.csv: contains group quarters trip rates for non-work trips developed from
            the household travel survey 
    - airport_sz.csv: contains subzones representing airports
    - SCHOOL_IN.CSV: lists high school and college enrollment values for subzones containing
            these institutions
    - ATTR_IN.TXT: contains subzone-level employment information
    - coefficients_attractions.csv: contains the trip type coefficients used to allocate
            attractions
    - coefficients_nonhome_productions.csv: contains the trip type coefficients used to
            allocate non-home productions
    - EXT_IN.TXT: PUMA-level adjustment factors to modify workplace trips from internal
            households to external workplaces and vice-versa
Output files:
    - SIMULATED_HHVEH.TXT: stores the category of simulated vehicles available for each
            household following the vehicle availability model
    - TRIP49_PA_OUT.TXT: contains subzone-level productions and attractions by trip type
            for non-telework households
    - TRIP49_PA_WFH_OUT.TXT: contains subzone-level productions and attractions by trip type
            for telework households
    - TG_HHENUM_OUTPUT.TXT: lists the subzone, zone and household vehicle type of each
            enumerated household
    - households_selected_noWFH_enumeration_count.csv: lists the number of times each survey
            household was selected during trip enumeration for non-telework households
    - households_selected_WFH_enumeration_count.csv: lists the number of times each survey
            household was selected during trip enumeration for telework households
    - trip_generation_model_log.txt: trip generation model log file
    - HI_HHENUM_TRIP_OUT.CSV (optional): contains the number of trips by type for each
            synthetic household
    - PRODS_HH_OUT.CSV (optional): contains a subzone-level summary of trip productions
            by trip type within four categories of households
    - PRODS_GQ_OUT.CSV (optional): contains a subzone-level summary of trip productions for
            workers and nonworking adults within each group quarters resident category
    - FIRST_PA_OUT.CSV (optional): contains preliminary subzone-level productions and 
            attractions by trip type prior to external adjustments
    - EXTERNAL_PA_OUT.CSV (optional): contains subzone-level external productions and 
            attractions by trip type
"""

# ----------------------------------------------------------------------------
# Import packages, set paths and variables.
# ----------------------------------------------------------------------------
import os
import pandas as pd
import numpy as np
from pathlib import Path
import numexpr as ne
import yaml
import timeit
import logging
import vehicle_availability as veh
import household_trip_enumeration as enumer

t_alpha = timeit.default_timer()

# Input Files
baseDir = Path(__file__).resolve().parents[1]
fileDir = Path(__file__).resolve().parents[1].joinpath('tg', 'fortran')
yamlDir = Path(__file__).resolve().parent
configDir = Path(__file__).resolve().parents[2].joinpath('Scripts',
                                                         'prepare',
                                                         'conformity_scenario',
                                                         'hand')
popSynFile = fileDir / 'POPSYN_HH.CSV'
geoFile = fileDir / 'GEOG_IN.TXT'
szAttributesFile = fileDir / 'HH_IN.TXT'
wfhFile = fileDir / 'HH_WFH_STATUS.CSV'
hhvtypeFile = fileDir / 'tg_hhvtype_lookup.csv'
enumerateNoWfhDataFile1 = fileDir / 'HHID_choices1.csv'
enumerateNoWfhDataFile2 = fileDir / 'HHID_choices2.csv'
enumerateWfhDataFile1 = fileDir / 'HHID_wfh1.csv'
enumerateWfhDataFile2 = fileDir / 'HHID_wfh2.csv'
travelSurveyFile = fileDir / 'HI_HHENUM_IN.TXT'
groupQuartersFile = fileDir / 'GQ_IN.TXT'
groupQuartersTripsFile = fileDir / 'GQ_TRIPRATES.csv'
airportFile = fileDir / 'airport_sz.csv'
enrollmentFile = fileDir / 'SCHOOL_IN.CSV'
attractionsFile = fileDir / 'ATTR_IN.TXT'
coeffsAttractFile = fileDir / 'coefficients_attractions.csv'
coeffsProdsFile = fileDir / 'coefficients_nonhome_productions.csv'
externalShares = fileDir / 'EXT_IN.TXT'
yamlBatchin = baseDir / 'batch_file.yaml'
yamlTg = yamlDir / 'trip_generation.yaml'
yamlConfig = configDir / 'config.yaml'

# Output Files
simVehicles = fileDir / 'SIMULATED_HHVEH.TXT'
nowfhEnumCount = fileDir / 'households_selected_noWFH_enumeration_count.csv'
wfhEnumCount = fileDir / 'households_selected_WFH_enumeration_count.csv'
finalProdsAttrs = fileDir / 'TRIP49_PA_OUT.TXT'
finalWfhProdsAttrs = fileDir / 'TRIP49_PA_WFH_OUT.TXT'
logFile = fileDir / 'trip_generation_model_log.txt'
hhVehtypeFile = fileDir / 'TG_HHENUM_OUTPUT.TXT'
syntheticHHTrips = fileDir / 'HI_HHENUM_TRIP_OUT.CSV'
householdProductions = fileDir / 'PRODS_HH_OUT.CSV'
groupQuartersProductions = fileDir / 'PRODS_GQ_OUT.CSV'
firstProdsAttrs = fileDir / 'FIRST_PA_OUT.CSV'
extrnlProdsAttrs = fileDir / 'EXTERNAL_PA_OUT.CSV'

for fl1 in [syntheticHHTrips, householdProductions, groupQuartersProductions,
            firstProdsAttrs, extrnlProdsAttrs]:
    if os.path.exists(fl1):
        os.remove(fl1)

# Configure the logging
handlers = [logging.FileHandler(logFile, mode='w'),
            logging.StreamHandler()]
logging.basicConfig(level=logging.DEBUG, handlers = handlers,
                    format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger()

# Read configuration file attributes
with open(yamlTg, 'r') as file:
    tgConfig = yaml.safe_load(file)

with open(yamlBatchin, 'r') as file:
    batchConfig = yaml.safe_load(file)

with open(yamlConfig, 'r') as file:
    config = yaml.safe_load(file)    

# Create a NumPy Generator seeded from config to pass to functions
rng = np.random.default_rng(tgConfig['randomSeed'])
# Model version
modelVersion = tgConfig['rtpVersion']
# Set maximum sidewalk density value
sidewalkMax = float(tgConfig['sidewalkMaxDensity'])
# Set threshold for enumeration replicates to trigger resampling
replicateMax = int(tgConfig['replicateMax'])
# Set replicate resampling maximum
resampleMax = int(tgConfig['resampleMax'])
# Get regional median household income
regionalMedianIncome = int(tgConfig['incomeRegMed'])
# Get military group quarters workers per person
militaryWorkers = float(tgConfig['militaryWorkersPerPerson'])
# Get dormitory group quarters workers per person
univWorkers = float(tgConfig['univWorkersPerPerson'])
# Get other 16-64 group quarters workers per person
other1664Workers = float(tgConfig['other1664WorkersPerPerson'])
# Get other 65+ group quarters workers per person
other65Workers = float(tgConfig['other65WorkersPerPerson'])
# Get military group quarters worker trip rates
militaryWorkTrips = float(tgConfig['militaryWorkTrips'])
# Get dormitory group quarters worker trip ratesn
univWorkTrips = float(tgConfig['univWorkTrips'])
# Get other 16-64 group quarters worker trip rates
other1664WorkTrips = float(tgConfig['other1664WorkTrips'])
# Get other 65+ group quarters worker trip rates
other65WorkTrips = float(tgConfig['other65WorkTrips'])
# Write optional files
writeOptional = bool(tgConfig['writeOptionalFiles'])

scen_code = batchConfig['scenario_code']
real_year = int(config['scenario_years'][scen_code])

pdVersion = pd.__version__
if float(pdVersion[:3]) >= 1.5:
    pdEngine = 'pyarrow'
else:
    pdEngine = 'c'

# ----------------------------------------------------------------------------
# Functions.
# ----------------------------------------------------------------------------
def run_formula(df):
    """
    Evaluate weight formulas from a DataFrame column using numexpr to calculate
    non-home trip production weights or trip allocation weights.

    For each unique formula in the 'weight_formula' column, evaluates the
    formula expression using numexpr on the subset of rows with that formula.
    Results are aggregated into a single output array.

    Parameters
    ----------
    df : DataFrame
        Input DataFrame containing a 'weight_formula' column with formula
        expressions and numeric columns referenced in those formulas.

    Returns
    -------
    ndarray
        Numeric array (float64) of evaluated results with length equal to
        the number of rows in df. NaN values are returned for rows where
        formula evaluation fails.
    """
    result = np.zeros(len(df))
    for formula in df['weight_formula'].unique():
        mask = df['weight_formula'] == formula
        subset = df[mask]
        # Filter the data to only needed columns for this formula
        local_dict = {col: subset[col].values for col in df.columns}
        try:
            result[mask.values] = ne.evaluate(formula, local_dict)
        except Exception as e:
            logging.error(f"Error evaluating formula '{formula}': {e}")
            result[mask.values] = np.nan
    return result

def fixed_width(outfile, df):
    """
    Write DataFrame to fixed-width format file.

    Extracts specified columns from the input DataFrame and writes them
    to a file with right-justified fields of fixed widths. Suitable for
    output compatibility with legacy fixed-format data files.

    Parameters
    ----------
    outfile : str or Path
        Path where the fixed-width file will be written.
    df : DataFrame
        Input DataFrame containing at least the columns: 'subzone', 'zone',
        'trip_type', 'sz_prods', 'sz_attrs'.

    Returns
    -------
    None
        Writes data directly to outfile.

    Notes
    -----
    Field widths are: subzone (6), zone (6), trip_type (2), sz_prods (9),
    sz_attrs (9). All numeric values are right-justified. sz_prods and
    sz_attrs are formatted with 1 decimal place.
    """
    data = df[['subzone', 'zone', 'trip_type', 'sz_prods', 'sz_attrs']].values
    with open(outfile, 'w') as f:
        for row in data:
            # write right-justified fields of specified width
            f.write('{:>6.0f}{:>6.0f}{:>2.0f}{:>9.1f}{:>9.1f}\n'.format(*row))

# ----------------------------------------------------------------------------
# Step 1. Load data files and prepare data.
# ----------------------------------------------------------------------------
logging.info("Beginning CMAP Trip Generation model: {0} Scenario {1}"
             .format(modelVersion, batchConfig['scenario_code']))
logging.info("Loading data files")

# Load synthetic households
popSynCols = ['subzone','hh_type','vehicles','serial_number','state_puma',
              'row_column','adults','workers','children','income_category',
              'age_category','hh_veh_type','income']
popSynDrop = ['vehicles','hh_veh_type']
hh = pd.read_csv(popSynFile, names=popSynCols, dtype='Int64', engine=pdEngine)
hh['household_record'] = hh.index + 1
hh.drop(popSynDrop, axis=1, inplace=True)
numberHouseholds = len(hh)

# Load geography file
geogCols = ['subzone','county','county_name','state','puma','zone','chicago',
            'cbd','row_column','area','cmap']
geogDrop = ['state','puma','row_column','state_puma']
geog = pd.read_csv(geoFile, names=geogCols, engine=pdEngine)
geog.loc[geog.state.eq('IL'), 'state_puma'] = 1700000 + geog['puma']
geog.loc[geog.state.eq('IN'), 'state_puma'] = 1800000 + geog['puma']
geog.loc[geog.state.eq('WI'), 'state_puma'] = 5500000 + geog['puma']
pumas = geog[['subzone','state_puma']].copy().reset_index(drop=True)
geog.drop(geogDrop, axis=1, inplace=True)

# Load subzone summary data file
szCols = ['subzone','households','adults','workers','children',
          'income_category1','income_category2','income_category3',
          'income_category4','age_category1','age_category2','age_category3',
          'auto_commute_share','sidewalk_density']
szDrop = ['adults','workers','children','income_category1',
          'income_category2','income_category3','income_category4',
          'age_category1','age_category2','age_category3']
sz = pd.read_csv(szAttributesFile, names=szCols, engine=pdEngine)
totalHHs = sz['households'].sum()
sz.drop(szDrop, axis=1, inplace=True)

# Load household work from home flag file
wfhCols = ['serial_number','wfh_flag']  
wfh = pd.read_csv(wfhFile, names=wfhCols, engine=pdEngine, usecols=[0, 1])
wfh['household_record'] = wfh.index + 1
wfh['wfh_flag'] = wfh['wfh_flag'].clip(upper=1)

# Load household vehicle type category file
hhvtype = pd.read_csv(hhvtypeFile, dtype='Int64', engine=pdEngine)
hhvtype.rename(columns={'ADULT': 'adults', 'WORKER': 'workers', 'CHILD': 
                        'children', 'VEH': 'vehicles', 'AGE_INDEX': 
                        'age_category', 'HHVTYPE': 'hh_veh_type'}, 
                        inplace=True)

# Load trip enumeration data for non-Work From Home households
# This identifies the set of survey households that will be sampled
enumNoWfhData1Cols = ['state_puma','hh_veh_type','match_category',
                      'choice_start','choice_end']
enumData1 = pd.read_csv(enumerateNoWfhDataFile1, names=enumNoWfhData1Cols,
                        dtype='Int64', engine=pdEngine)

# Load trip enumeration choices data for non-Work From Home households
# This identifies the survey household numbers and probability of selection
enumNoWfhData2Cols = ['survey_number','cumul_probability']
enumData2 = pd.read_csv(enumerateNoWfhDataFile2, names=enumNoWfhData2Cols,
                        engine=pdEngine)

# Load trip enumeration data for Work From Home households
# This identifies the set of survey households that will be sampled
enumWfhData1Cols = ['state_puma','hh_type','match_category','choice_start',
                    'choice_end']
enumWfhData1 = pd.read_csv(enumerateWfhDataFile1, names=enumWfhData1Cols,
                           dtype='Int64', engine=pdEngine)

# Load trip enumeration choices data for Work From Home households
# This identifies the survey household numbers and probability of selection
enumWfhData2 = pd.read_csv(enumerateWfhDataFile2, names=enumNoWfhData2Cols,
                           engine=pdEngine)

# convert enumeration selection boundaries from FORTRAN index to Python index
# (1-based to 0-based)
enumData1['choice_start'] = enumData1['choice_start'] - 1
enumWfhData1['choice_start'] = enumWfhData1['choice_start'] - 1
enumData1['choice_end'] = enumData1['choice_end'] - 1
enumWfhData1['choice_end'] = enumWfhData1['choice_end'] - 1

# Convert Lee County state_puma to match GEOG_IN.TXT values, if needed
enumData1.loc[enumData1['state_puma'] == 1700104, 'state_puma'] = 1717104
enumWfhData1.loc[enumWfhData1['state_puma'] == 1700104, 'state_puma'] = 1717104

# Convert from cumulative probability to individual probabilities
enumData2['p'] = enumData2['cumul_probability'].shift(1).fillna(0)
enumData2['probability'] = np.where(enumData2['p'] < 1,
                                    enumData2['cumul_probability'] 
                                    - enumData2['p'],
                                    enumData2['cumul_probability'])
enumWfhData2['p'] = enumWfhData2['cumul_probability'].shift(1).fillna(0)
enumWfhData2['probability'] = np.where(enumWfhData2['p'] < 1,
                                    enumWfhData2['cumul_probability'] 
                                    - enumWfhData2['p'],
                                    enumWfhData2['cumul_probability'])

# Load file of household travel survey data
surveyCols = ['state_puma','hh_veh_type','survey_number','adults_survey',
              'workers_survey','nonworkers_survey','children_survey',
              'children12-15_survey','vehicles_survey']
for i in range(1,50):
    fieldName = f"trips{i}"
    surveyCols.append(fieldName)
surveyDrop = ['state_puma','hh_veh_type','adults_survey','workers_survey',
              'nonworkers_survey','children_survey','children12-15_survey',
              'vehicles_survey']
surveyHouseholds = pd.read_csv(travelSurveyFile, names=surveyCols, 
                               engine=pdEngine)
surveyHouseholds.drop(surveyDrop, axis=1, inplace=True)

# Load file of group quarters residents
gqCols = ['subzone','military','college','other16-64','other65']
gq = pd.read_csv(groupQuartersFile, names=gqCols, dtype='Int64',
                 engine=pdEngine)
groupQuarters = pd.melt(gq, id_vars='subzone', var_name='gq_type',
                        value_name='gq_persons')
groupQuarters['gq_persons'] = groupQuarters['gq_persons'].astype(int)
numberGqHousholds = groupQuarters['gq_persons'].sum()

# Load file of group quarters trip rates 3-33, add trip type 1 placeholder
gqTripRates = pd.read_csv(groupQuartersTripsFile, names=['trip_type',
                                                         'trip_production_rate'],
                                                         engine=pdEngine)
new_row = {'trip_type': 1, 'trip_production_rate': 0.0}
gqTripRates.loc[len(gqTripRates)] = new_row
gqTripRates.sort_values('trip_type', inplace=True)

# Load airport subzone file
airports = pd.read_csv(airportFile, usecols=['subzone','year_open'],
                       engine=pdEngine)
airports['notairport_flag'] = np.where(real_year >= airports['year_open'], 0, 1)
airports.drop('year_open', axis=1, inplace=True)

# Load subzone high school and college enrollment file
schools = pd.read_csv(enrollmentFile, names=['subzone','enrollment'],
                      engine=pdEngine)

# Load subzone attractions file
attrCols = ['subzone','retail_employment','total_employment',
            'high_earner_share']
attractions = pd.read_csv(attractionsFile, names=attrCols, engine=pdEngine)
attractions['high_earner_share'] = attractions['high_earner_share'].round(3)
attractions['nonretail_employment'] = (attractions['total_employment']
                                       - attractions['retail_employment'])
attractions['low_earner_share'] = (1.0 - attractions['high_earner_share']
                                   ).round(3)

# Load file of coefficients for attractions allocation
coeffsAttr = pd.read_csv(coeffsAttractFile)

# Load file of coefficients for nonhome productions allocation
coeffsProd = pd.read_csv(coeffsProdsFile)

# Load file of external home-workplace production and attraction shares
extCols = ['state_puma','ext_prod_factor','ext_attr_factor']
extrnShare = pd.read_csv(externalShares, names=extCols, engine=pdEngine)
# Convert Lee County state_puma to match GEOG_IN.TXT values, if needed
extrnShare.loc[extrnShare['state_puma'] == 1700104, 'state_puma'] = 1717104

# Attach data to synthetic households
hh = hh.merge(geog, how='left', on='subzone', copy=False)
hh = hh.merge(sz, how='left', on='subzone', copy=False)
hh = hh.merge(wfh, how='left', on=['household_record','serial_number'],
              copy=False)

# Prepare variables
# Set household category attributes to maximum values for HHTYPE and HHVTYPE
hh['adults'] = hh['adults'].clip(upper=4)
hh['workers'] = hh['workers'].clip(upper=3)
hh['children'] = hh['children'].clip(upper=3)
hh['nonworkers'] = hh['adults'] - hh['workers']
# Set sidewalk density to maximum subzone value
hh['sidewalk_density'] = hh['sidewalk_density'].clip(upper=sidewalkMax)

# ----------------------------------------------------------------------------
# Verify values are within acceptable ranges.
# ----------------------------------------------------------------------------
assert totalHHs == len(hh), "Number of households is not consistent"
assert hh['adults'].min() == 1, "Minimum number of adults for HHTYPE is " \
                                "below 1"
assert hh['adults'].max() == 4, "Maximum number of adults for HHTYPE is not 4"
assert hh['workers'].min() == 0, "Minimum number of workers for HHTYPE is" \
                                    "not 0"
assert hh['workers'].max() == 3, "Maximum number of workers for HHTYPE is " \
                                    "not 3"
assert hh['children'].min() == 0, "Minimum number of children for HHTYPE is" \
                                    "not 0"
assert hh['children'].max() == 3, "Maximum number of children for HHTYPE is" \
                                    "not 3"
assert hh['age_category'].min() == 1, "Minimum age of head of householder " \
                                        "category for HHTYPE is not 1"
assert hh['age_category'].max() == 3, "Maximum age of head of householder" \
                                        "category for HHTYPE is not 3"
assert hh['income_category'].min() == 1, "Minimum income category for HHTYPE" \
                                            "is not 1"
assert hh['income_category'].max() == 4, "Maximum income category for HHTYPE" \
                                            "is not 4"
assert hh['sidewalk_density'].max() == tgConfig['sidewalkMaxDensity'], "Maximum" \
                            "sidewalk density does not match yaml file " \
                            "parameter"
assert hh['nonworkers'].min() == 0, "Minimum number of nonworkers is not 0"
assert hh['wfh_flag'].min() == 0, "Minimum WFH flag is not 0"
assert hh['wfh_flag'].max() == 1, "Maximum WFH flag is not 1"
logging.info('  -- Number of households to process: {0:,}'.format(len(hh)))

# ----------------------------------------------------------------------------
# Step 2. Run the vehicle availability model for each household.
# ----------------------------------------------------------------------------
logging.info("Running the Vehicle Availability model")

# Separate households by number of adults to apply the appropriate model
adult1 = hh[hh.adults.eq(1)].copy().reset_index()
adult2 = hh[hh.adults.eq(2)].copy().reset_index()
adult3 = hh[hh.adults.gt(2)].copy().reset_index()

# Apply vehicle availability model
adult1['vehicles'] = veh.vehOwnOneAdult(adult1[['sidewalk_density',
                                                'row_column', 'age_category',
                                                'workers', 'income_category',
                                                'auto_commute_share']
                                                ].to_numpy(), rng=rng
                                                ).astype(int)
adult2['vehicles'] = veh.vehOwnTwoAdult(adult2[['sidewalk_density',
                                                'row_column', 'age_category',
                                                'workers', 'income_category',
                                                'auto_commute_share',
                                                'children']].to_numpy(),
                                                rng=rng).astype(int)
adult3['vehicles'] = veh.vehOwnThreeAdult(adult3[['sidewalk_density',
                                                  'row_column', 'age_category',
                                                  'workers', 'income_category',
                                                  'auto_commute_share',
                                                  'nonworkers']].to_numpy(),
                                                  rng=rng).astype(int)

# Re-combine the households and write simulated vehicles file
hh = pd.concat([adult1, adult2, adult3], ignore_index=True, sort=False)
hh.sort_values('household_record', inplace=True)
logging.info('       - 0-vehicle households: {0:>10,}'.format((hh['vehicles']
                                                                == 0).sum()))
logging.info('       - 1-vehicle households: {0:>10,}'.format((hh['vehicles']
                                                                == 1).sum()))
logging.info('       - 2-vehicle households: {0:>10,}'.format((hh['vehicles']
                                                                == 2).sum()))
logging.info('       - 3+-vehicle households: {0:>9,}'.format((hh['vehicles']
                                                                == 3).sum()))
logging.info(' --> Writing {0:,} records to {1}'.format(len(hh),
                                                         Path(simVehicles).name))
hh.to_csv(simVehicles, columns=['serial_number','vehicles',
                                'household_record'], index=False)

# Apply household vehicle type category (HHVTYPE)
hh = hh.merge(hhvtype, how='left', on=['adults','workers','children',
                                       'vehicles','age_category'],
                                       copy=False)
assert hh['vehicles'].min() == 0, "Minimum household vehicles is not 0"
assert hh['vehicles'].max() < 4, "Maximum household vehicles is not within " \
                                    "range"
assert hh['hh_veh_type'].min() > 0, "Minimum household vehicle type code " \
                                    "is not greater than 0"
assert hh['hh_veh_type'].max() < 625, "Maximum household vehicle type code " \
                                        "is not within range"

# Write household vehicle type file
veh = hh[['subzone','zone','hh_veh_type']].reset_index(drop=True)
veh.sort_values(by=['zone', 'subzone'], inplace=True)
logging.info(' --> Writing {0:,} records to {1}'.format(
                            len(veh), Path(hhVehtypeFile).name))
veh.to_csv(hhVehtypeFile, header=False, index=False)

# ----------------------------------------------------------------------------
# Step 3. Run the household trip enumeration process.
# ----------------------------------------------------------------------------
logging.info('Running the Household Trip Enumeration Process')
logging.info(' -- Non-Work From Home households')
logging.info('  --  Preparing the data')

# Attach trip enumeration data to households
nowfh = hh[hh.wfh_flag.eq(0)].reset_index(drop=True)
nowfh = nowfh.merge(enumData1, how='left', on=['state_puma','hh_veh_type'],
              copy=False)
assert nowfh.isnull().any().sum() == 0, "Non-work from home data have" \
                                        "missing values"
# Create data arrays
selection_array = enumData2['survey_number'].to_numpy().astype(np.int64)
probability_array = enumData2['probability'].to_numpy()

# Create a dictionary of survey household ids to track how many times each is
# selected during the trip enumeration process
households_chosen = {key: 0 for key in enumData2['survey_number'].unique()}

# Implement household trip enumeration
logging.info('  --  Running trip enumeration')
nowfh['survey_number'] = enumer.tripEnumeration(
    nowfh['choice_start'].to_numpy(),
    nowfh['choice_end'].to_numpy(),
    selection_array,
    probability_array,
    rng=rng,
    households_chosen=households_chosen,
    replicateMax=replicateMax,
    resampleMax=resampleMax
)
                                                    
assert nowfh['survey_number'].min() >= enumData2['survey_number'].min(), \
    "Invalid non-work from home survey household selected during enumeration"
assert nowfh['survey_number'].max() <= enumData2['survey_number'].max(), \
    "Invalid non-work from home survey household selected during enumeration"

# Output dictionary of households selected during trip enumeration
dictList = [[key, value] for key, value in households_chosen.items()]
nowfhCount = pd.DataFrame(dictList, columns=['survey_household',
                                             'selection_count'])
nowfhCount.sort_values(['selection_count'], ascending=False, inplace=True)
nowfhCount.to_csv(nowfhEnumCount, index=False)
# ----------------------------------------------------------------------------
logging.info(' -- Work From Home households')
logging.info('  --  Preparing the data')

# Attach trip enumeration data to households
wfh = hh[hh.wfh_flag.eq(1)].reset_index(drop=True)
wfh = wfh.merge(enumWfhData1, how='left', on=['state_puma','hh_type'],
              copy=False)
assert wfh.isnull().any().sum() == 0, "Work from home data have" \
                                        "missing values"
# Create data arrays
selection_array = enumWfhData2['survey_number'].to_numpy()
probability_array = enumWfhData2['probability'].to_numpy()

# Create a dictionary of survey household ids to track selections
wfh_households_chosen = {key: 0 for key in enumWfhData2['survey_number'].
                         unique()}

# Implement household trip enumeration
logging.info('  --  Running trip enumeration')
wfh['survey_number'] = enumer.tripEnumeration(
    wfh['choice_start'].to_numpy(),
    wfh['choice_end'].to_numpy(),
    selection_array,
    probability_array,
    rng=rng,
    households_chosen=wfh_households_chosen,
    replicateMax=replicateMax,
    resampleMax=resampleMax
)
assert wfh['survey_number'].min() >= enumWfhData2['survey_number'].min(), \
    "Invalid work from home survey household selected during enumeration"
assert wfh['survey_number'].max() <= enumWfhData2['survey_number'].max(), \
    "Invalid work from home survey household selected during enumeration"

# Output dictionary of households selected during trip enumeration
dictList = [[key, value] for key, value in wfh_households_chosen.items()]
wfhCount = pd.DataFrame(dictList, columns=['survey_household',
                                             'selection_count'])
wfhCount.sort_values(['selection_count'], ascending=False, inplace=True)
wfhCount.to_csv(wfhEnumCount, index=False)

# recombine the household data
households = pd.concat([nowfh, wfh], ignore_index=True, sort=False)
households.sort_values('household_record', inplace=True)

# split out fields for HI_HHENUM_TRIP_OUT.CSV
housholds_out = households[['household_record', 'subzone', 'survey_number',
                            'state_puma', 'hh_type', 'hh_veh_type',
                            'vehicles', 'row_column', 'serial_number', 
                            'match_category', 'wfh_flag', 'income',
                            'children']].copy().reset_index(drop=True)
householdEnum = housholds_out.merge(surveyHouseholds, how='left', 
                                    on='survey_number', copy=True
                                    ).reset_index(drop=True)

# Redefine work trips for high income households
householdEnum['trips2'] = np.where(householdEnum['income'] >= regionalMedianIncome,
                                   householdEnum['trips1'],
                                   0)
householdEnum['trips1'] = np.where(householdEnum['income'] >= regionalMedianIncome,
                                   0,
                                   householdEnum['trips1'])
householdEnum.sort_values('household_record', inplace=True)

if writeOptional:
    logging.info(' --> Writing {0:,} records to {1}'.format(
        len(householdEnum), Path(syntheticHHTrips).name))
    householdEnum.to_csv(syntheticHHTrips, index=False)

# prepare data for PRODS_HH_OUT.CSV
# Summarize household productions by subzone, trip type and four household
# categories:  
#  1 = 0 children and 0 vehicles
#  2 = 1 or more children and 0 vehicles
#  3 = 0 children and 1 or more vehicles
#  4 = 1 or more children and 1 or more vehicles
householdEnum.loc[householdEnum.children.eq(0) & householdEnum.vehicles.eq(0),
                        'household_category'] = 1
householdEnum.loc[householdEnum.children.ge(1) & householdEnum.vehicles.eq(0),
                        'household_category'] = 2
householdEnum.loc[householdEnum.children.eq(0) & householdEnum.vehicles.ge(1),
                        'household_category'] = 3
householdEnum.loc[householdEnum.children.ge(1) & householdEnum.vehicles.ge(1),
                        'household_category'] = 4
dropCols = ['children','vehicles','survey_number','household_record',
            'state_puma','hh_type','hh_veh_type','vehicles', 'row_column',
            'serial_number', 'match_category', 'income']
householdEnum.drop(dropCols, axis=1, inplace=True)
hhProds = pd.melt(householdEnum, id_vars=['subzone','wfh_flag',
                                          'household_category'],
                                          var_name='trip_type',
                                          value_name='productions')
prods = hhProds.groupby(['subzone','wfh_flag','household_category',
                         'trip_type']).agg({'productions': 'sum'}
                                           ).reset_index()
prods['trip_type'] = prods['trip_type'].str.replace('trips', '',
                                                    regex=False).astype(int)
prods.sort_values(['subzone','wfh_flag','household_category','trip_type'],
                  inplace=True)

if writeOptional:
    logging.info(' --> Writing {0:,} records to {1}'.format(
                                len(prods), Path(householdProductions).name))
    prods.to_csv(householdProductions, index=False)

# Prepare household data for production and attraction allocations
# Summarize household productions by trip type, subzone and WFH flag
# Summarize household productions by trip type and WFH flag
householdTrips = prods.groupby(['subzone','trip_type','wfh_flag']).agg({
                                'productions': 'sum'}).reset_index()
householdTrips.rename(columns={'productions':'hh_productions'},
                          inplace=True)
tripWfh = prods.groupby(['trip_type','wfh_flag']).agg({'productions': 'sum'}
                                                      ).reset_index()
tripWfh.rename(columns={'productions':'trip_hh_productions'}, inplace=True)

# ----------------------------------------------------------------------------
# Step 4. Calculate group quarters trip productions
# ----------------------------------------------------------------------------
logging.info('Calculating Group Quarters Trip Productions')

# Determine workers and nonworkers
groupQuarters['workers'] = 0.0
groupQuarters.loc[groupQuarters.gq_type.eq('military'), 'workers'] = (
                        groupQuarters['gq_persons'] * militaryWorkers)
groupQuarters.loc[groupQuarters.gq_type.eq('college'), 'workers'] = (
                        groupQuarters['gq_persons'] * univWorkers)
groupQuarters.loc[groupQuarters.gq_type.eq('other16-64'), 'workers'] = (
                        groupQuarters['gq_persons'] * other1664Workers)
groupQuarters.loc[groupQuarters.gq_type.eq('other65'), 'workers'] = (
                        groupQuarters['gq_persons'] * other65Workers)
groupQuarters['workers'] = groupQuarters['workers'].round(3)
groupQuarters['nonworkers'] = 0.0
groupQuarters['nonworkers'] = (groupQuarters['gq_persons']
                               - groupQuarters['workers']).round(3)
groupQuarters = groupQuarters.merge(gqTripRates, how='cross', copy=False)

# Calculate Worker trip productions
# 1 - Home-work low income household 
groupQuarters.loc[groupQuarters.gq_type.eq('military') & 
                  groupQuarters.trip_type.eq(1), 'productions'] = (
                      groupQuarters['workers'] * militaryWorkTrips)
groupQuarters.loc[groupQuarters.gq_type.eq('college') & 
                  groupQuarters.trip_type.eq(1), 'productions'] = (
                      groupQuarters['workers'] * univWorkTrips)
groupQuarters.loc[groupQuarters.gq_type.eq('other16-64') & 
                  groupQuarters.trip_type.eq(1), 'productions'] = (
                      groupQuarters['workers'] * other1664WorkTrips)
groupQuarters.loc[groupQuarters.gq_type.eq('other65') & 
                  groupQuarters.trip_type.eq(1), 'productions'] = (
                      groupQuarters['workers'] * other65WorkTrips)
# 4 - Home-school (only for 16-64)
groupQuarters.loc[groupQuarters.gq_type.eq('other16-64') & 
                  groupQuarters.trip_type.eq(4), 'productions'] = (
                      groupQuarters['workers']
                      * groupQuarters['trip_production_rate'])
# Remaining Worker trip types applied to all GQ persons 
workTypes = [3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
groupQuarters.loc[groupQuarters['trip_type'].isin(workTypes), 'productions'] = (
                    groupQuarters['workers'] 
                    * groupQuarters['trip_production_rate'])

# Calculate Nonworker trip productions
# 21 - Home-school (only for 16-64)
groupQuarters.loc[groupQuarters.gq_type.eq('other16-64') & 
                  groupQuarters.trip_type.eq(21), 'productions'] = (
                      groupQuarters['nonworkers']
                      * groupQuarters['trip_production_rate'])
# Remaining Nonworker trip types applied to all GQ persons except military
skipType = ['military']
nonworkTypes = [22,23,24,25,26,27,28,29,30,31,32,33]
groupQuarters.loc[groupQuarters['trip_type'].isin(nonworkTypes) & 
                  ~groupQuarters['gq_type'].isin(skipType), 'productions'] = (
                      groupQuarters['nonworkers']
                      * groupQuarters['trip_production_rate'])

groupQuarters.fillna(0, inplace=True)
groupQuarters['productions'] = groupQuarters['productions'].round(3)
groupQuarters.drop('trip_production_rate', axis=1, inplace=True)

if writeOptional:
    logging.info(' --> Writing {0:,} records to {1}'.format(len(groupQuarters),
                                            Path(groupQuartersProductions).name))
    groupQuarters.to_csv(groupQuartersProductions, index=False)

# Prepare GQ data for production and attraction allocations
# Summarize GQ productions by trip type and subzone
# Summarize GQ productions by trip type
groupQuartersTrips = groupQuarters.groupby(['subzone','trip_type']).agg(
                                        {'productions': 'sum'}).reset_index()
groupQuartersTrips['wfh_flag'] = 0
groupQuartersTrips.rename(columns={'productions':'gq_productions'},
                          inplace=True)
gqTripTypeTrips = groupQuarters.groupby(['trip_type']).agg(
                                        {'productions': 'sum'}).reset_index()
gqTripTypeTrips.rename(columns={'productions':'trip_gq_productions'},
                          inplace=True)
groupQuartersTrips = groupQuartersTrips.merge(gqTripTypeTrips, how='left',
                                              on='trip_type', copy=False)
gq1Sum = int(groupQuarters['productions'].sum())
gq2Sum = int(groupQuartersTrips['gq_productions'].sum())
assert gq1Sum == gq2Sum, "Mismatch in GQ productions"

# ----------------------------------------------------------------------------
# Step 5. Allocate trip productions and attractions for non-home trip ends
# ----------------------------------------------------------------------------
logging.info('Allocating Productions and Attractions for Non-Home Trip Ends')
logging.info(' -- Preparing the data')

totalHouseholds = numberGqHousholds + numberHouseholds

# Create complete template of subzone-trip type-wfh flag options
prodTemplate = geog[['subzone', 'zone', 'cbd']].copy().reset_index(drop=True)
hhPart = sz[['subzone', 'households']].copy().reset_index(drop=True)
tripTypes = coeffsAttr[['trip_type']].copy().reset_index(drop=True)
tripTypes.drop_duplicates(subset=['trip_type'], keep='first', inplace=True)
prodTemplate = prodTemplate.merge(hhPart, how='left', on='subzone',
                                  copy=False)
prodTemplate = prodTemplate.merge(attractions, how='left', on='subzone',
                                  copy=False)
prodTemplate = prodTemplate.merge(airports, how='left', on='subzone',
                                  copy=False)
prodTemplate = prodTemplate.merge(schools, how='left', on='subzone',
                                  copy=False)
prodTemplate = prodTemplate.merge(tripTypes, how='cross', copy=False)
prodTemplate['notairport_flag'] = prodTemplate['notairport_flag'].fillna(1)
prodTemplate['wfh_flag'] = 0
prodWFH = prodTemplate.copy()
prodWFH['wfh_flag'] = 1
prodTemplate = pd.concat([prodTemplate, prodWFH], ignore_index=True,
                         sort=False)
assert len(prodTemplate) == (17418 * 49 * 2), "prodTemplate is not complete"

logging.info(' -- Allocating non-home productions')
# Attach household and group quarters productions
prodTemplate = prodTemplate.merge(householdTrips, how='left',
                                  on=['subzone','trip_type','wfh_flag'],
                                  copy=False)
prodTemplate = prodTemplate.merge(tripWfh, how='left', on=['trip_type',
                                                               'wfh_flag'],
                                                               copy=False)
prodTemplate = prodTemplate.merge(groupQuartersTrips, how='left',
                                  on=['subzone','trip_type','wfh_flag'],
                                  copy=False)
prodTemplate.fillna(0, inplace=True)

# Attach nonhome production coefficients and calculate shares
prodTemplate = prodTemplate.merge(coeffsProd, how='left',
                                  on=['cbd','trip_type','wfh_flag'],
                                  copy=False)
prodTemplate['sz_share'] = run_formula(prodTemplate)
prodTemplate.drop('weight_formula', axis=1, inplace=True)

logging.info(' -- Allocating attractions')
# Attach attraction coefficients and calculate shares
prodTemplate = prodTemplate.merge(coeffsAttr, how='left',
                                  on=['cbd','trip_type','wfh_flag'],
                                  copy=False)
prodTemplate['sz_share_attr'] = run_formula(prodTemplate)

# Sum subzone shares to determine trip type totals
tripTotals = prodTemplate.groupby(['trip_type','wfh_flag']).agg(
                                            {'sz_share': 'sum',
                                             'sz_share_attr': 'sum'}
                                             ).round(3).reset_index()
tripTotals.rename(columns={'sz_share':'tripShare',
                           'sz_share_attr':'tripShareAttr'}, inplace=True)
prodTemplate = prodTemplate.merge(tripTotals, how='left',
                                  on=['trip_type','wfh_flag'], copy=False)

logging.info(' -- Updating productions')
homeTypes = [1,2,3,4,5,6,7,21,22,23,24,34,35,36,37]
prodTemplate['sz_prods'] = np.where(prodTemplate['trip_type'].isin(homeTypes),
                                    prodTemplate['hh_productions'],
                                    prodTemplate['sz_share']
                                    / prodTemplate['tripShare']
                                    * prodTemplate['trip_hh_productions']
                                   ).round(1)

prodTemplate['sz_gq_prods'] = np.where(prodTemplate['trip_type'].isin(homeTypes),
                                    prodTemplate['gq_productions'],
                                    prodTemplate['sz_share']
                                    / prodTemplate['tripShare']
                                    * prodTemplate['trip_gq_productions']
                                   ).round(1)

logging.info(' -- Updating attractions')
notUsed = [2,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49]
prodTemplate['sz_attrs'] = (prodTemplate['sz_share_attr']
                            / prodTemplate['tripShareAttr']
                            * prodTemplate['trip_hh_productions']).round(1)
prodTemplate['sz_gq_attrs'] = np.where(prodTemplate['trip_type'].isin(notUsed),
                                    prodTemplate['gq_productions'],
                                    prodTemplate['sz_share_attr']
                                    / prodTemplate['tripShareAttr']
                                    * prodTemplate['trip_gq_productions']
                                   ).round(1)

firstPass = prodTemplate[['subzone', 'zone', 'wfh_flag', 'trip_type',
                          'sz_prods', 'sz_attrs', 'sz_gq_prods',
                          'sz_gq_attrs']].copy().reset_index(drop=True)

if writeOptional:
    logging.info(' --> Writing {0:,} records to {1}'.format(len(firstPass),
                                                Path(firstProdsAttrs).name))
    firstPass.to_csv(firstProdsAttrs, index=False)

# ----------------------------------------------------------------------------
# Step 6. Factor external home-based productions and attractions
# ----------------------------------------------------------------------------
logging.info('Factoring External Home-Workplace Productions and Attractions')

# Apply external factors to trip type 1 and 2
adjusted = firstPass[firstPass.trip_type.le(2)].copy().reset_index(drop=True)
unadjusted = firstPass[firstPass.trip_type.gt(2)].copy().reset_index(drop=True)
adjusted = adjusted.merge(pumas, how='left', on='subzone', copy=False)
adjusted = adjusted.merge(extrnShare, how='left', on='state_puma',
                             copy=False)
adjusted['sz_ext_prods'] = (adjusted['sz_prods'] * adjusted['ext_prod_factor']
                            ).round(1)
adjusted['sz_ext_attrs'] = adjusted['sz_attrs'] * adjusted['ext_attr_factor']

# Balance external attractions to productions
extTotals = adjusted.groupby(['wfh_flag','trip_type']).agg(
                                            {'sz_ext_prods': 'sum',
                                             'sz_ext_attrs': 'sum'}
                                             ).reset_index()

extTotals.rename(columns={'sz_ext_prods':'total_ext_prods',
                          'sz_ext_attrs':'total_ext_attrs'}, inplace=True)
adjusted = adjusted.merge(extTotals, how='left', on=['wfh_flag', 'trip_type'],
                             copy=True)
adjusted['sz_ext_attrs'] = (adjusted['sz_ext_attrs'] 
                            * adjusted['total_ext_prods']
                            / adjusted['total_ext_attrs'])

# Update productions and attractions, adjust external attractions if needed
adjusted['sz_ext_attrs'] = np.where(adjusted['sz_ext_attrs']
                                    > adjusted['sz_attrs'],
                                    adjusted['sz_attrs'],
                                    adjusted['sz_ext_attrs']).round(1)
adjusted['sz_prods'] = (adjusted['sz_prods'] - adjusted['sz_ext_prods']
                        ).round(1)
adjusted['sz_attrs'] = (adjusted['sz_attrs'] - adjusted['sz_ext_attrs']
                        ).round(1)

if writeOptional:
    logging.info(' --> Writing {0:,} records to {1}'.format(len(adjusted),
                                                Path(extrnlProdsAttrs).name))
    writeCols = ['subzone', 'zone', 'wfh_flag', 'trip_type', 'sz_ext_prods',
                 'sz_ext_attrs']
    adjusted.to_csv(extrnlProdsAttrs, columns=writeCols, index=False)

# ----------------------------------------------------------------------------
# Step 7. Finalize productions and attractions
# ----------------------------------------------------------------------------
logging.info('Finalizing Productions and Attractions')

dropCols = ['state_puma', 'ext_prod_factor', 'ext_attr_factor', 'sz_ext_prods',
            'sz_ext_attrs', 'total_ext_prods', 'total_ext_attrs']
adjusted.drop(dropCols, axis=1, inplace=True)
prodsAttrs = pd.concat([adjusted, unadjusted], ignore_index=True, sort=False)
prodsAttrs.sort_values(['trip_type','subzone'], inplace=True)
prodsAttrs['sz_prods'] = prodsAttrs['sz_prods'] + prodsAttrs['sz_gq_prods']
prodsAttrs['sz_attrs'] = prodsAttrs['sz_attrs'] + prodsAttrs['sz_gq_attrs']

# Write final productions and attractions
noWfhPA = prodsAttrs[prodsAttrs.wfh_flag.eq(0)].copy().reset_index(drop=True)
fixed_width(finalProdsAttrs, noWfhPA)
logging.info(' --> Writing {0:,} records to {1}'.format(len(noWfhPA),
                                                Path(finalProdsAttrs).name))
#
wfhPA = prodsAttrs[prodsAttrs.wfh_flag.eq(1)].copy().reset_index(drop=True)
fixed_width(finalWfhProdsAttrs, wfhPA)
logging.info(' --> Writing {0:,} records to {1}'.format(len(wfhPA),
                                                Path(finalWfhProdsAttrs).name))

# Final summary
workers = prodsAttrs[prodsAttrs.trip_type.le(20)].copy()
nonworkers = prodsAttrs[prodsAttrs.trip_type.ge(21) & 
                        prodsAttrs.trip_type.le(33)].copy()
children = prodsAttrs[prodsAttrs.trip_type.ge(34)].copy()
logging.info('Final Results')
logging.info(' - Workers')
logging.info('   - Productions: {0:>12,.1f}'.format(workers['sz_prods'].sum()))
logging.info('   - Attractions: {0:>12,.1f}'.format(workers['sz_attrs'].sum()))
logging.info(' - Nonworking Adults')
logging.info('   - Productions: {0:>12,.1f}'.format(nonworkers['sz_prods'].sum()))
logging.info('   - Attractions: {0:>12,.1f}'.format(nonworkers['sz_attrs'].sum()))
logging.info(' - Children 12-15')
logging.info('   - Productions: {0:>12,.1f}'.format(children['sz_prods'].sum()))
logging.info('   - Attractions: {0:>12,.1f}'.format(children['sz_attrs'].sum()))
logging.info(' - Total')
logging.info('   - Productions: {0:>12,.1f}'.format(prodsAttrs['sz_prods'].sum()))
logging.info('   - Attractions: {0:>12,.1f}'.format(prodsAttrs['sz_attrs'].sum()))

t_omega = timeit.default_timer()
logging.info('Trip Generation model took {0:.2f} minutes for {1:,} households'
             .format((t_omega-t_alpha)/60, len(hh)))
logging.info('Trip Generation model finished')