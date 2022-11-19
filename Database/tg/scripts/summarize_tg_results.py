"""
summarize_tg_results.py
05/22/2020
N. Ferguson

Reads data files from the trip generation model and creates a dataset
of the results with summary tables.

Revision history
----------------
05/22/2020 Ferguson: Adapted from summary_tg_results_popsyn.sas.
08/13/2020 Ferguson: Corrected error in P-A to O-D format conversion
           affecting non-home attractions.
03/01/2021 Ferguson: Updated write paths for removal of sas directory.
04/27/2021 Ferguson: Removes puma1 from geog header. Reads trip PAs from
           two files (not WFH and WFH) and combines them.
04/29/2021 Ferguson: Reads HH_IN.TXT with new schema and replaces PEF
           with Sidewalk Index.
"""

import sys

import numpy as np
import pandas as pd

# Set title.
project = sys.argv[1]
run = sys.argv[2]

# Set number of trip types (11 or 49).
num_trip_types = 49

# Set to 1 to include extra child age 12-15 trip types when using the
# 49 trip type output. Otherwise, set to 0.
kid_trips = 1

# Set to 1 to separate high and low-income productions and attractions.
# Otherwise, set to 0.
hilo = 1

# Read paths
pth_fortran = "../fortran"
pth_popsyn_hh = pth_fortran + "/POPSYN_HH.CSV"
pth_hh = pth_fortran + "/HH_IN.TXT"
pth_gq = pth_fortran + "/GQ_IN.TXT"
pth_attr = pth_fortran + "/ATTR_IN.TXT"
pth_geog = pth_fortran + "/GEOG_IN.TXT"
pth_not_wfh_trip = pth_fortran + "/TRIP{}_PA_OUT.TXT".format(num_trip_types)
pth_wfh_trip = pth_fortran + "/TRIP{}_PA_WFH_OUT.TXT".format(num_trip_types)

# Write paths
pth_data = "../data"
pth_results = pth_data + "/tg_results_{}.csv".format(run)
pth_sum_socec = pth_data + "/tg_summary_socec.csv"
pth_sum_trips = pth_data + "/tg_summary_trips.csv"
pth_reports = "../reports"
pth_rpt_input = pth_reports + "/tg_rpt_input.csv"
pth_rpt_output = pth_reports + "/tg_rpt_output.csv"

# Geography
geog = pd.read_csv(
  pth_geog,
  names=[
    'subzone17',
    'fips',
    'cnty_name',
    'state',
    'puma5',
    'zone17',
    'chicago',
    'cbd',
    'row_column',
    'area',
    'cmap'
  ]
)
labels_fips = {
  '17007': 'Illinois Boone',
  '17031': 'Illinois Cook',
  '17037': 'Illinois DeKalb',
  '17043': 'Illinois DuPage',
  '17063': 'Illinois Grundy',
  '17089': 'Illinois Kane',
  '17091': 'Illinois Kankakee',
  '17093': 'Illinois Kendall',
  '17097': 'Illinois Lake',
  '17111': 'Illinois McHenry',
  '17197': 'Illinois Will',
  '17201': 'Illinois Winnebago',
  '17141': 'Illinois Ogle',
  '17099': 'Illinois LaSalle',
  '17103': 'Illinois Lee',
  '18089': 'Indiana Lake',
  '18127': 'Indiana Porter',
  '18091': 'Indiana LaPorte',
  '55059': 'Wisconsin Kenosha',
  '55101': 'Wisconsin Racine',
  '55127': 'Wisconsin Walworth'
}
labels_bin = {'1': 'yes', '0': 'no'}

# Households
hh = pd.read_csv(pth_hh, header=None, usecols=[0, 34, 35])
hh.columns = ['subzone17', 'wrkautoms', 'sdwlkidx']
labels_hh = dict(
  subzone17 = 'subzone',
  wrkautoms = 'work auto mode share',
  pef = 'sidewalk index'
)

# Synthetic household population
popsyn_hh = pd.read_csv(
  pth_popsyn_hh,
  names=[
    'subzone17',
    'hh_type',
    'vehicles',
    'pums',
    'puma5',
    'row_col',
    'adults',
    'workers',
    'children',
    'income_index',
    'age_index',
    'hh_vtype',
    'hh_income'
  ]
)
labels_popsyn_hh = dict(
  subzone17 = 'subzone',
  hh_type = 'household type',
  vehicles = 'available vehicles in household',
  pums = 'household PUMS serial number',
  puma5 = 'PUMA5',
  row_col = 'row-column',
  adults = 'adults in household',
  workers = 'workers in household',
  children = 'children in household',
  income_index = 'household income quartile index',
  age_index = 'age of householder index',
  hh_vtype = 'revised vehicle availability household type',
  hh_income = 'household income'
)

# Group quarters population
gq = pd.read_csv(
  pth_gq,
  names=['subzone17', 'gq_mil', 'gq_univ', 'gq_16to64', 'gq_65plus']
)
labels_gq = dict(
  subzone17 = 'subzone',
  gq_mil = 'military barracks pop',
  gq_univ = 'college dorm pop',
  gq_16to64 = 'other gq pop age 16 to 64',
  gq_65plus = 'other gq pop age 65 plus'
)

# Attractions
attr = pd.read_csv(
  pth_attr,
  names=['subzone17', 'retail_emp', 'tot_emp', 'hi_earn_share']
)
labels_attr = dict(
  subzone17 = 'subzone',
  retail_emp = 'retail employment',
  tot_emp = 'total employment',
  hi_earn_share = 'hi income worker share'
)

# Final trip table from TG model
not_wfh_trip = pd.read_fwf(
  pth_not_wfh_trip,
  widths=[6, 6, 2, 9, 9],
  names=['subzone17', 'zone17', 'trip_type', 'hh_prods', 'hh_attrs']
)
wfh_trip = pd.read_fwf(
  pth_wfh_trip,
  widths=[6, 6, 2, 9, 9],
  names=['subzone17', 'zone17', 'trip_type', 'hh_prods', 'hh_attrs']
)
trip = not_wfh_trip.append(wfh_trip)
trip = trip.groupby(['subzone17', 'zone17', 'trip_type']).sum().reset_index()

# Calculate TAZ median household income
hh_inc = popsyn_hh.merge(geog, on='subzone17', how='left')
hh_inc = hh_inc[['subzone17', 'hh_income', 'zone17']]
zmedinc = hh_inc.groupby('zone17')['hh_income'].median()
zmedinc = zmedinc.rename('zmedinc')
labels_zmedinc = dict(zmedinc = 'zonal median household income')

# Calculate TG zone summary statistics
sz_stats = popsyn_hh.groupby('subzone17').agg(
  {
    'hh_type': np.size,
     'vehicles': np.mean,
     'adults': np.mean,
     'workers': np.mean,
     'children': np.mean,
     'income_index': np.mean,
     'age_index': np.mean
  }
)
sz_stats = sz_stats.rename(
  columns={
    'hh_type': 'households',
    'vehicles': 'avg_vehicles',
    'adults': 'avg_adults',
    'workers': 'avg_workers',
    'children': 'avg_children',
    'income_index': 'avg_income_index',
    'age_index': 'avg_age_index'
  }
)
labels_sz_stats = dict(
  subzone17 = 'subzone',
  households = 'households',
  avg_vehicles = 'available vehicles per household',
  avg_adults = 'adults per household',
  avg_workers = 'workers per household',
  avg_children = 'children per household',
  avg_income_index = 'income quartile index',
  avg_age_index = 'age of householder index'
)

# Create a table for each trip type from the final trip table
trip_tables = {}
for t in range(1, (num_trip_types + 1)):
    trip_t = trip[trip['trip_type'] == t]
    trip_t = trip_t.rename(
      columns={'hh_prods': 'p{}'.format(t), 'hh_attrs': 'a{}'.format(t)}
    )
    trip_t = trip_t.drop(columns=['zone17','trip_type'])
    trip_tables['trip{}'.format(t)] = trip_t

# Create a table of trip type productions and attractions by subzone
trip1 = trip_tables.get('trip1')
for t in range(2, num_trip_types + 1):
    trip1 = trip1.merge(
      trip_tables.get('trip{}'.format(t)), on='subzone17', how='outer'
    )

trip2 = trip1.copy()
# Collapse trip types from 49 trip types
if num_trip_types == 49:
    # Convert P-A to O-D format
    od_types = list(range(8, 11)) + list(range(38, 41))
    for t in od_types:
        trip2['p{}'.format(t)] = (
          (trip2['p{}'.format(t)] + trip2['a{}'.format(t)]) / 2
        )
        trip2['a{}'.format(t)] = trip2['p{}'.format(t)]
    # Home-work trips with income groups
    if hilo == 1:
        groups = dict(lo='1', hi='2')
        for g, t in groups.items():
            trip2['hwp' + g] = (
              trip2['p' + t]
              + (
                  (trip2['p3'] + trip2['p4'] + trip2['p21'])
                  * (trip2['p' + t] / (trip2['p1'] + trip2['p2']))
              )
            )
            trip2['hwa' + g] = (
              trip2['a' + t]
              + (
                  (trip2['a3'] + trip2['a4'] + trip2['a21'])
                  * (trip2['a' + t] / (trip2['a1'] + trip2['a2']))
              )
            )
        #trip2['hwpoth'] = trip2['p3'] + trip2['p4']
        #trip2['hwaoth'] = trip2['a3'] + trip2['a4']
    # Home-work trips without income groups
    else:
        hw_types = list(range(1, 5)) + [21]
        trip2['hwp'] = 0
        trip2['hwa'] = 0
        for t in hw_types:
            trip2['hwp'] += trip2['p{}'.format(t)]
            trip2['hwa'] += trip2['a{}'.format(t)]
    # Home-other trips
    # Exclude trip type 34 (home-school Ps-As for children 12-15)
    #ho_types = range(5, 8).append(range(22, 25)).append(range(34, 38))
    ho_types = list(range(5, 8)) + list(range(22, 25)) + list(range(35, 38))
    trip2['hop'] = 0
    trip2['hoa'] = 0
    for t in ho_types:
        trip2['hop'] += trip2['p{}'.format(t)]
        trip2['hoa'] += trip2['a{}'.format(t)]
    # Non-home trips including extra child age 12-15 trips
    if kid_trips == 1:
        nh_types = (
          list(range(8, 21)) + list(range(25, 34)) + list(range(38, 50))
        )
    # Non-home trips excluding extra child age 12-15 trips
    else:
        nh_types = list(range(8, 21)) + list(range(25, 34))
    trip2['nhp'] = 0
    trip2['nha'] = 0
    for t in nh_types:
        trip2['nhp'] += trip2['p{}'.format(t)]
        trip2['nha'] += trip2['a{}'.format(t)]
# Collapse trip types from 11 trip types
else:
    # Convert P-A to O-D format
    od_types = [4, 5]
    for t in od_types:
        trip2['p{}'.format(t)] = (
          (trip2['p{}'.format(t)] + trip2['a{}'.format(t)]) / 2
        )
        trip2['a{}'.format(t)] = trip2['p{}'.format(t)]
    # Home-work trips
    trip2['hwp'] = trip2['p1']
    trip2['hwa'] = trip2['a1']
    # Home-other trips
    ho_types = [2, 3, 8, 9, 11]
    for t in ho_types:
        trip2['hop'] += trip2['p{}'.format(t)]
        trip2['hoa'] += trip2['a{}'.format(t)]
    # Non-home trips
    nh_types = list(range(4, 8)) + [10]
    trip2['nhp'] = 0
    trip2['nha'] = 0
    for t in nh_types:
        trip2['nhp'] += trip2['p{}'.format(t)]
        trip2['nha'] += trip2['a{}'.format(t)]

labels_types = dict(
  subzone17='subzone',
  p1='worker home-based work low-income productions',
  a1='worker home-based work low-income attractions',
  p2='worker home-based work hi-income productions',
  a2='worker home-based work hi-income attractions',
  p3='worker home-based work-related productions',
  a3='worker home-based work-related attractions',
  p4='worker home-based school productions',
  a4='worker home-based school attractions',
  p5='worker home-based nonhome/work at residence productions',
  a5='worker home-based nonhome/work at residence attractions',
  p6='worker home-based nonhome/work not at residence productions',
  a6='worker home-based nonhome/work not at residence attractions',
  p7='worker home-based shop productions',
  a7='worker home-based shop attractions',
  p8='worker work-based nonhome/work at residence productions',
  a8='worker work-based nonhome/work at residence attractions',
  p9='worker work-based nonhome/work not at residence productions',
  a9='worker work-based nonhome/work not at residence attractions',
  p10='worker work-based shop productions',
  a10='worker work-based shop attractions',
  p11='worker work-based work productions',
  a11='worker work-based work attractions',
  p12='worker nonhome/work at residence nonhome/work at residence productions',
  a12='worker nonhome/work at residence nonhome/work at residence attractions',
  p13='worker nonhome/work at residence nonhome/work not at residence productions',
  a13='worker nonhome/work at residence nonhome/work not at residence attractions',
  p14='worker nonhome/work at residence shop productions',
  a14='worker nonhome/work at residence shop attractions',
  p15='worker nonhome/work not at residence nonhome/work at residence productions',
  a15='worker nonhome/work not at residence nonhome/work at residence attractions',
  p16='worker nonhome/work not at residence nonhome/work not at residence productions',
  a16='worker nonhome/work not at residence nonhome/work not at residence attractions',
  p17='worker nonhome/work not at residence shop productions',
  a17='worker nonhome/work not at residence shop attractions',
  p18='worker shop nonhome/work at residence productions',
  a18='worker shop nonhome/work at residence attractions',
  p19='worker shop nonhome/work not at residence productions',
  a19='worker shop nonhome/work not at residence attractions',
  p20='worker shop shop productions',
  a20='worker shop shop attractions',
  p21='non-working adult home-based school productions',
  a21='non-working adult home-based school attractions',
  p22='non-working adult home-based nonhome at residence productions',
  a22='non-working adult home-based nonhome at residence attractions',
  p23='non-working adult home-based nonhome not at residence productions',
  a23='non-working adult home-based nonhome not at residence attractions',
  p24='non-working adult home-based shop productions',
  a24='non-working adult home-based shop attractions',
  p25='non-working adult nonhome at residence nonhome at residence productions',
  a25='non-working adult nonhome at residence nonhome at residence attractions',
  p26='non-working adult nonhome at residence nonhome not at residence productions',
  a26='non-working adult nonhome at residence nonhome not at residence attractions',
  p27='non-working adult nonhome at residence shop productions',
  a27='non-working adult nonhome at residence shop attractions',
  p28='non-working adult nonhome not at residence nonhome at residence productions',
  a28='non-working adult nonhome not at residence nonhome at residence attractions',
  p29='non-working adult nonhome not at residence nonhome not at residence productions',
  a29='non-working adult nonhome not at residence nonhome not at residence attractions',
  p30='non-working adult nonhome not at residence shop productions',
  a30='non-working adult nonhome not at residence shop attractions',
  p31='non-working adult shop nonhome at residence productions',
  a31='non-working adult shop nonhome at residence attractions',
  p32='non-working adult shop nonhome not at residence productions',
  a32='non-working adult shop nonhome not at residence attractions',
  p33='non-working adult shop shop productions',
  a33='non-working adult shop shop attractions',
  p34='child home-based school productions',
  a34='child home-based school attractions',
  p35='child home-based nonhome at residence productions',
  a35='child home-based nonhome at residence attractions',
  p36='child home-based nonhome not at residence productions',
  a36='child home-based nonhome not at residence attractions',
  p37='child home-based shop productions',
  a37='child home-based shop attractions',
  p38='child school nonhome at residence productions',
  a38='child school nonhome at residence attractions',
  p39='child school nonhome not at residence productions',
  a39='child school nonhome not at residence attractions',
  p40='child school shop productions',
  a40='child school shop attractions',
  p41='child nonhome at residence nonhome at residence productions',
  a41='child nonhome at residence nonhome at residence attractions',
  p42='child nonhome at residence nonhome not at residence productions',
  a42='child nonhome at residence nonhome not at residence attractions',
  p43='child nonhome at residence shop productions',
  a43='child nonhome at residence shop attractions',
  p44='child nonhome not at residence nonhome at residence productions',
  a44='child nonhome not at residence nonhome at residence attractions',
  p45='child nonhome not at residence nonhome not at residence productions',
  a45='child nonhome not at residence nonhome not at residence attractions',
  p46='child nonhome not at residence shop productions',
  a46='child nonhome not at residence shop attractions',
  p47='child shop nonhome at residence productions',
  a47='child shop nonhome at residence attractions',
  p48='child shop nonhome not at residence productions',
  a48='child shop nonhome not at residence attractions',
  p49='child shop shop productions',
  a49='child shop shop attractions',
  hwplo = 'sum of home-based work low income productions',
  hwphi = 'sum of home-based work high income productions',
  hwalo = 'sum of home-based work low income attractions',
  hwahi = 'sum of home-based work high income attractions',
  # hwpoth = 'sum of home-based work misc productions',
  # hwaoth = 'sum of home-based work misc attractions',
  hwp = 'sum of home-based work productions',
  hwa = 'sum of home-based work attractions',
  hop = 'sum of home-based other productions',
  hoa = 'sum of home-based other attractions',
  nhp = 'sum of non-home based productions',
  nha = 'sum of non-home based attractions'
)

# Delete extra fields after Ps and As have been collapsed to more
# general trip types
trip3 = trip2.copy()
for t in range(1, num_trip_types + 1):
    trip3 = trip3.drop(columns=['p{}'.format(t), 'a{}'.format(t)])
trip3 = trip3[
  ['subzone17', 'nhp', 'nha', 'hwplo', 'hwphi', 'hwalo', 'hwahi', 'hop', 'hoa']
]

# Merge TG data
results = geog.copy()
for d in [sz_stats, gq, attr, trip3, hh]:
    results = results.merge(d, on='subzone17', how='left')
results = results.merge(zmedinc, on='zone17', how='left')
results = results.fillna(0)

# Summarize TG socec data
socec = results.copy()
socec['workers'] = (
  (np.maximum(socec['avg_workers'], 0) * np.maximum(socec['households'], 0))
  + (np.maximum(socec['gq_mil'], 0) * 1.00)
  + (np.maximum(socec['gq_univ'], 0) * 0.88)
  + (np.maximum(socec['gq_16to64'], 0) * 0.50)
  + (np.maximum(socec['gq_65plus'], 0) * 0.18)
)
socec['adults'] = (
  np.maximum(socec['avg_adults'], 0) * np.maximum(socec['households'], 0)
)
socec['children'] = (
  np.maximum(socec['avg_children'], 0) * np.maximum(socec['households'], 0)
)
socec['population'] = (
  socec['adults']
  + socec['children']
  + socec['gq_mil']
  + socec['gq_univ']
  + socec['gq_16to64']
  + socec['gq_65plus']
)
socec = socec[
  [
    'cmap',
    'fips',
    'chicago',
    'cbd',
    'households',
    'workers',
    'retail_emp',
    'tot_emp',
    'adults',
    'children',
    'population'
  ]
]
sum_socec = socec.groupby(['cmap', 'fips', 'chicago', 'cbd']).sum()

# Prepare TG input report from socec data
rpt_input = socec.copy()
for row in range(len(rpt_input)):
    rpt_input.loc[row, 'fips'] = labels_fips[str(rpt_input.loc[row, 'fips'])]
    rpt_input.loc[row, 'chicago'] = labels_bin[
      str(rpt_input.loc[row, 'chicago'])
    ]
    rpt_input.loc[row, 'cbd'] = labels_bin[str(rpt_input.loc[row, 'cbd'])]
    rpt_input.loc[row, 'cmap'] = labels_bin[str(rpt_input.loc[row, 'cmap'])]
rpt_input = rpt_input.pivot_table(
  values=[
    'households',
    'workers',
    'retail_emp',
    'tot_emp',
    'adults',
    'children',
    'population'
  ],
  index=['cmap', 'fips', 'chicago', 'cbd'],
  aggfunc='sum',
  margins=True
)
rpt_input = rpt_input.round(0)
rpt_input = rpt_input.reindex(
  columns=[
    'households',
    'workers',
    'retail_emp',
    'tot_emp',
    'population',
    'adults',
    'children'
  ]
)
rpt_input = rpt_input.rename(columns=labels_attr)

# Summarize TG trip data
trips = results.copy()
if num_trip_types == 49 and hilo == 1:
    trips['hwp'] = trips['hwplo'] + trips['hwphi']# + trips['hwpoth']
    trips['hwa'] = trips['hwalo'] + trips['hwahi']# + trips['hwaoth']
trips = trips[
  ['hwp', 'hwa', 'hop', 'hoa', 'nhp', 'nha', 'cmap', 'fips', 'chicago', 'cbd']
]
sum_trips = trips.groupby(['cmap', 'fips', 'chicago', 'cbd']).sum()

# Prepare TG output report from trip data
rpt_output = trips.copy()
for row in range(len(rpt_output)):
    rpt_output.loc[row, 'fips'] = labels_fips[str(rpt_output.loc[row, 'fips'])]
    rpt_output.loc[row, 'chicago'] = labels_bin[
      str(rpt_output.loc[row, 'chicago'])
    ]
    rpt_output.loc[row, 'cbd'] = labels_bin[str(rpt_output.loc[row, 'cbd'])]
    rpt_output.loc[row, 'cmap'] = labels_bin[str(rpt_output.loc[row, 'cmap'])]
rpt_output = rpt_output.pivot_table(
  values=['hwp', 'hwa', 'hop', 'hoa', 'nhp', 'nha'],
  index=['cmap', 'fips', 'chicago', 'cbd'],
  aggfunc='sum',
  margins=True
)
rpt_output = rpt_output.round(0)
rpt_output = rpt_output.reindex(
  columns=['hwp', 'hwa', 'hop', 'hoa', 'nhp', 'nha']
)
rpt_output = rpt_output.rename(columns=labels_types)

# Write out TG data
results.to_csv(pth_results)

# Write out TG socec summary
sum_socec.to_csv(pth_sum_socec)

# Write out TG trip summary
sum_trips.to_csv(pth_sum_trips)

# Write out TG input report
rpt_input.to_csv(pth_rpt_input)

# Write out TG output report
rpt_output.to_csv(pth_rpt_output)
