'''
prepare_iom_inputs.py
07/30/2018
N. Ferguson

Reads results from the trip generation model and prepares inputs for the
doubly-constrained intervening opportunities model used for trip
distribution.

Revision history
----------------
07/30/2018 Ferguson: Adapted from prepare_iom_inputs.sas.
03/01/2021 Ferguson: Updated paths for removal of sas directory.
05/12/2021 Heither: Do not cap income value for m01tg.txt at 999
'''

import sys

import numpy as np
import pandas as pd

# Set title
project = sys.argv[1]
run = sys.argv[2]

# Read paths
pth_tg_data = "../data"
pth_results = pth_tg_data + "/tg_results_{}.csv".format(run)

# Write paths
pth_log = "prepare_iom_inputs.log"

p = dict(
  hwphi = [pth_tg_data + "/hwphi.in", 'mo2'],
  hwplo = [pth_tg_data + "/hwplo.in", 'mo1'],
  hop = [pth_tg_data + "/hop.in", 'mo3'],
  nhp = [pth_tg_data + "/nhp.in", 'mo4']
)
a = dict(
  hwahi = [pth_tg_data + "/hwahi.in", 'md2'],
  hwalo = [pth_tg_data + "/hwalo.in", 'md1'],
  hoa = [pth_tg_data + "/hoa.in", 'md3'],
  nha = [pth_tg_data + "/nha.in", 'md4']
)

pth_db_data = "../../data"
pth_mdpop = pth_db_data + "/md400.in"
pth_mdemp = pth_db_data + "/md401.in"
pth_moemp = pth_db_data + "/mo400.in"

pth_m01 = pth_tg_data + "/m01tg.txt"

# Initialize log file
log = open(pth_log, 'w')
sys.stdout = log
sys.stderr = log
print('{}_{}'.format(project, run))

# Read in data
tg = pd.read_csv(pth_results)

# Balance attractions
trips = tg[
  ['zone17', 'hwplo', 'hwalo', 'hwphi', 'hwahi', 'hop', 'hoa', 'nhp', 'nha']
]
trips = trips.groupby('zone17', as_index=False).sum()

trip_factors = trips.drop(columns='zone17').sum()
trip_factors['fhwlo'] = trip_factors['hwplo'] / trip_factors['hwalo']
trip_factors['fhwhi'] = trip_factors['hwphi'] / trip_factors['hwahi']
trip_factors['fho'] = trip_factors['hop'] / trip_factors['hoa']
trip_factors['fnh'] = trip_factors['nhp'] / trip_factors['nha']

trips['hwalo'] = trips['hwalo'] * trip_factors['fhwlo']
trips['hwahi'] = trips['hwahi'] * trip_factors['fhwhi']
trips['hoa'] = trips['hoa'] * trip_factors['fho']
trips['nha'] = trips['nha'] * trip_factors['fnh']

# Summarize population and employment
socec = tg.copy()
socec['adults'] = (socec['avg_adults'] * socec['households']).round()
socec['children'] = (socec['avg_children'] * socec['households']).round()
socec_pop = socec[
  ['adults', 'children', 'gq_mil', 'gq_univ', 'gq_16to64', 'gq_65plus']
]
socec['population'] = socec_pop.sum(axis='columns').round()
socec['emp'] = socec['tot_emp'].round()
socec = socec[['zone17', 'population', 'emp']]
socec = socec.groupby('zone17', as_index=False).sum()
socec['population'] = socec['population'].apply(lambda x: max(x, 0))
socec['emp'] = socec['emp'].apply(lambda x: max(x, 0))

# Report totals to log file
socec_tot = socec[['population', 'emp']].sum()
print('Total population: {}'.format(socec_tot['population']))
print('Total employment: {}'.format(socec_tot['emp']))

trips_tot = trips.drop('zone17', axis='columns').sum()
for type, tot in trips_tot.items():
    print('Total {}: {}'.format(type, tot))

trips.sort_values('zone17', inplace=True)
socec.sort_values('zone17', inplace=True)

# Write out productions
for type, v in p.items():
    pth = v[0]
    mtx = v[1]
    with open(pth, 'w') as f:
        f.write('t matrices\n')
        f.write('d {}\n'.format(mtx))
        f.write(
          'a matrix={} trips 0 {} {} {} for iom\n'.format(mtx, project, run, type)
        )
        for i, row in trips.iterrows():
            f.write('{} all:{:.7g}\n'.format(int(row['zone17']), row[type]))

# Write out attractions
for type, v in a.items():
    pth = v[0]
    mtx = v[1]
    with open(pth, 'w') as f:
        f.write('t matrices\n')
        f.write('d {0}\n'.format(mtx))
        f.write(
          'a matrix={} trips 0 {} {} {} for iom\n'.format(mtx, project, run, type)
        )
        for i, row in trips.iterrows():
            f.write(' all {}:{:.7g}\n'.format(int(row['zone17']), row[type]))

# Write out population
with open(pth_mdpop, 'w') as f:
    f.write('t matrices\n')
    f.write('d md400\n')
    f.write(
      'a matrix=md400 population 0 {} {} population for TREDIS\n'.format(project, run)
    )
    for i, row in socec.iterrows():
        f.write(
          ' all {}:{}\n'.format(int(row['zone17']), int(row['population']))
        )

# Write out employment
with open(pth_mdemp, 'w') as f:
    f.write('t matrices\n')
    f.write('d md401\n')
    f.write(
      'a matrix=md401 employment 0 {} {} employment for TREDIS\n'.format(project, run)
    )
    for i, row in socec.iterrows():
        f.write(' all {}:{}\n'.format(int(row['zone17']), int(row['emp'])))

with open(pth_moemp, 'w') as f:
    f.write('t matrices\n')
    f.write('d mo400\n')
    f.write(
      'a matrix=mo400 employment 0 {} {} employment for TREDIS\n'.format(project, run)
    )
    for i, row in socec.iterrows():
        f.write('{} all:{}\n'.format(int(row['zone17']), int(row['emp'])))

# Estimate pctdev as density
tg['households'] = tg['households'].apply(lambda x: max(x, 0))
tg['retail_emp'] = tg['retail_emp'].apply(lambda x: max(x, 0))
tg['pctdev'] = tg.apply(
  lambda x: (
    min((((x['households'] + x['retail_emp']) / x['area']) / 3000), 1) * 1000
  ),
  axis='columns'
)
tg = tg[
  [
    'zone17',
    'avg_income_index',
    'wrkautoms',
    'zmedinc',
    'pctdev',
    'households'
  ]
]
tg = tg.groupby('zone17').apply(
  lambda x: (
    x[['avg_income_index', 'wrkautoms', 'zmedinc', 'pctdev']].multiply(x['households'], axis='index').sum()
    / x['households'].sum()
  )
)
tg = tg.reset_index()
tg['pctdev'] = tg['pctdev'].fillna(0)

# Format median income in 100s of dollars with a floor of 0 
# and no ceiling
tg['medinc'] = np.where(
  tg['zmedinc'] > 0,
  tg['zmedinc'].apply(lambda x: round(x, -3) / 100),
  0
)

# Bracket PEF as confact
#tg.loc[tg['wrkautoms'] > 20, 'confact'] = 1
#tg.loc[(tg['wrkautoms'] > 10) & (tg['wrkautoms'] <= 20), 'confact'] = 2
#tg.loc[tg['wrkautoms'] <= 10, 'confact'] = 4

# Write out median income
with open(pth_m01, 'w') as f:
    for i, row in tg.iterrows():
        f.write('{} {}\n'.format(int(row['zone17']), int(row['medinc'])))

log.close()
