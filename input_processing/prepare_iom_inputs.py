'''
PREPARE_IOM_INPUTS.PY
Adapted from prepare_iom_inputs.sas
Nick Ferguson
7/30/18

Reads results from the trip generation model and prepares inputs for the
doubly-constrained intervening opportunities model used for trip distribution
'''

import pandas as pd
import numpy as np
import sys

# send console output to log file
log = open('prepare_iom_inputs.log', 'w')
sys.stdout = log
sys.stderr = log

project = 'tg_c19q3'
run = '100_20190701'

# DATASETS
dd1 = '..\\data'

p_out = dict(
    hwphi = ['..\\data\\hwphi.in', 'mo2'],
    hwplo = ['..\\data\\hwplo.in', 'mo1'],
    hop = ['..\\data\\hop.in', 'mo3'],
    nhp = ['..\\data\\nhp.in', 'mo4'])

a_out = dict(
    hwahi = ['..\\data\\hwahi.in', 'md2'],
    hwalo = ['..\\data\\hwalo.in', 'md1'],
    hoa = ['..\\data\\hoa.in', 'md3'],
    nha = ['..\\data\\nha.in', 'md4'])

outmdpop = '..\\..\\..\\data\\md400.in'
outmdemp = '..\\..\\..\\data\\md401.in'
outmoemp = '..\\..\\..\\data\\mo400.in'

outm01 = '..\\data\\m01tg.txt'

# INITIALIZE
a = pd.read_sas('{}\\tg{}.sas7bdat'.format(dd1, run))

d = a[['zone17', 'hwplo', 'hwalo', 'hwphi', 'hwahi', 'hop', 'hoa', 'nhp', 'nha']].groupby('zone17', as_index=False).sum()

e1 = d.drop('zone17', axis=1).sum()
e1['fhwlo'] = e1['hwplo'] / e1['hwalo']
e1['fhwhi'] = e1['hwphi'] / e1['hwahi']
e1['fho'] = e1['hop'] / e1['hoa']
e1['fnh'] = e1['nhp'] / e1['nha']

d['hwalo'] = d['hwalo'] * e1['fhwlo']
d['hwahi'] = d['hwahi'] * e1['fhwhi']
d['hoa'] = d['hoa'] * e1['fho']
d['nha'] = d['nha'] * e1['fnh']

# summarize population & emp
a1 = a.copy()
a1['adults'] = a1['avg_adults'] * a1['households']
a1['children'] = a1['avg_children'] * a1['households']
a1['population'] = a1[['adults', 'children', 'gq_mil', 'gq_univ', 'gq_16to64', 'gq_65plus']].sum(axis=1).round()
a1['emp'] = a1['tot_emp'].round()

a2 = a1[['zone17', 'population', 'emp']].groupby('zone17', as_index=False).sum()
a2['population'] = a2['population'].apply(lambda x: max(x, 0))
a2['emp'] = a2['emp'].apply(lambda x: max(x, 0))

z =a2[['population', 'emp']].sum()
print('population: {}\nemp: {}\n'.format(z['population'], z['emp']))

# REPORT
prnt = d.drop('zone17', axis=1).sum()
print('{} -- {} -- prepare_iom_inputs'.format(project, run))
for col, val in prnt.iteritems():
    print('{}: {}'.format(col, val))

# WRITE
d.sort_values('zone17', inplace=True)
a2.sort_values('zone17', inplace=True)

# productions
for k, v in p_out.items():
    with open(v[0], 'w') as f:
        f.write('t matrices\nd {0}\na matrix={0} trips 0 {1} {2} {3} for iom\n'.format(v[1], project, run, k))
        for i, row in d.iterrows():
            f.write('{} all:{:.7g}\n'.format(int(row['zone17']), row[k]))

# attractions
for k, v in a_out.items():
    with open(v[0], 'w') as f:
        f.write('t matrices\nd {0}\na matrix={0} trips 0 {1} {2} {3} for iom\n'.format(v[1], project, run, k))
        for i, row in d.iterrows():
            f.write(' all {}:{:.7g}\n'.format(int(row['zone17']), row[k]))
        
# population
with open(outmdpop, 'w') as f:
    f.write('t matrices\nd md400\na matrix=md400 population 0 {} {} population for TREDIS\n'.format(project, run))
    for i, row in a2.iterrows():
        f.write(' all {}:{}\n'.format(int(row['zone17']), int(row['population'])))
                
# employment
with open(outmdemp, 'w') as f:
    f.write('t matrices\nd md401\na matrix=md401 employment 0 {} {} employment for TREDIS\n'.format(project, run))
    for i, row in a2.iterrows():
        f.write(' all {}:{}\n'.format(int(row['zone17']), int(row['emp'])))
                
with open(outmoemp, 'w') as f:
    f.write('t matrices\nd mo400\na matrix=mo400 employment 0 {} {} employment for TREDIS\n'.format(project, run))
    for i, row in a2.iterrows():
        f.write('{} all:{}\n'.format(int(row['zone17']), int(row['emp'])))

# CONTRIBUTE TO M01
# NEEDS UPDATING

# OUTPUT .TXT FORMATTED FILES FOR M01

# estimate pctdev as density
a['households'] = a['households'].apply(lambda x: max(x, 0))
a['retail_emp'] = a['retail_emp'].apply(lambda x: max(x, 0))
a['pctdev'] = a.apply(lambda x: min((((x['households'] + x['retail_emp']) / x['area']) / 3000), 1) * 1000, axis=1)

a = a[['zone17', 'avg_income_index', 'wrkautoms', 'zmedinc', 'pctdev', 'households']].groupby('zone17').apply(lambda x: x[['avg_income_index', 'wrkautoms', 'zmedinc', 'pctdev']].multiply(x['households'], axis=0).sum() / x['households'].sum()).reset_index()

a['pctdev'].fillna(0, inplace=True)

a['medinc'] = np.where(a['zmedinc'] > 0, a['zmedinc'].apply(lambda x: min(round(x, -3) / 100, 999)), 0)

# bracket pef as confact
a.loc[a['wrkautoms'] > 20, 'confact'] = 1
a.loc[(a['wrkautoms'] > 10) & (a['wrkautoms'] <= 20), 'confact'] = 2
a.loc[a['wrkautoms'] <= 10, 'confact'] = 4

with open(outm01, 'w') as f:
    for i, row in a.iterrows():
        f.write('{} {}\n'.format(int(row['zone17']), int(row['medinc'])))

log.close()

