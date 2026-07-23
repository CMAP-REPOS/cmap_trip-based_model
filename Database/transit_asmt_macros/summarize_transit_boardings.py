'''
SUMMARIZE_TRANSIT_BOARDINGS.PY
    Craig Heither, 10-25-2022
    Translated from Emme macro to Python by Tim O'Leary, 9-26-2025
    
    Summarize transit boardings for congested transit assignment.
    No inputs necessary-- reads batch_file.yaml to determine scen numbers.
    Will not run unless transit assignment has been completed.
'''

#libraries
import os
import sys
from pathlib import Path
import yaml
import pandas as pd

#startup emme
proj_dir = Path(__file__).resolve().parents[2]
db = proj_dir.joinpath('Database')
sys.path.append(str(proj_dir.joinpath('Scripts')))
from tbmtools import project as tbm

#connect to modeller
modeller = tbm.connect(proj_dir)
emmebank = modeller.emmebank

#output location
out_boarding_csv = os.path.join(
    db, 'transit_asmt_macros/report/Boarding_summary.csv'
)
out_transit_punch = os.path.join(
    db, 'transit_asmt_macros/report/transit_punch_segment.csv'
)

if os.path.exists(out_boarding_csv):
    os.remove(out_boarding_csv)

#get scenario info from batch_file.yaml for transit asmt scenarios
db = proj_dir.joinpath('Database')
with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)
yr = str(config['scenario_code'])[0]  # e.g., '2' from '200'

trnt_scens = {
    f'Night (6pm-6am)': f'{yr}21',
    f'AM (6am-9am)': f'{yr}23',
    f'Midday (9am-4pm)': f'{yr}25',
    f'PM (4pm-6pm)': f'{yr}27',
}

tr_dfs = []

for period_name, scen_num in trnt_scens.items():

    scen = emmebank.scenario(scen_num)
    if scen is None:
        raise ValueError(f'Scenario {scen_num} not found in Emmebank')

    network = scen.get_network()
    links = network.links()

    trpunch_list = []
    for link in network.links():
        segments = link.segments() #empty if link does not have transit segments
        inode = network.node(link.i_node)
        jnode = network.node(link.j_node)
        #for each segment (if it has any), add this info to table:
        for segment in segments:
            line = network.transit_line(str(segment.id).split('-')[0])
            seg = [
                link.id,
                segment.id,
                link.length,
                line.headway,
                segment['transit_boardings'],
                segment['transit_volume'],
                segment['data1'],
                inode['@zone'],
                jnode['@zone']
            ]

            trpunch_list.append(seg)
            
    columns = ['id', 'segment_id', 'length', 'headway', 'transit_boardings','transit_volume','ltime','inode_zone','jnode_zone']
    trpunch = pd.DataFrame(data=trpunch_list, columns=columns)

    trpunch.eval('pmt = transit_volume * length', inplace=True)
    trpunch.eval('pht = transit_volume * ltime / 60', inplace=True)

    #use transit line info to determine mode
    def line_mode(x):
        '''
        function for .map() method used directly below this function 
        to determine mode name from segment_id
        
        segment_id format: [alpha-code][5-digit number], e.g., 'b12535', 'cbl10011'
        
        b -> CTA local bus
        e -> CTA express
        p -> Pace regular bus
        l -> Pace feeder bus
        q -> Pace express bus
        c** -> CTA rail lines (3-letter code)
        m** -> Metra lines (3-letter code)
        
        returns: the english name representation of the mode code
        
        '''
        
        line = x.split('-')[0]
        name = ''.join([char for char in line if char.isalpha()])
        name_key = {
            'cbl': 'CTA Blue Line',
            'cbr': 'CTA Brown Line',
            'cg': 'CTA Green Line',
            'cor': 'CTA Orange Line',
            'cpk': 'CTA Pink Line',
            'cpr': 'CTA Purple Line',
            'crd': 'CTA Red Line',
            'cye': 'CTA Yellow Line',
            'mbn': 'Metra BNSF',
            'mhc': 'Metra Heritage Corridor',
            'mme': 'Metra Electric',
            'mmn': 'Metra Milwaukee North',
            'mmw': 'Metra Milwaukee West',
            'mnc': 'Metra North Central',
            'mri': 'Metra Rock Island',
            'msw': 'Metra SouthWest Service',
            'mun': 'Metra UP North',
            'mnw': 'Metra UP Northwest',
            'muw': 'Metra UP West',
            'mss': 'NICTD South Shore',
            'b': 'CTA Regular Bus',
            'e': 'CTA Express Bus',
            'p': 'Pace Regular Bus',
            'q': 'Pace Express Bus',
            'l': 'Pace Feeder Bus',
        }
        nam = [code for code in name_key.keys() if code in name]
        nam = max(nam, key=len) if len(nam) > 0 else None
        return name_key[nam]

    trpunch['mode'] = trpunch['segment_id'].map(line_mode)

    if len(trpunch.loc[trpunch['mode'].isnull()]) > 0:
        print(f'SOME TRANSIT SEGMENTS WERE NOT MAPPED TO A MODE!')
        print(trpunch.loc[trpunch['mode'].isnull()])
        raise ValueError('Some transit segments were not mapped to a mode (must be c, m, p, q, l, b, or e). Look at printed output of errors above.')

    trpunch['tod'] = scen_num
    trpunch['tod_name'] = period_name

    tr_dfs.append(trpunch)

tr_all = pd.concat(tr_dfs, ignore_index=True)

#output all segment info to csv (similar to punch link)
tr_all.to_csv(out_transit_punch, index=False)

#summary by time-of-day
summary_tod = tr_all.groupby(['tod_name', 'tod', 'mode']).agg({
    'transit_boardings':'sum',
    'pmt':'sum'
}).reset_index()
#total daily
summary_daily = summary_tod.groupby('mode').agg({
    'transit_boardings':'sum',
    'pmt':'sum'
}).reset_index()
summary_daily['tod'] = 'All'
summary_daily['tod_name'] = 'Daily'

summary = pd.concat([summary_tod, summary_daily], ignore_index=True)
summary.to_csv(out_boarding_csv, index=False)
print(f'Summary transit boarding information written to: {out_boarding_csv}')