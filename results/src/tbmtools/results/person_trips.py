from pathlib import Path
import pandas as pd
from tbmtools.results import trip_roster

def export_matrices(projdir, outdir):
    """
    Generate daily person trip matrices from trip roster and export to CSV.

    Parameters:  projdir : str or path object
                     Path to Emme project directory.

                 outdir : str or path object
                     Path to output directory.

    Returns:     path object
                     Path to output OMX.
    """
    # Handle arguments.
    if isinstance(projdir, str):
        projdir = Path(projdir).resolve()
    if isinstance(outdir, str):
        outdir = Path(outdir).resolve()

    # Check for trip roster.
    trip_roster_path = outdir.joinpath('trip_roster.csv')
    if not trip_roster_path.exists():
        print('Exported', trip_roster.run_export(projdir, outdir))
    else:
        print('Found', trip_roster_path)

    # Make output directories.
    mtxdir1 = outdir.joinpath('trips')
    mtxdir1.mkdir(exist_ok=True)
    mtxdir2 = mtxdir1.joinpath('work_trips')
    mtxdir2.mkdir(exist_ok=True)
    mtxdir3 = mtxdir1.joinpath('hov_trips')
    mtxdir3.mkdir(exist_ok=True)

    # Create matrix indices.
    max_taz = 3649
    z_range = range(1, max_taz + 1)
    arrays = [[row for row in z_range for col in z_range],
              [col for row in z_range for col in z_range]]
    pa_index = pd.MultiIndex.from_arrays(arrays, names=['p_zone', 'a_zone'])
    od_index = pd.MultiIndex.from_arrays(arrays, names=['o_zone', 'd_zone'])

    # Specify matrices.
    mtx_specs = {'hbwL_all': {'description': 'total daily low-income hbw person trips',
                              'purpose': ['HBWL'],
                              'mode': range(1, 10),
                              'format': pa_index},
                 'hbwH_all': {'description': 'total daily high-income hbw person trips',
                              'purpose': ['HBWH'],
                              'mode': range(1, 10),
                              'format': pa_index},
                 'hbw_auto': {'description': 'total daily hbw auto person trips',
                              'purpose': ['HBWH', 'HBWL'],
                              'mode': range(1, 7),
                              'format': pa_index},
                 'hbwL_auto': {'description': 'total daily low-income hbw auto person trips',
                              'purpose': ['HBWL'],
                              'mode': range(1, 7),
                              'format': pa_index},
                 'hbwH_auto': {'description': 'total daily high-income hbw auto person trips',
                              'purpose': ['HBWH'],
                              'mode': range(1, 7),
                              'format': pa_index},
                 'hbs_auto': {'description': 'total daily hbs auto person trips',
                              'purpose': ['HBS'],
                              'mode': range(1, 7),
                              'format': pa_index},
                 'hbo_auto': {'description': 'total daily hbo auto person trips',
                              'purpose': ['HBO'],
                              'mode': range(1, 7),
                              'format': pa_index},
                 'nhb_auto': {'description': 'total daily nhb auto person trips',
                              'purpose': ['NHB'],
                              'mode': range(1, 7),
                              'format': od_index},
                 'visit_auto': {'description': 'total daily visitor auto person trips',
                                'purpose': ['VISIT'],
                                'mode': range(1, 7),
                                'format': od_index},
                 'dead_auto': {'description': 'total daily deadhead auto person trips',
                               'purpose': ['DEAD'],
                               'mode': range(1, 7),
                               'format': od_index},
                 'hbw_sov': {'description': 'total daily hbw sov person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [1],
                             'format': pa_index},
                 'hbw_hov2': {'description': 'total daily hbw hov2 person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [2],
                             'format': pa_index},
                 'hbw_hov3': {'description': 'total daily hbw hov3+ person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [3],
                             'format': pa_index},
                 'hbs_sov': {'description': 'total daily hbs sov person trips',
                             'purpose': ['HBS'],
                             'mode': [1],
                             'format': pa_index},
                 'hbs_hov2': {'description': 'total daily hbs hov2 person trips',
                             'purpose': ['HBS'],
                             'mode': [2],
                             'format': pa_index},
                 'hbs_hov3': {'description': 'total daily hbs hov3+ person trips',
                             'purpose': ['HBS'],
                             'mode': [3],
                             'format': pa_index},
                 'hbo_sov': {'description': 'total daily hbo sov person trips',
                             'purpose': ['HBO'],
                             'mode': [1],
                             'format': pa_index},
                 'hbo_hov2': {'description': 'total daily hbo hov2 person trips',
                             'purpose': ['HBO'],
                             'mode': [2],
                             'format': pa_index},
                 'hbo_hov3': {'description': 'total daily hbo hov3+ person trips',
                             'purpose': ['HBO'],
                             'mode': [3],
                             'format': pa_index},
                 'nhb_sov': {'description': 'total daily nhb sov person trips',
                             'purpose': ['NHB'],
                             'mode': [1],
                             'format': od_index},
                 'nhb_hov2': {'description': 'total daily nhb hov2 person trips',
                             'purpose': ['NHB'],
                             'mode': [2],
                             'format': od_index},
                 'nhb_hov3': {'description': 'total daily nhb hov3+ person trips',
                             'purpose': ['NHB'],
                             'mode': [3],
                             'format': od_index}}
    # Read trip roster.
    trip_roster_df = pd.read_csv(trip_roster_path)

    # Function to calculate production zone.
    p_zone_calc = lambda x: x['o_zone'] if x['a_zone'] == x['d_zone'] else x['d_zone']

    # Build specified matrices.
    for name, spec in mtx_specs.items():

        # Select trips from roster.
        select_trips = trip_roster_df.loc[trip_roster_df['purpose'].isin(spec['purpose']) &
                                          trip_roster_df['mode'].isin(spec['mode'])].copy()
        print(f"{name}: {select_trips['trips'].sum()}")
    
        # Sum selected trips by index zones.
        select_trips['p_zone'] = select_trips.apply(p_zone_calc, axis=1)
        mtx_index = spec['format']
        p = mtx_index.names[0]
        q = mtx_index.names[1]
        mtx_index_sum = select_trips[[p, q, 'trips']].groupby([p, q]).sum()
        
        # Format as matrix and export to CSV.
        if name in ['hbwL_all', 'hbwH_all',
                    'hbwL_auto', 'hbwH_auto']:
            mtxdir = mtxdir2
        elif name in ['hbw_sov', 'hbw_hov2', 'hbw_hov3',
                      'hbs_sov', 'hbs_hov2', 'hbs_hov3',
                      'hbo_sov', 'hbo_hov2', 'hbo_hov3',
                      'nhb_sov', 'nhb_hov2', 'nhb_hov3']:
            mtxdir = mtxdir3
        else:
            mtxdir = mtxdir1
        mtx_header = f"{p[0]}/{q[0]}/{spec['description']}"
        mtx = pd.DataFrame(index=mtx_index).merge(mtx_index_sum, how='left', on=[p, q])\
                                           .fillna(0)\
                                           .reset_index()\
                                           .rename(columns={p: mtx_header})\
                                           .pivot(index=mtx_header, columns=q, values='trips')\
                                           .to_csv(mtxdir.joinpath(f'{name}.csv'))
    return mtxdir