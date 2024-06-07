from pathlib import Path
import multiprocessing
import pandas as pd

def export_matrix_from_roster(name, spec, outdir, roster, report):
    """
    """
    # Create matrix indices.
    max_taz = 3649
    z_range = range(1, max_taz + 1)
    arrays = [[row for row in z_range for col in z_range],
              [col for row in z_range for col in z_range]]
    pa_index = pd.MultiIndex.from_arrays(arrays, names=['p_zone', 'a_zone'])
    od_index = pd.MultiIndex.from_arrays(arrays, names=['o_zone', 'd_zone'])
    # Define function to calculate production zone.
    p_zone_calc = lambda x: x['o_zone'] if x['a_zone'] == x['d_zone'] else x['d_zone']
    # Select trips from roster.
    select_trips = roster.loc[roster['purpose'].isin(spec['purpose']) &
                              roster['mode'].isin(spec['mode'])].copy()
    with open(report, 'a') as f:
        print(f"{name}: {select_trips['trips'].sum()}", file=f)
    # Sum selected trips by index zones.
    select_trips['p_zone'] = select_trips.apply(p_zone_calc, axis=1)
    if spec['format'] == 'PA':
        mtx_index = pa_index
    elif spec['format'] == 'OD':
        mtx_index = od_index
    p = mtx_index.names[0]
    q = mtx_index.names[1]
    mtx_index_sum = select_trips[[p, q, 'trips']].groupby([p, q]).sum()
    # Format as matrix and export to CSV.
    mtx_header = f"{p[0]}/{q[0]}/{spec['description']}"
    pd.DataFrame(index=mtx_index).merge(mtx_index_sum, how='left', on=[p, q])\
                                 .fillna(0)\
                                 .reset_index()\
                                 .rename(columns={p: mtx_header})\
                                 .pivot(index=mtx_header, columns=q, values='trips')\
                                 .to_csv(outdir.joinpath(f'{name}.csv'))

def export_auto_matrices(projdir, outdir, trip_roster_path):
    """
    Generate daily auto person trip matrices from trip roster and export to CSV.

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
    # Make output subdirectories.
    tripdir = outdir.joinpath('trips')
    tripdir.mkdir(exist_ok=True)
    hovtripdir  = tripdir.joinpath('hov_trips')
    hovtripdir.mkdir(exist_ok=True)
    # Read trip roster.
    roster = pd.read_csv(trip_roster_path)
    # Specify matrices.
    mtx_specs = {'hbwL_auto': {'description': 'total daily low-income hbw auto person trips',
                              'purpose': ['HBWL'],
                              'mode': range(1, 7),
                              'format': 'OD'},
                 'hbwH_auto': {'description': 'total daily high-income hbw auto person trips',
                              'purpose': ['HBWH'],
                              'mode': range(1, 7),
                              'format': 'OD'},
                 'hbs_auto': {'description': 'total daily hbs auto person trips',
                              'purpose': ['HBS'],
                              'mode': range(1, 7),
                              'format': 'OD'},
                 'hbo_auto': {'description': 'total daily hbo auto person trips',
                              'purpose': ['HBO'],
                              'mode': range(1, 7),
                              'format': 'OD'},
                 'nhb_auto': {'description': 'total daily nhb auto person trips',
                              'purpose': ['NHB'],
                              'mode': range(1, 7),
                              'format': 'OD'},
                 'visit_auto': {'description': 'total daily visitor auto person trips',
                                'purpose': ['VISIT'],
                                'mode': range(1, 7),
                                'format': 'OD'},
                 'dead_auto': {'description': 'total daily deadhead auto person trips',
                               'purpose': ['DEAD'],
                               'mode': range(1, 7),
                               'format': 'OD'},
                 'hbw_sov': {'description': 'total daily hbw sov person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [1],
                             'format': 'OD'},
                 'hbw_hov2': {'description': 'total daily hbw hov2 person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [2],
                             'format': 'OD'},
                 'hbw_hov3': {'description': 'total daily hbw hov3+ person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [3],
                             'format': 'OD'},
                 'hbs_sov': {'description': 'total daily hbs sov person trips',
                             'purpose': ['HBS'],
                             'mode': [1],
                             'format': 'OD'},
                 'hbs_hov2': {'description': 'total daily hbs hov2 person trips',
                             'purpose': ['HBS'],
                             'mode': [2],
                             'format': 'OD'},
                 'hbs_hov3': {'description': 'total daily hbs hov3+ person trips',
                             'purpose': ['HBS'],
                             'mode': [3],
                             'format': 'OD'},
                 'hbo_sov': {'description': 'total daily hbo sov person trips',
                             'purpose': ['HBO'],
                             'mode': [1],
                             'format': 'OD'},
                 'hbo_hov2': {'description': 'total daily hbo hov2 person trips',
                             'purpose': ['HBO'],
                             'mode': [2],
                             'format': 'OD'},
                 'hbo_hov3': {'description': 'total daily hbo hov3+ person trips',
                             'purpose': ['HBO'],
                             'mode': [3],
                             'format': 'OD'},
                 'nhb_sov': {'description': 'total daily nhb sov person trips',
                             'purpose': ['NHB'],
                             'mode': [1],
                             'format': 'OD'},
                 'nhb_hov2': {'description': 'total daily nhb hov2 person trips',
                             'purpose': ['NHB'],
                             'mode': [2],
                             'format': 'OD'},
                 'nhb_hov3': {'description': 'total daily nhb hov3+ person trips',
                             'purpose': ['NHB'],
                             'mode': [3],
                             'format': 'OD'}}
    # Export specified matrices.
    report = outdir.joinpath('auto_person_trip_totals.txt')
    if report.exists():
        report.unlink()
    args = []
    for name, spec in mtx_specs.items():
        args.append((name, spec, tripdir, roster, report))
    with multiprocessing.Pool() as pool:
        pool.starmap(export_matrix_from_roster, args)
    # Move HOV matrices to output directory.
    for p in sorted(tripdir.glob('*.csv')):
        if p.stem in ['hbw_sov', 'hbw_hov2', 'hbw_hov3',
                      'hbs_sov', 'hbs_hov2', 'hbs_hov3',
                      'hbo_sov', 'hbo_hov2', 'hbo_hov3',
                      'nhb_sov', 'nhb_hov2', 'nhb_hov3']:
            p.replace(hovtripdir.joinpath(p.name))
    return (tripdir, hovtripdir)

def export_transit_matrices(outdir, modeller):
    """
    Export transit person trip matrices from Emme.
    """
    # Make output subdirectories.
    tripdir = outdir.joinpath('trips')
    tripdir.mkdir(exist_ok=True)
    # Construct Modeller tools.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    matrix_names = {'mf38': 'visit_transit',
                    'mf39': 'hbs_transit',
                    'mf40': 'hbwL_transit',
                    'mf41': 'hbwH_transit',
                    'mf42': 'hbo_transit',
                    'mf43': 'nhb_transit'}
    # Export transit trips.
    export_matrix_data(matrices=[i for i in list(matrix_names.keys())],
                       export_path=tripdir)
    return tripdir