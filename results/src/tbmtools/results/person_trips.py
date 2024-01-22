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

def export_auto_matrices(projdir, outdir):
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
    worktripdir = tripdir.joinpath('work_trips')
    worktripdir.mkdir(exist_ok=True)
    hovtripdir  = tripdir.joinpath('hov_trips')
    hovtripdir.mkdir(exist_ok=True)
    # Read trip roster.
    roster = pd.read_csv(outdir.joinpath('trip_roster.csv'))
    # Specify matrices.
    mtx_specs = {'hbwL_all': {'description': 'total daily low-income hbw person trips',
                              'purpose': ['HBWL'],
                              'mode': range(1, 10),
                              'format': 'PA'},
                 'hbwH_all': {'description': 'total daily high-income hbw person trips',
                              'purpose': ['HBWH'],
                              'mode': range(1, 10),
                              'format': 'PA'},
                 'hbw_auto': {'description': 'total daily hbw auto person trips',
                              'purpose': ['HBWH', 'HBWL'],
                              'mode': range(1, 7),
                              'format': 'PA'},
                 'hbwL_auto': {'description': 'total daily low-income hbw auto person trips',
                              'purpose': ['HBWL'],
                              'mode': range(1, 7),
                              'format': 'PA'},
                 'hbwH_auto': {'description': 'total daily high-income hbw auto person trips',
                              'purpose': ['HBWH'],
                              'mode': range(1, 7),
                              'format': 'PA'},
                 'hbs_auto': {'description': 'total daily hbs auto person trips',
                              'purpose': ['HBS'],
                              'mode': range(1, 7),
                              'format': 'PA'},
                 'hbo_auto': {'description': 'total daily hbo auto person trips',
                              'purpose': ['HBO'],
                              'mode': range(1, 7),
                              'format': 'PA'},
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
                             'format': 'PA'},
                 'hbw_hov2': {'description': 'total daily hbw hov2 person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [2],
                             'format': 'PA'},
                 'hbw_hov3': {'description': 'total daily hbw hov3+ person trips',
                             'purpose': ['HBWH', 'HBWL'],
                             'mode': [3],
                             'format': 'PA'},
                 'hbs_sov': {'description': 'total daily hbs sov person trips',
                             'purpose': ['HBS'],
                             'mode': [1],
                             'format': 'PA'},
                 'hbs_hov2': {'description': 'total daily hbs hov2 person trips',
                             'purpose': ['HBS'],
                             'mode': [2],
                             'format': 'PA'},
                 'hbs_hov3': {'description': 'total daily hbs hov3+ person trips',
                             'purpose': ['HBS'],
                             'mode': [3],
                             'format': 'PA'},
                 'hbo_sov': {'description': 'total daily hbo sov person trips',
                             'purpose': ['HBO'],
                             'mode': [1],
                             'format': 'PA'},
                 'hbo_hov2': {'description': 'total daily hbo hov2 person trips',
                             'purpose': ['HBO'],
                             'mode': [2],
                             'format': 'PA'},
                 'hbo_hov3': {'description': 'total daily hbo hov3+ person trips',
                             'purpose': ['HBO'],
                             'mode': [3],
                             'format': 'PA'},
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
    # Move matrices to output directories.
    for p in sorted(tripdir.glob('*.csv')):
        if p.stem in ['hbwL_all', 'hbwH_all',
                      'hbwL_auto', 'hbwH_auto']:
            p.replace(worktripdir.joinpath(p.name))
        elif p.stem in ['hbw_sov', 'hbw_hov2', 'hbw_hov3',
                        'hbs_sov', 'hbs_hov2', 'hbs_hov3',
                        'hbo_sov', 'hbo_hov2', 'hbo_hov3',
                        'nhb_sov', 'nhb_hov2', 'nhb_hov3']:
            p.replace(hovtripdir.joinpath(p.name))

def export_transit_matrices(outdir, modeller):
    """
    Export transit person trip matrices from Emme.
    """
    # Make output subdirectories.
    tripdir = outdir.joinpath('trips')
    tripdir.mkdir(exist_ok=True)
    worktripdir = tripdir.joinpath('work_trips')
    worktripdir.mkdir(exist_ok=True)
    # Construct Modeller tools.
    create_matrix = modeller.tool('inro.emme.data.matrix.create_matrix')
    compute_matrix = modeller.tool('inro.emme.matrix_calculation.matrix_calculator')
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    delete_matrix = modeller.tool('inro.emme.data.matrix.delete_matrix')
    matrix_names = {'mf40': 'hbwL_transit',
                    'mf41': 'hbwH_transit',
                    'mf42': 'hbo_transit',
                    'mf43': 'nhb_transit',
                    'mf99': 'hbw_transit'}
    # Sum low and high income home-work transit trips.
    create_matrix(matrix_id='mf99',
                  matrix_name=matrix_names['mf99'],
                  matrix_description='home-based work transit person trips')
    spec = {'type': 'MATRIX_CALCULATION',
            'expression': 'mf40 + mf41',
            'result': 'mf99'}
    compute_matrix(spec)
    # Export transit trips.
    export_matrix_data(matrices=[i for i in list(matrix_names.keys()) if i not in ['mf40', 'mf41']],
                       export_path=tripdir)
    # Export home-work transit trips by income level.
    export_matrix_data(matrices=['mf40', 'mf41'],
                       export_path=worktripdir)
    delete_matrix('mf99')