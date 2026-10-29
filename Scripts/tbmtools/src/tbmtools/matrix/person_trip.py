"""Export person-trip matrices from trip rosters and Emme model outputs.

This module provides utilities for assembling auto and transit person-trip
matrices, writing them to CSV files, and building a trip roster from
choice-simulator parquet outputs. It is used to convert trip-level data into
matrix-ready files for downstream analysis and reporting.
"""

import logging
import multiprocessing
import os
from pathlib import Path

import pandas as pd


def export_auto(proj_dir, out_dir, trip_roster_path):
    """
    Create auto person trip matrices from a trip roster and export the
    matrices to CSVs.

    Parameters
    ----------
    proj_dir : str or pathlib.Path
        Path to the Emme project directory. If a string is provided, it is
        converted to a resolved pathlib.Path.
    out_dir : str or pathlib.Path
        Root output directory where `trips/` and `trips/hov_trips/` are created.
    trip_roster_path : str or pathlib.Path
        Path to the CSV trip roster file used to build auto person matrices.

    Returns
    -------
    tuple[pathlib.Path, pathlib.Path]
        A tuple containing the auto trip export directory and the HOV trip
        subdirectory.

    Notes
    -----
    The function reads the roster with pandas, generates auto person trip
    matrices for multiple trip purposes and modes using `matrix_from_roster`,
    writes totals to `auto_person_trip_totals.txt`, and moves HOV matrices
    into the `hov_trips/` subdirectory.
    """
    logging.info('Exporting auto person trips')
    # Normalize arguments.
    if isinstance(proj_dir, str):
        proj_dir = Path(proj_dir).resolve()
    if isinstance(out_dir, str):
        out_dir = Path(out_dir).resolve()
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
    logging.info('Exporting auto person trips')
    report = out_dir.joinpath('auto_person_trip_totals.txt')
    if report.exists():
        report.unlink()
    # Read trip roster.
    roster = pd.read_csv(trip_roster_path)
    # Make output subdirectory.
    trip_dir = out_dir.joinpath('trips')
    trip_dir.mkdir(exist_ok=True)
    # Export specified matrices.
    args = []
    for name, spec in mtx_specs.items():
        args.append((name, spec, trip_dir, roster, report))
    with multiprocessing.Pool(processes=min(os.cpu_count(), 61)) as pool:
        pool.starmap(matrix_from_roster, args)
    # Move HOV matrices to output directory.
    hovtrip_dir  = trip_dir.joinpath('hov_trips')
    hovtrip_dir.mkdir(exist_ok=True)
    for p in sorted(trip_dir.glob('*.csv')):
        if p.stem in ['hbw_sov', 'hbw_hov2', 'hbw_hov3',
                      'hbs_sov', 'hbs_hov2', 'hbs_hov3',
                      'hbo_sov', 'hbo_hov2', 'hbo_hov3',
                      'nhb_sov', 'nhb_hov2', 'nhb_hov3']:
            p.replace(hovtrip_dir.joinpath(p.name))

    return (trip_dir, hovtrip_dir)


def export_transit(proj_dir, out_dir, scenario_code, modeller):
    """
    Export daily transit person trip matrices from an emmebank to CSVs.

    Parameters
    ----------
    proj_dir : str or pathlib.Path
        Path to the Emme project directory. If a string is provided, it is
        converted to a resolved ``pathlib.Path``.
    out_dir : str or pathlib.Path
        Root output directory where the ``trips/`` subdirectory is created.
    scenario_code : int
        Scenario year code used to select the daily Emme scenario
        (``{scenario_code}29``).
    modeller : inro.modeller.Modeller
        Modeller instance used to construct the matrix export tool.


    Returns
    -------
    pathlib.Path
        Path to the directory containing exported transit trip CSV files.

    Notes
    -----
    The function exports transit person trip matrices for fixed matrix IDs
    from the daily scenario and writes them into the ``trips/`` directory.
    """
    logging.info('Exporting transit person trips')
    # Normalize arguments.
    if isinstance(proj_dir, str):
        proj_dir = Path(proj_dir).resolve()
    if isinstance(out_dir, str):
        out_dir = Path(out_dir).resolve()
    # Make output subdirectory.
    trip_dir = out_dir.joinpath('trips')
    trip_dir.mkdir(exist_ok=True)
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
                       export_path=trip_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    
    return trip_dir


def export_trip_roster(proj_dir, out_dir, out_filename):
    """
    Assemble a trip roster from parquet job files and export it as a CSV.

    Parameters
    ----------
    proj_dir : str or pathlib.Path
        Path to the Emme project directory containing
        `Database/cache/choice_simulator_trips_out`.
    out_dir : str or pathlib.Path
        Output directory where the exported CSV file will be written.
    out_filename : str
        Name of the exported CSV file.

    Returns
    -------
    pathlib.Path
        Path to the exported trip roster CSV file.
    """
    logging.info('Exporting trip roster')
    if isinstance(proj_dir, str):
        proj_dir = Path(proj_dir).resolve()
    if isinstance(out_dir, str):
        out_dir = Path(out_dir).resolve()
    pq_dir = Path(proj_dir).joinpath('Database/cache/choice_simulator_trips_out')
    hh_types = ['typical', 'wfh', 'deadhead']
    hh_type_trip_rosters = {}
    for t in hh_types:
        jobfiles = [f for f in pq_dir.glob(f'*{t}.pq') if '_util' not in f.name]
        dfs = [pd.read_parquet(f).reset_index().set_index('purpose') for f in jobfiles]
        hh_type_trip_rosters.update({t: pd.concat(dfs)})
    complete_trip_roster = pd.concat(hh_type_trip_rosters, names=['hh_type'], sort=False)
    out_dir.mkdir(exist_ok=True)
    trip_roster_path = out_dir.joinpath(out_filename)
    complete_trip_roster.to_csv(trip_roster_path)

    return trip_roster_path


def matrix_from_roster(name, spec, out_dir, roster, report):
    """
    Export a matrix from a trip roster using a specification.

    Parameters
    ----------
    name : str
        Base file name to use for the exported CSV (e.g. "hbw_auto").
    spec : dict
        Specification dictionary containing:
            - 'description' : str
            - 'purpose' : list[str]
            - 'mode' : iterable[int]
            - 'format' : {'PA', 'OD'}
    out_dir : str or pathlib.Path
        Directory where the output CSV file will be written.
    roster : pandas.DataFrame
        Trip roster containing columns including 'purpose', 'mode', 'o_zone',
        'd_zone', and 'trips'.
    report : str or pathlib.Path
        Path to a text report file where trip totals are appended.

    Returns
    -------
    None
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
                                .to_csv(out_dir.joinpath(f'{name}.csv'))
    