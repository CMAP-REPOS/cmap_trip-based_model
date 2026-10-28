"""Export vehicle-trip matrices from Emme model outputs.

This module provides utilities for exporting vehicle trip matrices to CSV
files for downstream analysis and reporting.
"""

import logging


def export_auto(out_dir, scenario_code, modeller):
    """
    Export vehicle trip matrices from an emmebank to CSVs.

    Parameters
    ----------
    out_dir : str or pathlib.Path
        Root output directory where the `trips` subdirectory will be created.
    scenario_code : int
        Scenario year code used to select the daily Emme scenario.
    modeller : inro.modeller.Modeller
        Modeller instance used to construct the matrix export tool.

    Returns
    -------
    pathlib.Path
        Path to the directory containing exported vehicle trip CSV files.
    """
    logging.info('Exporting vehicle trips')
    # Make output subdirectories.
    trip_dir = out_dir.joinpath('trips')
    trip_dir.mkdir(exist_ok=True)
    # Construct Modeller tool.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    # Export vehicle trip matrices.
    matrix_names = {'mf4': 'b_truck',
                    'mf5': 'l_truck',
                    'mf6': 'm_truck',
                    'mf7': 'h_truck',
                    'mf8': 'poe_auto',
                    'mf9': 'poe_truck',
                    'mf10': 'airport'}
    export_matrix_data(matrices=list(matrix_names.keys()),
                       export_path=trip_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    
    return trip_dir
