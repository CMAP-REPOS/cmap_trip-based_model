"""Export highway and transit skim matrices from Emme model outputs.

This module provides helpers for exporting travel time, distance, and transit
skim matrices to CSV files for downstream analysis and reporting.
"""

import logging

from tbmtools.utils import flag_disconnected_transit_ods


def export_highway(out_dir, scenario_code, modeller):
    """
    Export highway time and distance skim matrices from an emmebank to
    CSVs.

    Parameters
    ----------
    out_dir : str or pathlib.Path
        Root output directory where the `skims/` subdirectory will be created.
    scenario_code : int
        Scenario year code used to select the daily Emme scenario (exported
        from the scenario identified as `"{scenario_code}29"`).
    modeller : inro.modeller.Modeller
        Modeller instance used to construct the matrix export tool.

    Returns
    -------
    pathlib.Path
        Path to the created `skims/` directory containing exported highway
        skim CSV files.

    Notes
    -----
    This function exports AM and MD highway skim matrices:
    - AM: time=`mf44`, distance=`mf45`
    - MD: time=`mf46`, distance=`mf47`

    The export is performed via the modeller tool
    `inro.emme.data.matrix.export_matrix_to_csv`.
    """
    logging.info('Exporting highway skims')
    skim_matrix_ids = {'am': {'time': 'mf44',
                              'distance': 'mf45'},
                       'md': {'time': 'mf46',
                              'distance': 'mf47'}}
    # Make output subdirectory.
    skim_dir = out_dir.joinpath('skims')
    skim_dir.mkdir(exist_ok=True)
    # Construct Modeller tool.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    # Export am highway skims.
    export_matrix_data(matrices=[i for i in list(skim_matrix_ids['am'].values())],
                       export_path=skim_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    # Export md highway skims.
    export_matrix_data(matrices=[i for i in list(skim_matrix_ids['md'].values())],
                       export_path=skim_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    
    return skim_dir


def export_transit(out_dir, scenario_code, modeller):
    """
    Export peak and off-peak transit skim matrices from an emmebank to
    CSV files.

    Parameters
    ----------
    out_dir : str or pathlib.Path
        Root output directory where the `skims/` subdirectory is created.
    scenario_code : int
        Scenario year code used to select the daily Emme scenario
        (`{scenario_code}29`).
    modeller : inro.modeller.Modeller
        Modeller instance used to construct the matrix export tool.

    Returns
    -------
    pathlib.Path
        Path to the created `skims/` directory containing exported transit
        skim CSV files.

    Notes
    -----
    The function flags disconnected transit O-Ds before exporting skim
    matrices for both peak and off-peak periods using the modeller tool
    `inro.emme.data.matrix.export_matrix_to_csv`.
    """
    logging.info('Exporting transit skims')
    skim_matrix_ids = {'peak': {'in-vehicle minutes': 'mf822',
                                'walk transfer minutes': 'mf823',
                                'wait time': 'mf838',
                                'priority mode': 'mf830',
                                'average fare': 'mf828',
                                'station zone': 'mf837'},
                       'off-peak': {'in-vehicle minutes': 'mf922',
                                    'walk transfer minutes': 'mf923',
                                    'wait time': 'mf938',
                                    'priority mode': 'mf930',
                                    'average fare': 'mf928',
                                    'station zone': 'mf937'}}
    flag_disconnected_transit_ods(skim_matrix_ids, scenario_code, modeller)
    # Make output subdirectory.
    skim_dir = out_dir.joinpath('skims')
    skim_dir.mkdir(exist_ok=True)
    # Construct Modeller tool.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    # Export peak transit skims.
    export_matrix_data(matrices=[i for i in list(skim_matrix_ids['peak'].values())],
                       export_path=skim_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    # Export off-peak transit skims.
    export_matrix_data(matrices=[i for i in list(skim_matrix_ids['off-peak'].values())],
                       export_path=skim_dir,
                       scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
    
    return skim_dir
