"""
Utility helper functions for trip-based model data export.

This module provides generic support routines used by the TBM export
pipeline, including network attribute calculation, ZIP compression,
transit skim flagging, configuration loading, and multiprocessing
support for compression tasks.

Functions
---------
calculate_vadt
    Create and compute the `@vadt` extra attribute for a scenario network.
compress
    Compress a single file or directory into a ZIP archive.
flag_disconnected_transit_ods
    Flag disconnected transit skim O-D pairs with a numeric sentinel value.
load_config
    Load project configuration values from YAML files.
mp_compress
    Helper wrapper for calling `compress` from multiprocessing pools.
"""
from copy import deepcopy
import logging
from pathlib import Path
import shutil
from zipfile import ZipFile, ZIP_DEFLATED

import yaml


def calculate_vadt(create_attrib, net_calc, scenario, p):
    """
    Calculate vehicle average daily traffic (VADT) for a scenario and
    store it in an extra attribute.

    Parameters
    ----------
    create_attrib : callable
        Modeller tool used to create an extra link attribute.
    net_calc : callable
        Modeller tool used to perform network calculations.
    scenario : inro.emme.scenario.Scenario
        Emme scenario containing the network on which VADT is calculated.
    p : int
        Time-of-day period identifier used in the attribute description.

    Returns
    -------
    None
        This function updates the scenario network by adding the `@vadt`
        extra attribute and computing its values.
    """
    # Create extra attribute.
    create_attrib(extra_attribute_type='LINK',
                  extra_attribute_name='@vadt',
                  extra_attribute_description=f'adt p{p}',
                  scenario=scenario)
    # Calculate vehicle volumes.
    spec = {'type': 'NETWORK_CALCULATION',
            'result': '@vadt',
            'expression': '@avauv + @avh2v + @avh3v + @avbqv + @avlqv + (@avmqv/2) + (@avhqv/3)',
            'selections': {'link': 'all'}}
    net_calc(specification=spec, scenario=scenario)


def compress(out_file_name, source_path, out_dir):
    """
    Compress a file or directory into a ZIP archive.

    Parameters
    ----------
    out_file_name : str or path-like
        Name of the destination ZIP file.
    source_path : str or path-like
        Path to the file or directory to compress.
    out_dir : pathlib.Path
        Directory where the ZIP file will be written.

    Returns
    -------
    pathlib.Path
        Path to the created ZIP archive.
    """
    # Handle arguments.
    if isinstance(source_path, str):
        source_path = Path(source_path).resolve()
    # Compress content.
    out_file = out_dir.joinpath(out_file_name)
    with ZipFile(out_file, mode='w', compression=ZIP_DEFLATED, compresslevel=9) as zip:
        if source_path.is_file():
            zip.write(source_path, arcname=source_path.name)
        elif source_path.is_dir():
            for container in source_path.iterdir():
                if container.is_file():
                    zip.write(container, arcname=container.name)

    return out_file


def copy_prods_attrs(proj_dir, out_dir):
    """Copy production and attraction tables into an output directory.

    Parameters
    ----------
    proj_dir : pathlib.Path
        Project directory containing the production/attraction table files.
    out_dir : pathlib.Path
        Directory where the copied tables will be written.

    Returns
    -------
    pathlib.Path
        Path to the directory containing the copied production and attraction
        tables.
    """
    # Copy productions and attractions to output subdirectory.
    logging.info('Copying productions and attractions')
    files = [proj_dir.joinpath('Database', 'tg', 'fortran', 'TRIP49_PA_OUT.TXT'),
             proj_dir.joinpath('Database', 'tg', 'fortran', 'TRIP49_PA_WFH_OUT.TXT')]
    pa_tables_path = out_dir.joinpath('prods_attrs')
    pa_tables_path.mkdir(exist_ok=True)
    for file in files:
        shutil.copy(file, pa_tables_path)
    return pa_tables_path


def flag_disconnected_transit_ods(skim_matrix_ids, scenario_code, modeller):
    """
    Flag transit skim O-D pairs that are not connected by transit.

    Parameters
    ----------
    skim_matrix_ids : dict
        Dictionary of skim matrix IDs grouped by transit period,
        e.g. {'peak': {...}, 'off-peak': {...}}. Each group must include
        'in-vehicle minutes' and other skim matrix identifiers.
    scenario_code : int
        Scenario year code used to select the daily scenario
        (`"{scenario_code}29"`).
    modeller : inro.modeller.Modeller
        Modeller instance used to construct the matrix calculation tool.

    Returns
    -------
    None
        The function updates skim matrices in the specified scenario by
        assigning a flag value of `9999` for disconnected transit O-D pairs.
    """
    # Flag peak and off-peak transit skim O-Ds that are not connected by
    # transit. Use a flag value of 9999.
    compute_matrices = modeller.tool('inro.emme.matrix_calculation.matrix_calculator')
    for transitnet, transitnet_skims in skim_matrix_ids.items():
        # Flag O-Ds with negative or impossibly large values for in-vehicle minutes.
        spec = {'type': 'MATRIX_CALCULATION',
                'expression': '9999',
                'result': transitnet_skims['in-vehicle minutes'],
                'constraint': {'by_value': {'od_values': transitnet_skims['in-vehicle minutes'],
                                            'interval_min': 0,
                                            'interval_max': 9999,
                                            'condition': 'EXCLUDE'}}}
        compute_matrices(spec, scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))
        # Apply flag to other transit skim matrices.
        spec['constraint']['by_value']['interval_min'] = 9999
        spec['constraint']['by_value']['condition'] = 'INCLUDE'
        specs = []
        for desc, mtx_id in transitnet_skims.items():
            if desc not in ['in-vehicle minutes', 'station zone']:
                spec['result'] = mtx_id
                specs.append(deepcopy(spec))
        compute_matrices(specs, scenario=modeller.emmebank.scenario(str(scenario_code) + '29'))


def load_config(file, proj_dir):
    """
    Load model configuration values from YAML files.

    Parameters
    ----------
    file : str or pathlib.Path
        Path to the YAML configuration file to load.
    proj_dir : pathlib.Path
        Path to the Emme project directory containing
        `Database/batch_file.yaml`.

    Returns
    -------
    dict
        Configuration dictionary containing values from
        ``Database/batch_file.yaml`` and the supplied YAML file, with
        ``scenario_code`` and ``model_version`` added from the batch file.
    """
    with open(proj_dir.joinpath('Database/batch_file.yaml')) as f:
        batch_file_config = yaml.safe_load(f)
    with open(file) as f:
        config = yaml.safe_load(f)
    config['scenario_code'] = batch_file_config['scenario_code']
    config['model_version'] = batch_file_config['model_version']
    return config
