"""Export highway network data and attributes from Emme scenarios.

This module provides utilities for exporting highway networks, link
attributes, and shapefiles for multiple time-of-day periods and the daily
scenario.
"""

import csv
import logging
from pathlib import Path

from tbmtools.utils import calculate_vadt


def export_all(out_dir, scenario_code, modeller):
    """
    Export highway networks and attributes for all time-of-day periods
    and the daily scenario. Networks are exported to both transaction
    file and shapefile formats.

    Parameters
    ----------
    out_dir : str or pathlib.Path
        Path to the root output directory where `networks/highway` will
        be created.
    scenario_code : int
        Scenario year code (used as `{scenario_code}29` for the daily
        highway scenario).
    modeller : inro.modeller.Modeller
        Modeller instance used to construct Emme export tools.

    Returns
    -------
    tuple[pathlib.Path, ...]
        A tuple containing the highway transaction export directory
        followed by any generated shapefile directories.
    """
    logging.info(f'Exporting highway networks')
    # Normalize arguments.
    out_dir = Path(out_dir).resolve() if isinstance(out_dir, str) else out_dir
    # Create output subdirectories.
    hwy_dir = out_dir.joinpath('networks', 'highway')
    hwy_dir.mkdir(parents=True, exist_ok=True)
    # Construct Modeller tools.
    create_attrib = modeller.tool('inro.emme.data.extra_attribute.create_extra_attribute')
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    # Export each time of day highway network and attributes as Emme
    # transaction files and shapefiles.
    hwyshp_dirs = []
    for p in range(1, 9):
        scenario = modeller.emmebank.copy_scenario(source_id=p, destination_id=99)
        calculate_vadt(create_attrib, net_calc, scenario, p)
        export_transaction_files_tod(export_basenet, net_calc, hwy_dir, scenario, p)
        if p in [3, 7]:
            hwyshp_dirs.append(export_shapefiles_tod(net_calc, net_to_shp, out_dir, scenario, p, scenario_code))
        modeller.emmebank.delete_scenario(scenario.id)
    # Export daily highway network and attributes as Emme transaction
    # files and shapefiles.
    daily_scenario = modeller.emmebank.scenario(f'{scenario_code}29')
    export_transaction_files_day(export_basenet, net_calc, hwy_dir, daily_scenario)
    hwyshp_dirs.append(export_shapefiles_day(net_to_shp, out_dir, scenario_code, daily_scenario))
    return tuple([hwy_dir] + hwyshp_dirs)

def export_shapefiles_day(net_to_shp, out_dir, scenario_code, scenario):
    """
    Export the daily highway network to shapefile format.

    Parameters
    ----------
    net_to_shp : callable
        Modeller tool used to export a network as a shapefile.
    out_dir : str or pathlib.Path
        Output directory where the `highway-{scenario_code}` shapefile
        directory will be created.
    scenario_code : int
        Scenario year code used to name the output shapefile directory.
    scenario : inro.emme.scenario.Scenario
        Emme scenario object with the network to export.

    Returns
    -------
    pathlib.Path
        Path to the created shapefile export directory.
    """
    # Write shapefiles.
    hwyshp_dir = out_dir.joinpath(f'highway-{scenario_code}')
    net_to_shp(export_path=hwyshp_dir, scenario=scenario)

    return hwyshp_dir

def export_shapefiles_tod(net_calc, net_to_shp, out_dir, scenario, p, scenario_code):
    """
    Export a time-of-day highway network to shapefile format.

    This function clears temporary user data attributes from the scenario
    network, stores vehicle volumes in the `ul1` attribute, and exports the
    network to a shapefile directory named using the period tag and scenario
    code.

    Parameters
    ----------
    net_calc : callable
        Modeller tool used to perform network calculations.
    net_to_shp : callable
        Modeller tool used to export the network as a shapefile.
    out_dir : str or pathlib.Path
        Output directory where the `highway_{tag}-{scenario_code}` shapefile
        directory will be created.
    scenario : inro.emme.scenario.Scenario
        Emme scenario object containing the time-of-day highway network.
    p : int
        Time-of-day period identifier used to determine the shapefile tag.
    scenario_code : int
        Scenario year code used in the exported directory name.

    Returns
    -------
    pathlib.Path
        Path to the created shapefile export directory.
    """
    # Clear user data attributes.
    spec1 = {'type': 'NETWORK_CALCULATION',
            'result': 'ul1',
            'expression': '0',
            'selections': {'link': 'all'}}
    spec2 = spec1.copy()
    spec2['result'] = 'ul2'
    spec3 = spec1.copy()
    spec3['result'] = 'ul3'
    net_calc(specification=[spec1, spec2, spec3], scenario=scenario)
    # Store vehicle volumes.
    net_calc(specification={'type': 'NETWORK_CALCULATION',
                            'result': 'ul1',
                            'expression': '@vadt',
                            'selections': {'link': 'all'}},
             scenario=scenario)
    # Remove extra attributes.
    for xattrib in scenario.extra_attributes():
        scenario.delete_extra_attribute(xattrib.id)
    # Set file tag.
    if p == 3:
        f_tag = 'ampk'
    elif p == 7:
        f_tag = 'pmpk'
    else:
        f_tag = 'p' + str(p)
    # Write shapefiles.
    hwyshp_dir = out_dir.joinpath(f'highway_{f_tag}-{scenario_code}')
    net_to_shp(export_path=hwyshp_dir, scenario=scenario)
    return hwyshp_dir

def export_transaction_files_day(export_basenet, net_calc, out_dir, scenario):
    """
    Export the daily highway network and link attributes to transaction
    files.

    Parameters
    ----------
    export_basenet : callable
        Modeller tool used to export the base network transaction file.
    net_calc : callable
        Modeller tool used to compute network attributes and return the
        calculation report.
    out_dir : str or pathlib.Path
        Output directory where `network_daily.txt` and
        `attribs_daily.txt` will be written.
    scenario : inro.emme.scenario.Scenario
        Emme scenario object containing the daily highway network.

    Returns
    -------
    None
    """
    # Write network transaction file.
    export_basenet(export_file=out_dir.joinpath(f'network_daily.txt'), scenario=scenario)
    # Write attribute transaction file.
    report = net_calc(specification={'type': 'NETWORK_CALCULATION',
                                     'expression': '@vadt',
                                     'selections': {'link': 'all'}},
                      scenario=scenario,
                      full_report=True)
    with open(out_dir.joinpath(f'attribs_daily.txt'), 'w', newline='') as f:
        csv.writer(f, delimiter=' ').writerows(report['table'])

def export_transaction_files_tod(export_basenet, net_calc, out_dir, scenario, p):
    """
    Export a time-of-day highway network and its link attributes to transaction files.

    Parameters
    ----------
    export_basenet : callable
        Modeller tool used to export the base network transaction file.
    net_calc : callable
        Modeller tool used to compute network attributes and return the
        calculation report.
    out_dir : str or pathlib.Path
        Output directory where `network_p{p}.txt` and `attribs_p{p}.txt`
        will be written.
    scenario : inro.emme.scenario.Scenario
        Emme scenario object containing the time-of-day highway network.
    p : int
        Time-of-day period identifier used to name the network and attribute
        export files.

    Returns
    -------
    None
    """
    # Write network transaction file.
    export_basenet(export_file=out_dir.joinpath(f'network_p{p}.txt'), scenario=scenario)
    # Write attribute transaction file.
    report = net_calc(specification={'type': 'NETWORK_CALCULATION',
                                     'expression': '@speed + @width + @parkl + @toll + @sigic + @tipid + @ftime + @emcap + @avelw + @vadt + timau',
                                     'selections': {'link': 'all'}},
                      scenario=scenario,
                      full_report=True)
    with open(out_dir.joinpath(f'attribs_p{p}.txt'), 'w', newline='') as f:
        csv.writer(f, delimiter=' ').writerows(report['table'])