"""Export transit network data and attributes from Emme scenarios.

This module provides utilities for exporting transit networks, itineraries,
and related attributes to transaction files and shapefiles for peak and
off-peak periods.
"""

import csv
import logging
from pathlib import Path


def export_all(out_dir, scenario_code, modeller):
    """
    Export transit networks, itineraries, and attributes for peak and
    off-peak periods. Neworks are exported to both transaction file and
    shapefile formats.

    Parameters
    ----------
    out_dir : str or pathlib.Path
        Path to the root output directory where `networks/transit` will
        be created.
    scenario_code : int
        Scenario year code used to select the transit scenarios.
    modeller : inro.modeller.Modeller
        Modeller instance used to construct Emme export tools.

    Returns
    -------
    tuple[pathlib.Path, ...]
        A tuple containing the transit transaction export directory
        followed by any generated shapefile directories.
    """
    logging.info(f'Exporting transit network')
    # Normalize arguments.
    out_dir = Path(out_dir).resolve() if isinstance(out_dir, str) else out_dir
    # Make output subdirectories.
    transit_dir = out_dir.joinpath('networks', 'transit')
    transit_dir.mkdir(parents=True, exist_ok=True)
    # Construct Modeller tools.
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    export_lines = modeller.tool('inro.emme.data.network.transit.export_transit_lines')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    # Export peak and off-peak networks as Emme transaction files and
    # shapefiles.
    transitshp_dirs = []
    for n in [0, 5]:
        # Set scenario.
        s = modeller.emmebank.scenario(scenario_code + n)
        # Set file tag.
        if n == 0:
            f_tag = 'pk'
        elif n == 5:
            f_tag = 'op'
        export_transaction_files_tod(export_basenet, export_lines, net_calc, transit_dir, f_tag, s)
        transitshp_dirs.append(export_shapefiles_tod(net_to_shp, transit_dir, f_tag, s, scenario_code))
    return tuple([transit_dir] + transitshp_dirs)


def export_shapefiles_tod(net_to_shp, transit_dir, f_tag, s, scenario_code):
    """
    Export a transit network to shapefile format.

    Parameters
    ----------
    net_to_shp : callable
        Modeller tool used to export a network as shapefiles.
    transit_dir : str or pathlib.Path
        Output directory where the `transit_{f_tag}-{scenario_code}`
        shapefile directory will be created.
    f_tag : str
        Transit period file tag used in the exported directory name.
    s : inro.emme.scenario.Scenario
        Emme scenario object containing the transit network to export.
    scenario_code : int
        Scenario year code used in the exported directory name.

    Returns
    -------
    pathlib.Path
        Path to the created shapefile export directory.
    """
    # Write shapefiles.
    transitshp_dir = transit_dir.joinpath(f'transit_{f_tag}-{scenario_code}')
    net_to_shp(export_path=transitshp_dir,
               scenario=s)
    return transitshp_dir


def export_transaction_files_tod(export_basenet, export_lines, net_calc, transit_dir, f_tag, s):
    """
    Export a transit network, its itineraries, and link attributes to
    transaction files.

    Parameters
    ----------
    export_basenet : callable
        Modeller tool used to export the base transit network
        transaction file.
    export_lines : callable
        Modeller tool used to export transit itinerary transaction
        files.
    net_calc : callable
        Modeller tool used to compute transit link attributes and return
        the calculation report.
    transit_dir : str or pathlib.Path
        Output directory where `network_{f_tag}.txt`,
        `itins_{f_tag}.txt`, and `attribs_{f_tag}.txt` will be written.
    f_tag : str
        Transit period file tag used to name the exported files.
    s : inro.emme.scenario.Scenario
        Emme scenario object containing the transit network and
        itinerary data.

    Returns
    -------
    None
    """
    # Write network transaction file.
    export_basenet(export_file=transit_dir.joinpath(f'network_{f_tag}.txt'), scenario=s)
    # Write itinerary transaction file.
    export_lines(export_file=transit_dir.joinpath(f'itins_{f_tag}.txt'), scenario=s)
    # Write attribute transaction file.
    report = net_calc(specification={'type': 'NETWORK_CALCULATION',
                                     'expression': '@ltime + @hwytm + @zfare_link',
                                     'selections': {'link': 'all', 'transit_line': 'all'}},
                      scenario=s,
                      full_report=True)
    with open(transit_dir.joinpath(f'attribs_{f_tag}.txt'), 'w', newline='') as f:
        csv.writer(f, delimiter=' ').writerows(report['table'])
        