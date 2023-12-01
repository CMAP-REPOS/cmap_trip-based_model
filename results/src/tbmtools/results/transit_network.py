from pathlib import Path
import csv

def export(path, scenario, format, modeller, emmebank, period=[0, 5]):
    """
    Write transit network, itineraries, and attributes to Emme
    transaction files or Esri shapefiles.

    Parameters:  path : str or path object
                     Path to destination directory.

                 scenario : int
                     3-digit scenario number.

                 format : str
                     Should be 'transaction' for Emme transaction file
                     format or 'shape' for Esri shapefile format.

                 modeller : inro.modeller.Modeller

                 emmebank : inro.emme.database.emmebank.Emmebank

                 period : int or list of int, default [0, 5]
                     Time of day period(s) to export.

    Returns:     None
    """
    if isinstance(path, str):
        path = Path(path).resolve()
    if not isinstance(period, list):
        period = [period]

    # Construct Emme Modeller tools.
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    export_lines = modeller.tool('inro.emme.data.network.transit.export_transit_lines')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    
    for p in period:
        # Set scenario.
        s = emmebank.scenario(scenario + p)
        # Set file tag.
        if p == 0:
            f_tag = 'pk'
        elif 0 in period and p == 5:
            f_tag = 'op'
        else:
            f_tag = str(p)
        if format == 'transaction':
            # Write network transaction file.
            export_basenet(export_file=path.joinpath(f'network_{f_tag}.txt'),
                           scenario=s)
            # Write itinerary transaction file.
            export_lines(export_file=path.joinpath(f'itins_{f_tag}.txt'),
                         scenario=s)
            # Write attribute transaction file.
            spec = {'type': 'NETWORK_CALCULATION',
                    'expression': '@ltime + @hwytm + @zfare',
                    'selections': {'link': 'all',
                                   'transit_line': 'all'}}
            report = net_calc(specification=spec,
                              scenario=s,
                              full_report=True)
            with open(path.joinpath(f'attribs_{f_tag}.txt'), 'w', newline='') as f:
                txtwriter = csv.writer(f, delimiter=' ')
                txtwriter.writerows(report['table'])
        elif format == 'shape':
            # Write shapefiles.
            net_to_shp(export_path=path.joinpath(f'transit_{f_tag}-{scenario}'),
                       scenario=s)