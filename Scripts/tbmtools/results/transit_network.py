from pathlib import Path
import csv

def export(outdir, scenario, format, modeller, emmebank, period=[0, 5]):
    """
    Write transit network, itineraries, and attributes to Emme
    transaction files or Esri shapefiles.

    Parameters:  outdir : str or path object
                     Path to output directory.

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
    if isinstance(outdir, str):
        outdir = Path(outdir).resolve()
    if not isinstance(period, list):
        period = [period]
    # Make output subdirectories.
    transitdir = outdir.joinpath('networks', 'transit')
    transitdir.mkdir(parents=True, exist_ok=True)
    # Construct Modeller tools.
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    export_lines = modeller.tool('inro.emme.data.network.transit.export_transit_lines')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    
    transitshpdirs = list()
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
            export_basenet(export_file=transitdir.joinpath(f'network_{f_tag}.txt'),
                           scenario=s)
            # Write itinerary transaction file.
            export_lines(export_file=transitdir.joinpath(f'itins_{f_tag}.txt'),
                         scenario=s)
            # Write attribute transaction file.
            spec = {'type': 'NETWORK_CALCULATION',
                    'expression': '@ltime + @hwytm + @zfare_link',
                    'selections': {'link': 'all',
                                   'transit_line': 'all'}}
            report = net_calc(specification=spec,
                              scenario=s,
                              full_report=True)
            with open(transitdir.joinpath(f'attribs_{f_tag}.txt'), 'w', newline='') as f:
                txtwriter = csv.writer(f, delimiter=' ')
                txtwriter.writerows(report['table'])
        elif format == 'shape':
            # Write shapefiles.
            transitshpdir = transitdir.joinpath(f'transit_{f_tag}-{scenario}')
            net_to_shp(export_path=transitshpdir,
                       scenario=s)
            transitshpdirs.append(transitshpdir)
    if format == 'transaction':
        return transitdir
    elif format == 'shape':
        return tuple(transitshpdirs)