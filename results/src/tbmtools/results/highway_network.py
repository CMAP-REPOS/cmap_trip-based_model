from pathlib import Path
import csv

def export_by_tod(outdir, scenario, format, modeller, emmebank, period=[1, 2, 3, 4, 5, 6, 7, 8]):
    """
    Write highway network and attributes to Emme transaction files or
    Esri shapefiles.

    Parameters:  outdir : str or path object
                     Path to output directory.

                 scenario : int
                     3-digit scenario number.

                 format : str
                     Should be 'transaction' for Emme transaction file
                     format or 'shape' for Esri shapefile format.

                 modeller : inro.modeller.Modeller

                 emmebank : inro.emme.database.emmebank.Emmebank

                 period : int or list of int, default [1, 2, 3, 4, 5, 6, 7, 8]
                     Time of day period(s) to export.

    Returns:     None
    """
    if isinstance(outdir, str):
        outdir = Path(outdir).resolve()
    if not isinstance(period, list):
        period = [period]
    # Make output subdirectories.
    hwydir = outdir.joinpath('networks', 'highway')
    hwydir.mkdir(parents=True, exist_ok=True)
    # Construct Modeller tools.
    copy_scen = modeller.tool('inro.emme.data.scenario.copy_scenario')
    create_attrib = modeller.tool('inro.emme.data.extra_attribute.create_extra_attribute')
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    change_scen = modeller.tool('inro.emme.data.scenario.change_primary_scenario')
    delete_scen = modeller.tool('inro.emme.data.scenario.delete_scenario')
    
    for p in period:
        # Copy scenario.
        copy_scen(from_scenario=emmebank.scenario(p),
                  scenario_id=99,
                  scenario_title=f'Copy of p{p}',
                  set_as_primary=True)
        # Create extra attribute.
        create_attrib(extra_attribute_type='LINK',
                      extra_attribute_name='@vadt',
                      extra_attribute_description=f'adt p{p}')
        # Calculate vehicle volumes.
        spec = {'type': 'NETWORK_CALCULATION',
                'result': '@vadt',
                'expression': '@avauv + @avh2v + @avh3v + @avbqv + @avlqv + (@avmqv/2) + (@avhqv/3)',
                'selections': {'link': 'all'}}
        net_calc(specification=spec)
        
        if format == 'transaction':
            # Write network transaction file.
            export_basenet(export_file=hwydir.joinpath(f'network_p{p}.txt'))
            # Write attribute transaction file.
            spec = {'type': 'NETWORK_CALCULATION',
                    'expression': '@speed + @width + @parkl + @toll + @sigic + @tipid + @ftime + @emcap + @avelw + @vadt + timau',
                    'selections': {'link': 'all'}}
            report = net_calc(specification=spec,
                              full_report=True)
            with open(hwydir.joinpath(f'attribs_p{p}.txt'), 'w', newline='') as f:
                txtwriter = csv.writer(f, delimiter=' ')
                txtwriter.writerows(report['table'])
        elif format == 'shape':
            # Clear user data attributes.
            spec1 = {'type': 'NETWORK_CALCULATION',
                    'result': 'ul1',
                    'expression': '0',
                    'selections': {'link': 'all'}}
            spec2 = spec1.copy()
            spec2['result'] = 'ul2'
            spec3 = spec1.copy()
            spec3['result'] = 'ul3'
            net_calc(specification=[spec1, spec2, spec3])
            # Store vehicle volumes.
            spec = {'type': 'NETWORK_CALCULATION',
                    'result': 'ul1',
                    'expression': '@vadt',
                    'selections': {'link': 'all'}}
            net_calc(specification=spec)
            # Remove extra attributes.
            for xattrib in emmebank.scenario(99).extra_attributes():
                emmebank.scenario(99).delete_extra_attribute(xattrib.id)
            # Set file tag.
            if p == 3:
                f_tag = 'ampk'
            elif p == 7:
                f_tag = 'pmpk'
            else:
                f_tag = 'p' + str(p)
            # Write shapefile.
            net_to_shp(export_path=hwydir.joinpath(f'highway_{f_tag}-{scenario}'))
        change_scen(p)
        delete_scen(emmebank.scenario(99))

def export_daily(outdir, scenario, format, modeller, emmebank):
    """
    Write highway network and attributes to Emme transaction files or
    Esri shapefiles.

    Parameters:  outdir : str or path object
                     Path to output directory.

                 scenario : int
                     3-digit scenario number.

                 format : str
                     Should be 'transaction' for Emme transaction file
                     format or 'shape' for Esri shapefile format.

                 modeller : inro.modeller.Modeller

                 emmebank : inro.emme.database.emmebank.Emmebank

    Returns:     None
    """
    if isinstance(outdir, str):
        outdir = Path(outdir).resolve()
    # Make output subdirectories.
    hwydir = outdir.joinpath('networks', 'highway')
    hwydir.mkdir(parents=True, exist_ok=True)
    # Construct Emme Modeller tools.
    export_basenet = modeller.tool('inro.emme.data.network.base.export_base_network')
    net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
    net_to_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    change_scen = modeller.tool('inro.emme.data.scenario.change_primary_scenario')
    
    change_scen(f'{scenario}29')

    if format == 'transaction':
        # Write network transaction file.
        export_basenet(export_file=hwydir.joinpath(f'network_daily.txt'))
        # Write attribute transaction file.
        spec = {'type': 'NETWORK_CALCULATION',
                'expression': '@vadt',
                'selections': {'link': 'all'}}
        report = net_calc(specification=spec,
                            full_report=True)
        with open(hwydir.joinpath(f'attribs_daily.txt'), 'w', newline='') as f:
            txtwriter = csv.writer(f, delimiter=' ')
            txtwriter.writerows(report['table'])
    elif format == 'shape':
        # Write shapefile.
        net_to_shp(export_path=hwydir.joinpath(f'highway-{scenario}'))