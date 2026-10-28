import statistics

import geopandas as gpd
from inro.emme.desktop.worksheet import Column

from tbmtools.transit import config

cta_rail_route_id_codes = [('Blue','bl'),
                           ('Brn', 'br'),
                           ('G', 'gr'),
                           ('Org', 'or'),
                           ('Pink', 'pk'),
                           ('P', 'pr'),
                           ('Red', 'rd'),
                           ('Y', 'ye')]

metra_route_id_codes = [('BNSF', 'bn'),
                        ('HC', 'hc'),
                        ('MD-N', 'mn'),
                        ('MD-W', 'mw'),
                        ('ME', 'me'),
                        ('NCS', 'nc'),
                        ('RI', 'ri'),
                        ('SWS', 'sw'),
                        ('UP-N', 'un'),
                        ('UP-NW', 'nw'),
                        ('UP-W', 'uw')]

nictd_route_id_codes = [('so_shore', 'ss'),
                        ('mo_co', 'mc')]

transit_tod_hours = {'1': list(range(18, 24)) + list(range(6)),
                     '2': list(range(6, 9)),
                     '3': list(range(9, 16)),
                     '4': list(range(16, 18))}

network_modes = {'bus': 'BEPQL',
                 'rail': 'CM'}

transit_periods = ['nt', 'am', 'md', 'pm']

bus_line_table_schema = {'transit_line': 'line',
                         'description': 'description',
                         'mode': 'mode',
                         'vehicle_type': 'veh',
                         'headway': 'hdw',
                         'speed': 'speed',
                         'route_id': '#route_name',
                         'longname': '#route_name',
                         'direction': '@direction',
                         'terminal': '#trip_headsign',
                         'start': '@first_departure',
                         'starthour': '@first_departure / 60',
                         'am_share': '""',
                         'feedline': '#trip_id'}

bus_segment_table_schema = {'transit_line': 'line',
                            'itin_a': 'i',
                            'itin_b': 'j',
                            'abb': '""',
                            'itin_order': 'segno',
                            'layover': 'isLast * lay1',
                            'dwell_code': 'not(isJStop)',
                            'zone_fare': '@zfare',
                            'line_serv_time': 'us3',
                            'ttf': 'ttf',
                            'link_stops': '#stop_name',
                            'imputed': '""',
                            'dep_time': '@stop_departure',
                            'arr_time': '@stop_arrival',
                            'f_meas': '""',
                            't_meas': '""'}

rail_line_table_schema = {'tr_line': 'line',
                          'description': 'description',
                          'mode': 'mode',
                          'veh_type': 'veh',
                          'headway': 'hdw',
                          'speed': 'speed',
                          'feedline': '#trip_id',
                          'route_id': '#route_name',
                          'longname': '#route_name',
                          'direction': '@direction',
                          'terminal': '#trip_headsign',
                          'start': '@first_departure',
                          'starthour': '@first_departure / 60',
                          'am_share': '""'}

rail_segment_table_schema = {'tr_line': 'line',
                             'f_meas': '""',
                             't_meas': '""',
                             'itin_a': 'i',
                             'itin_b': 'j',
                             'it_order': 'segno',
                             'layover': 'isLast * lay1',
                             'dw_code': 'not(isJStop)',
                             'zn_fare': '@zfare',
                             'trv_time': 'us3',
                             'dep_time': '@stop_departure',
                             'arr_time': '@stop_arrival',
                             'imputed': '""'}

copy_scenario = None
create_extra_attribute = None
network_calculator = None
export_transit_lines = None


def construct_tool_objects(modeller_object):
    global copy_scenario, create_extra_attribute, network_calculator, export_transit_lines
    copy_scenario = modeller_object.tool('inro.emme.data.scenario.copy_scenario')
    create_extra_attribute = modeller_object.tool('inro.emme.data.extra_attribute.create_extra_attribute')
    network_calculator = modeller_object.tool('inro.emme.network_calculation.network_calculator')
    export_transit_lines = modeller_object.tool('inro.emme.data.network.transit.export_transit_lines')


def recode_transit_line_ids(scenario):
    route_id_codes = dict(cta_rail_route_id_codes + metra_route_id_codes + nictd_route_id_codes)
    id_numbers = {}
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        if transit_line.mode.id in ['C', 'M']:
            route_id = transit_line['#route_name'].split(' - ')[0]
            id_characters = transit_line.mode.id.lower() + route_id_codes[route_id]
            if id_characters in id_numbers:
                id_numbers[id_characters] += 1
            else:
                id_numbers[id_characters] = 400
        else:
            id_characters = transit_line.mode.id.lower()
            if id_characters in id_numbers:
                id_numbers[id_characters] += 1
            else:
                id_numbers[id_characters] = 50000
        transit_line.id = id_characters + str(id_numbers[id_characters])
    scenario.publish_network(network)


def compute_transit_line_tod(scenario):
    scenario.create_extra_attribute(type='TRANSIT_SEGMENT',
                                    id='@stop_departure_hour')
    scenario.create_extra_attribute(type='TRANSIT_LINE',
                                    id='@transit_tod')
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        segment_hours = []
        for transit_segment in transit_line.segments():
            transit_segment['@stop_departure_hour'] = int(transit_segment['@stop_departure']/3600)
            segment_hours.append(transit_segment['@stop_departure_hour'])
        segment_tods = []
        for hour in segment_hours:
            for tod, tod_hours in transit_tod_hours.items():
                if hour in tod_hours:
                    segment_tods.append(tod)
        line_tod = statistics.mode(segment_tods)
        transit_line['@transit_tod'] = int(line_tod)
    scenario.publish_network(network)


def compute_metra_segment_fares(scenario, gdb):
    # Read fare zones and nodes into a dictionary.
    fare_zone_nodes = {}
    station_zone_tbl = gpd.read_file(gdb, layer="station_zone")
    for index, row in station_zone_tbl.iterrows():
        fare_zone = row["fare_zone"]
        node = row["node"]
        if fare_zone in fare_zone_nodes:
            fare_zone_nodes[fare_zone] += [node]
        else:
            fare_zone_nodes[fare_zone] = [node]
    # Read monthly zonal fares into a dictionary.
    zone_fares = {}
    zone_fare_tbl = gpd.read_file(gdb, layer="zone_fare")
    for index, row in zone_fare_tbl.iterrows():
        dest_zone = row["destination_zone"]
        monthly_fare_from_a = row["monthly"]
        zone_fares[dest_zone] = monthly_fare_from_a
    # Update @zfare for Metra segments.
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        if transit_line.mode.id == 'M' and not transit_line.description.startswith('SSL'):
            i_fare_zone = None
            j_fare_zone = None
            for segment in transit_line.segments():
                for fare_zone, nodes in fare_zone_nodes.items():
                    if segment.i_node.id in nodes:
                        i_fare_zone = fare_zone
                    if segment.j_node.id in nodes:
                        j_fare_zone = fare_zone
                if i_fare_zone != j_fare_zone and j_fare_zone is not None:
                    marginal_fare = abs(zone_fares[i_fare_zone] - zone_fares[j_fare_zone])
                    segment['@zfare'] = marginal_fare * 100 / 40
    scenario.publish_network(network)


def copy_gtfs_scenario():
    """Make a copy of the GTFS transit network scenario."""
    if config.MODELLER.emmebank.scenario('901'):
        config.MODELLER.emmebank.delete_scenario('901')
    transformed_gtfs_scenario = copy_scenario(from_scenario='900',
                                              scenario_id='901',
                                              scenario_title='Transformed GTFS Transit Network')

    return transformed_gtfs_scenario


def transfer_segment_data(transformed_gtfs_scenario):
    # Copy stop arrival time from us1.
    create_extra_attribute(extra_attribute_type='TRANSIT_SEGMENT',
                           extra_attribute_name='@stop_arrival',
                           extra_attribute_description='arrival time (seconds after midnight)',
                           overwrite=True,
                           scenario=transformed_gtfs_scenario)
    network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                      'result': '@stop_arrival',
                                      'expression': 'us1',
                                      'selections': {'transit_line': 'all',
                                                     'link': 'all'}},
                       scenario=transformed_gtfs_scenario)
    # Copy stop departure time from us2.
    create_extra_attribute(extra_attribute_type='TRANSIT_SEGMENT',
                           extra_attribute_name='@stop_departure',
                           extra_attribute_description='departure time (seconds after midnight)',
                           overwrite=True,
                           scenario=transformed_gtfs_scenario)
    network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                      'result': '@stop_departure',
                                      'expression': 'us2',
                                      'selections': {'transit_line': 'all',
                                                     'link': 'all'}},
                       scenario=transformed_gtfs_scenario)
    # Copy inter-stop service time from us3.
    network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                      'result': '@stop_time',
                                      'expression': 'us3',
                                      'selections': {'transit_line': 'all',
                                                     'link': 'all'}},
                       scenario=transformed_gtfs_scenario)
    # Copy inter-stop service time to us1.
    network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                      'result': 'us1',
                                      'expression': 'us3',
                                      'selections': {'transit_line': 'all',
                                                     'link': 'all'}},
                       scenario=transformed_gtfs_scenario)


def transform_network(transformed_gtfs_scenario, trip_aggregation):
    # Transform line IDs.
    recode_transit_line_ids(transformed_gtfs_scenario)
    transfer_segment_data(transformed_gtfs_scenario)
    # If trips are aggregated, combine night headways. Otherwise, add
    # transit TOD to each trip.
    # if transformed_gtfs_scenario.extra_attribute('@hdw_nt1') and transformed_gtfs_scenario.extra_attribute('@hdw_nt2'):
    if trip_aggregation:
        transformed_gtfs_scenario.create_extra_attribute(type='TRANSIT_LINE',
                                                         id='@hdw_nt')
        network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                          'result': '@hdw_nt',
                                          'expression': '999 .min. (720 / (((@hdw_nt1 < 999) * (360 / @hdw_nt1)) + ((@hdw_nt2 < 999) * (360 / @hdw_nt2))))',
                                          'selections': {'transit_line': 'all'}},
                           scenario=transformed_gtfs_scenario)
    else:
        compute_transit_line_tod(transformed_gtfs_scenario)
    # Add fares to Metra segments.
    compute_metra_segment_fares(transformed_gtfs_scenario,
                                             gdb=config.mrn)
    # Copy zone fare to us2.
    network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                      'result': 'us2',
                                      'expression': '@zfare',
                                      'selections': {'transit_line': 'all',
                                                     'link': 'all'}},
                       scenario=transformed_gtfs_scenario)
    # Set layover and dwell time.
    network = transformed_gtfs_scenario.get_network()
    for transit_line in network.transit_lines():
        transit_line.layover_time = 3
        for segment in transit_line.segments():
            if segment.allow_alightings or segment.allow_boardings:
                segment.dwell_time = 0.01
    transformed_gtfs_scenario.publish_network(network)


def export_itineraries(transit_networks, trip_aggregation, transformed_gtfs_scenario):
    """Export transit lines by mode and TOD."""
    for network in transit_networks:
        modes = network_modes[network]
        for tod in range(1, 5):
            if trip_aggregation:
                # Copy TOD headway to hdw.
                network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                                  'result': 'hdw',
                                                  'expression': f'@hdw_{transit_periods[tod - 1]}',
                                                  'selections': {'transit_line': 'all'}},
                                   scenario=transformed_gtfs_scenario)
                export_transit_lines(selection=f'mode={modes} and hdw=0,720',
                                     export_file=str(config.PROJ_DIR.joinpath('output', f'{network}.itinerary_{tod}')),
                                     scenario=transformed_gtfs_scenario,
                                     include_first_hidden_data=False)
            else:
                export_transit_lines(selection=f'mode={modes} and @transit_tod={tod}',
                                     export_file=str(config.PROJ_DIR.joinpath('output', f'{network}.itinerary_{tod}')),
                                     scenario=transformed_gtfs_scenario,
                                     include_first_hidden_data=False)


def export_data_tables(transformed_gtfs_scenario, transit_network):
    config.MODELLER.desktop.data_explorer().replace_primary_scenario(transformed_gtfs_scenario)

    tbl_specs = {"bus": {"filter": 'match("BEPQL", mode)',
                         "schema": {"line": bus_line_table_schema,
                                    "segment": bus_segment_table_schema},
                         "integer_fields": {"line": ['vehicle_type', 'direction'],
                                            "segment": ['itin_a', 'itin_b', 'itin_order', 'layover', 'dwell_code', 'ttf']}},
                 "rail": {"filter": 'match("CM", mode)',
                          "schema": {"line": rail_line_table_schema,
                                     "segment": rail_segment_table_schema},
                          "integer_fields": {"line": ['veh_type', 'direction'],
                                             "segment": ['itin_a', 'itin_b', 'it_order', 'layover', 'dw_code']}}}

    print(f"Exporting {transit_network} data tables")

    line_table = config.MODELLER.desktop.project.new_network_table(type='TRANSIT_LINE')
    line_table.par('Filter').set(tbl_specs[transit_network]["filter"])
    for n, (field, attribute) in enumerate(tbl_specs[transit_network]["schema"]["line"].items()):
        column = Column()
        column.name = field
        column.expression = attribute
        line_table.add_column(index=n, column_settings=column)
    line_data = line_table.get_data()
    for attribute in line_data.attributes():
        if attribute.name in tbl_specs[transit_network]["integer_fields"]["line"]:
            attribute.change_atype('INTEGER')
        if attribute.name == 'geometry':
            print(attribute.geometry_type, attribute.spatial_reference.name)
    line_data.export_to_csv(filename=str(config.PROJ_DIR.joinpath('output', f'{transit_network}_lines_current.csv')), separator=',')

    segments_table = config.MODELLER.desktop.project.new_network_table(type='TRANSIT_SEGMENT')
    segments_table.par('Filter').set(f'{tbl_specs[transit_network]["filter"]} && not(isHidden)')
    for n, (field, attribute) in enumerate(tbl_specs[transit_network]["schema"]["segment"].items()):
        column = Column()
        column.name = field
        column.expression = attribute
        segments_table.add_column(index=n, column_settings=column)
    segments_data = segments_table.get_data()
    for attribute in segments_data.attributes():
        if attribute.name in tbl_specs[transit_network]["integer_fields"]["segment"]:
            attribute.change_atype('INTEGER')
        if attribute.name == 'geometry':
            print(attribute.geometry_type, attribute.spatial_reference.name)
    segments_data.export_to_csv(filename=str(config.PROJ_DIR.joinpath('output', f'{transit_network}_segments_current.csv')), separator=',')


def run(transit_networks, trip_aggregation):
    construct_tool_objects(config.MODELLER)
    transformed_gtfs_scenario = copy_gtfs_scenario()
    transform_network(transformed_gtfs_scenario, trip_aggregation)
    export_itineraries(transit_networks, trip_aggregation, transformed_gtfs_scenario)
    # Export data tables for MHN and MRN.
    if not trip_aggregation:
        for transit_network in transit_networks:
            export_data_tables(transformed_gtfs_scenario, transit_network)
