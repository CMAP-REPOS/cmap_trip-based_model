import sys
from pathlib import Path
import argparse
from zipfile import ZipFile
import subprocess
import time
from inro.emme.desktop.worksheet import Column
sys.path.append(str(Path(__file__).resolve().parents[3]))
from tbmtools import project as tbm
from tbmtools import transit_feed
from tbmtools.prep import scenarios

_src_dir = Path(__file__).resolve().parent
_in_dir = _src_dir.parent.joinpath('input')
_out_dir = _src_dir.parent.joinpath('output')
_proj_dir = _src_dir.parents[3]


def main():
    # Start Modeller in the Emme project.
    modeller = tbm.connect(_proj_dir)
    # Construct Modeller tools.
    copy_scenario = modeller.tool('inro.emme.data.scenario.copy_scenario')
    create_extra_attribute = modeller.tool('inro.emme.data.extra_attribute.create_extra_attribute')
    network_calculator = modeller.tool('inro.emme.network_calculation.network_calculator')
    export_transit_lines = modeller.tool('inro.emme.data.network.transit.export_transit_lines')

    # Make a copy of the GTFS transit network scenario.
    if modeller.emmebank.scenario('901'):
        modeller.emmebank.delete_scenario('901')
    transformed_gtfs_scenario = copy_scenario(from_scenario='900',
                                              scenario_id='901',
                                              scenario_title='Transformed GTFS Transit Network')
    # Transform line IDs.
    transit_feed.recode_transit_line_ids(transformed_gtfs_scenario)
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
    # If trips are aggregated, combine night headways. Otherwise, add
    # transit TOD to each trip.
    if transformed_gtfs_scenario.extra_attribute('@hdw_nt1') and transformed_gtfs_scenario.extra_attribute('@hdw_nt2'):
        transformed_gtfs_scenario.create_extra_attribute(type='TRANSIT_LINE',
                                                         id='@hdw_nt')
        network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                          'result': '@hdw_nt',
                                          'expression': '999 .min. (720 / (((@hdw_nt1 < 999) * (360 / @hdw_nt1)) + ((@hdw_nt2 < 999) * (360 / @hdw_nt2))))',
                                          'selections': {'transit_line': 'all'}},
                           scenario=transformed_gtfs_scenario)
    else:
        transit_feed.compute_transit_line_tod(transformed_gtfs_scenario)
    # Add fares to Metra segments.
    transit_feed.compute_metra_segment_fares(transformed_gtfs_scenario,
                                             station_zone_file=_in_dir.joinpath('station_zone.csv'),
                                             zone_fare_file=_in_dir.joinpath('zone_fare.csv'))
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
    # Export transit lines by mode and TOD.
    network_modes = {
                     'bus': 'BEPQL',
                     'rail': 'CM'}
    transit_periods = ['nt', 'am', 'md', 'pm']
    for network, modes in network_modes.items():
        for tod in range(1, 5):
            if transformed_gtfs_scenario.extra_attribute('@hdw_nt'):
                network_calculator(specification={'type': 'NETWORK_CALCULATION',
                                                  'result': 'hdw',
                                                  'expression': f'@hdw_{transit_periods[tod - 1]}',
                                                  'selections': {'transit_line': 'all'}},
                                   scenario=transformed_gtfs_scenario)
                export_transit_lines(selection=f'mode={modes} and hdw=0,720',
                                     export_file=str(_out_dir.joinpath(f'{network}.itinerary_{tod}')),
                                     scenario=transformed_gtfs_scenario,
                                     include_first_hidden_data=False)
            else:
                export_transit_lines(selection=f'mode={modes} and @transit_tod={tod}',
                                     export_file=str(_out_dir.joinpath(f'{network}.itinerary_{tod}')),
                                     scenario=transformed_gtfs_scenario,
                                     include_first_hidden_data=False)
    # Export data tables for MHN and MRN.
    if not transformed_gtfs_scenario.extra_attribute('@hdw_nt'):
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
        modeller.desktop.data_explorer().replace_primary_scenario(transformed_gtfs_scenario)
        bus_line_table = modeller.desktop.project.new_network_table(type='TRANSIT_LINE')
        bus_line_table.par('Filter').set('match("BEPQL", mode)')
        n = 0
        for field, attribute in bus_line_table_schema.items():
            column = Column()
            column.name = field
            column.expression = attribute
            bus_line_table.add_column(index=n, column_settings=column)
            n += 1
        bus_line_data = bus_line_table.get_data()
        for attribute in bus_line_data.attributes():
            if attribute.name in ['vehicle_type', 'direction']:
                attribute.change_atype('INTEGER')
            if attribute.name == 'geometry':
                print(attribute.geometry_type, attribute.spatial_reference.name)
        bus_line_data.export_to_csv(filename=str(_out_dir.joinpath('bus_lines_current.csv')), separator=',')
        bus_segments_table = modeller.desktop.project.new_network_table(type='TRANSIT_SEGMENT')
        bus_segments_table.par('Filter').set('match("BEPQL", mode) && not(isHidden)')
        n = 0
        for field, attribute in bus_segment_table_schema.items():
            column = Column()
            column.name = field
            column.expression = attribute
            bus_segments_table.add_column(index=n, column_settings=column)
            n += 1
        bus_segments_data = bus_segments_table.get_data()
        for attribute in bus_segments_data.attributes():
            if attribute.name in ['itin_a', 'itin_b', 'itin_order', 'layover', 'dwell_code', 'ttf']:
                attribute.change_atype('INTEGER')
            if attribute.name == 'geometry':
                print(attribute.geometry_type, attribute.spatial_reference.name)
        bus_segments_data.export_to_csv(filename=str(_out_dir.joinpath('bus_segments_current.csv')), separator=',')
        rail_line_table = modeller.desktop.project.new_network_table(type='TRANSIT_LINE')
        rail_line_table.par('Filter').set('match("CM", mode)')
        n = 0
        for field, attribute in rail_line_table_schema.items():
            column = Column()
            column.name = field
            column.expression = attribute
            rail_line_table.add_column(index=n, column_settings=column)
            n += 1
        rail_line_data = rail_line_table.get_data()
        for attribute in rail_line_data.attributes():
            if attribute.name in ['veh_type', 'direction']:
                attribute.change_atype('INTEGER')
            if attribute.name == 'geometry':
                print(attribute.geometry_type, attribute.spatial_reference.name)
        rail_line_data.export_to_csv(filename=str(_out_dir.joinpath('rail_lines_current.csv')), separator=',')
        rail_segments_table = modeller.desktop.project.new_network_table(type='TRANSIT_SEGMENT')
        rail_segments_table.par('Filter').set('match("CM", mode) && not(isHidden)')
        n = 0
        for field, attribute in rail_segment_table_schema.items():
            column = Column()
            column.name = field
            column.expression = attribute
            rail_segments_table.add_column(index=n, column_settings=column)
            n += 1
        rail_segments_data = rail_segments_table.get_data()
        for attribute in rail_segments_data.attributes():
            if attribute.name in ['itin_a', 'itin_b', 'it_order', 'layover', 'dw_code']:
                attribute.change_atype('INTEGER')
            if attribute.name == 'geometry':
                print(attribute.geometry_type, attribute.spatial_reference.name)
        rail_segments_data.export_to_csv(filename=str(_out_dir.joinpath('rail_segments_current.csv')), separator=',')


if __name__ == '__main__':
    sys.exit(main())