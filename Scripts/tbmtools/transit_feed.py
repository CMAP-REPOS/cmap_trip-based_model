from pathlib import Path
import statistics
import pandas as pd

agency_modes = dict(CTA=[
                         'rail',
                         'express_bus',
                         'regular_bus'
                        ],
                    METRA=['rail'],
                    PACE=[
                          'local_bus',
                          'express_bus',
                          'regular_bus'
                         ],
                    NICTD=['rail'])

def get_agency_name(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('agency.txt'), skipinitialspace=True)
    return df.at[0, 'agency_name']

def get_agency_id(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('agency.txt'), skipinitialspace=True)
    return df.at[0, 'agency_id']

def get_route_short_names(feed_dir):
    df = pd.read_csv(feed_dir + '/routes.txt',
                     dtype={'route_short_name': 'str'})
    route_short_names = df['route_short_name'].tolist()
    return route_short_names

def get_route_ids(feed_dir):
    df = pd.read_csv(feed_dir + '/routes.txt',
                     dtype={'route_id': 'str'})
    route_ids = df['route_id'].tolist()
    return route_ids

def get_trip_ids(feed_dir):
    df = pd.read_csv(feed_dir + '/trips.txt',
                     dtype={'trip_id': 'str'})
    trip_ids = df['trip_id'].tolist()
    return trip_ids

# def get_route_id(feed_dir, route_short_name):
#     df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
#     s = df.loc[df['route_short_name'] == route_short_name, 'route_id']
#     return s.iat[0]

def get_route_short_name(feed_dir, route_id):
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    s = df.loc[df['route_id'] == route_id, 'route_short_name']
    return s.iat[0]

def get_shape_ids(feed_dir, route_id):
    df = pd.read_csv(Path(feed_dir).joinpath('trips.txt'), skipinitialspace=True)
    s = df.loc[df['route_id'] == route_id, 'shape_id']
    return s.drop_duplicates().to_list()

def get_shape(feed_dir, shape_id):
    df = pd.read_csv(Path(feed_dir).joinpath('shapes.txt'), skipinitialspace=True)
    return df.loc[df['shape_id'] == shape_id]

def has_shape(feed_dir, route_id):
    shape_ids = get_shape_ids(feed_dir, route_id)
    missing = []
    for shape_id in shape_ids:
        shape_df = get_shape(feed_dir, shape_id)
        if shape_df.empty:
            missing.append(shape_id)
    if len(missing) > 0:
        print('missing=', missing)
        return False
    else:
        return True

def cta_cleaner(file, df):
    if file.name in ['agency.txt', 'routes.txt']:
        if 'agency_id' not in df.columns:
            # Add agency_id.
            df['agency_id'] = 'CTA'
            print(file.name, '- added agency_id column')
            # Move agency_id to first column.
            cols = df.columns.tolist()
            df = df[cols[-1:] + cols[:-1]]
    if file.name == 'routes.txt':
        # Fill missing route_short_name values with route_id values.
        df.loc[df['route_short_name'].isna(), 'route_short_name'] = df['route_id']
        print(file.name, '- filled missing route_short_name values')
    return df

def metra_cleaner(file, df):
    if file.name == 'stop_times.txt':
        # Patch for NCS #120.
        # https://gist.github.com/ianleighton/5770478
        # https://metra.com/sites/default/files/metra_46624_fm00_tt_proof_1.pdf
        # https://en.wikipedia.org/wiki/North_Central_Service
        df = pd.concat([df,
                        pd.DataFrame.from_dict({'row_1': ['NCS_NC120_V1_AA', '19:33:00', '19:33:00', 'LIBERTYVIL', 10, 0, 0, 0, 0, 1, 0],
                                                'row_2': ['NCS_NC120_V1_AA', '19:43:00', '19:43:00', 'LAKEFRST', 12, 0, 0, 0, 0, 1, 0]},
                                                orient='index',
                                                columns=['trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'pickup_type', 'drop_off_type', 'center_boarding', 'south_boarding', 'bikes_allowed', 'notice'])])
        print(file.name, '- patched NCS #120')
    return df

def pace_cleaner(file, df):
    if file.name == 'routes.txt':
        if 'agency_id' not in df.columns:
            # Add agency_id.
            df['agency_id'] = 'PACE'
            print(file.name, '- added agency_id column')
            # Move agency_id to first column.
            cols = df.columns.tolist()
            df = df[cols[-1:] + cols[:-1]]
        if len(df['route_short_name'].isna()) > 0:
            # Fill missing route_short_name values with route_id values.
            df.loc[df['route_short_name'].isna(), 'route_short_name'] = df['route_id'].apply(lambda x: x.split('-')[0])
            print(file.name, '- filled missing route_short_name values')
    elif file.name == 'shapes.txt':
        if 'shape_dist_traveled' not in df.columns:
            # Add shape_dist_traveled.
            df['shape_dist_traveled'] = ''
            print(file.name, '- added shape_dist_traveled column')
    return df

def nictd_cleaner(file, df, feed_dir):
    if file.name == 'trips.txt':
        stop_times_df = pd.read_csv(feed_dir.joinpath('stop_times.txt'))
        no_stop_times = df[~df['trip_id'].isin(stop_times_df['trip_id'])]
        df = df[~df['trip_id'].isin(no_stop_times['trip_id'])]
        print(f"{file.name} - dropped trips without stop times: {no_stop_times['trip_id'].tolist()}")
    return df

def clean_feed(feed_dir, out_dir):
    """
    Fills missing information and corrects formatting errors in GTFS feed files.
    """
    Path(out_dir).mkdir(exist_ok=True)
    agency_name = get_agency_name(feed_dir)
    feed_files = Path(feed_dir).glob('*.txt')
    for file in feed_files:
        if 'license' not in file.name:
            # Read file.
            df = pd.read_csv(file,
                             dtype={'route_short_name': 'str'},
                             skipinitialspace=True,
                             low_memory=False)
            # Clean file as needed based on agency.
            if agency_name.upper() == 'CHICAGO TRANSIT AUTHORITY':
                df = cta_cleaner(file, df)
            # elif agency == 'Metra':
            #     metra_cleaner(file, df)
            elif agency_name.upper() == 'PACE':
                df = pace_cleaner(file, df)
            elif agency_name.upper() == 'NORTHERN INDIANA COMMUTER TRANSPORTATION DISTRICT':
                df = nictd_cleaner(file, df, feed_dir)
            clean_feed_dir = Path(out_dir, agency_name)
            clean_feed_dir.mkdir(exist_ok=True)
            df.to_csv(clean_feed_dir.joinpath(file.name), index=False)
    return clean_feed_dir

def get_seasonal_routes(feed_dir):
    agency_id = get_agency_id(feed_dir)
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    if agency_id == 'CTA':
        seasonal_routes = df.loc[(df['route_short_name'].isin(['10', '19', '130']))]
    elif agency_id == 'PACE':
        seasonal_routes = df.loc[(df['route_short_name'].isin([236, 475, 768, 769, 776]))]
    return seasonal_routes['route_id'].tolist()

def get_local_routes(feed_dir):
    agency_id = get_agency_id(feed_dir)
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    seasonal_routes = get_seasonal_routes(feed_dir)
    if agency_id == 'PACE':
        local_routes = df.loc[(df['route_long_name'].str.contains('local|shuttle|circulator|trolley', case=False))]
    return [i for i in local_routes['route_id'].tolist() if i not in seasonal_routes]

def get_express_routes(feed_dir):
    agency_id = get_agency_id(feed_dir)
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    seasonal_routes = get_seasonal_routes(feed_dir)
    if agency_id == 'CTA':
        express_routes = df.loc[(df['route_long_name'].str.contains('express|exp.', case=False))
                                | (df['route_short_name'].isin(['28', 'J14']))]
    elif agency_id == 'PACE':
        express_routes = df.loc[(df['route_long_name'].str.contains('express|pulse|limited', case=False))
                                | (df['route_short_name'].isin([604, 554, 360, 309, 318, 715, 568, 330, 379, 381, 313, 213]))
                               ]
    return [i for i in express_routes['route_id'].tolist() if i not in seasonal_routes]

def get_regular_routes(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    s = df['route_id']
    seasonal_routes = get_seasonal_routes(feed_dir)
    express_routes = get_express_routes(feed_dir)
    if get_agency_id(feed_dir) == 'PACE':
        local_routes = get_local_routes(feed_dir)
        regular_routes = [i for i in s.tolist() if i not in seasonal_routes + local_routes + express_routes]
    else:
        regular_routes = [i for i in s.tolist() if i not in seasonal_routes + express_routes]
    return regular_routes

def load_feed(feed_dir, date, scenario, modeller, trip_aggregation=False):
    """ Loads GTFS feed onto scenario network."""
    import_from_gtfs = modeller.tool('inro.emme.data.network.transit.import_from_gtfs')
    import_schedule_from_gtfs = modeller.tool('inro.emme.data.network.transit.import_schedule_from_gtfs')
    agency_id = get_agency_id(feed_dir)
    modes = agency_modes[agency_id.upper()]
    available_info = {'route_name': 'TRANSIT_LINE#route_name',
                      'trip_id': 'TRANSIT_LINE#trip_id',
                      'stop_name': 'TRANSIT_SEGMENT#stop_name',
                      'agency_name': 'TRANSIT_LINE#agency_name',
                      'bikes_allowed': '@bikes_allowed',
                      'direction_id': '@direction',
                      'line_offset': '@first_departure',
                      'trip_headsign': 'TRANSIT_LINE#trip_headsign',
                      'wheelchair_accessible': '@wheelchair_access'}
    if agency_id.upper() == 'CTA':
        del available_info['bikes_allowed']
        del available_info['trip_headsign']
    elif agency_id.upper() == 'METRA':
        del available_info['wheelchair_accessible']
        del available_info['bikes_allowed']
    elif agency_id.upper() == 'PACE':
        del available_info['wheelchair_accessible']
        del available_info['trip_headsign']
    elif agency_id.upper() == 'NICTD':
        del available_info['wheelchair_accessible']
        del available_info['bikes_allowed']
    for mode in modes:
        use_shapes=feed_dir.joinpath('shapes.txt').exists()
        if agency_id.upper() == 'CTA':
            if mode == 'rail':
                route_types=['1']
                route_ids='ALL'
                # route_ids=['Org']
                use_shapes=False
                if trip_aggregation:
                    route_rep = {'1': {'ttf': 'ft1', 'vehicle': '3'}}
                else:
                    route_rep = {'1': '3'}
            elif mode == 'express_bus':
                route_types=['3']
                route_ids=get_express_routes(feed_dir)
                # route_ids=['X49']
                if trip_aggregation:
                    route_rep = {'3': {'ttf': 'ft1', 'vehicle': '32'}}
                else:
                    route_rep = {'3': '32'}
            elif mode == 'regular_bus':
                route_types=['3']
                route_ids=get_regular_routes(feed_dir)
                # route_ids=['71']
                if trip_aggregation:
                    route_rep = {'3': {'ttf': 'ft1', 'vehicle': '26'}}
                else:
                    route_rep = {'3': '26'}
            else:
                raise ValueError(f'Unknown {agency_id} mode -- review transit_feed.agency_modes')
        elif agency_id.upper() == 'METRA':
            route_types = ['2']
            route_ids = 'ALL'
            if trip_aggregation:
                route_rep = {'2': {'ttf': 'ft1', 'vehicle': '11'}}
            else:
                route_rep =  {'2': '11'}
            use_shapes=False
        elif agency_id.upper() == 'PACE':
            if mode == 'local_bus':
                route_types = ['3']
                route_ids = get_local_routes(feed_dir)
                if trip_aggregation:
                    route_rep = {'3': {'ttf': 'ft1', 'vehicle': '30'}}
                else:
                    route_rep = {'3': '30'}
            elif mode == 'express_bus':
                route_types = ['3']
                route_ids = get_express_routes(feed_dir)
                # route_ids = ['607-377']
                if trip_aggregation:
                    route_rep = {'3': {'ttf': 'ft1', 'vehicle': '29'}}
                else:
                    route_rep = {'3': '29'}
            elif mode == 'regular_bus':
                route_types = ['3']
                route_ids = get_regular_routes(feed_dir)
                # route_ids = ['552-377']
                if trip_aggregation:
                    route_rep = {'3': {'ttf': 'ft1', 'vehicle': '28'}}
                else:
                    route_rep = {'3': '28'}
            else:
                raise ValueError(f'Unknown {agency_id} mode -- review transit_feed.agency_modes')
        elif agency_id.upper() == 'NICTD':
            route_types = ['2']
            route_ids = 'ALL'
            if trip_aggregation:
                route_rep = {'2': {'ttf': 'ft1', 'vehicle': '11'}}
            else:
                route_rep = {'2': '21'}
        else:
            raise ValueError('Unknown agency ID -- review agency.txt')
        feed_dir.joinpath('modes', mode).mkdir(parents=True, exist_ok=True)
        selection={'route_types': route_types,
                   'date': date,
                   'start_time': '00:00',
                   'end_time': '23:59',
                   'agency_ids': [agency_id],
                   'route_ids': route_ids}
        if agency_id.upper() == 'METRA':
            mapmatching_criteria={'simplify_tolerance': 0.01,  # Recommended starting point of 0.01 - in miles. Calibrated using PACE 607.
                                  'max_number_of_paths': 5,  # Decrease to improve runtime - minimum of 5.
                                  'max_number_of_points': 5,  # Decrease to improve runtime - minimum of 5.
                                  'primary_radius': 0.625,  # Recommended starting point 0.625 - minimum of 0.001 in miles. Calibrated using PACE 381.
                                  'outlier_radius': 0.3125,  # Recommended starting point 0.3125 - in miles. Only used to generate warnings.
                                  'distance_factor': 1,  # Values > drift_factor prefer shorter paths - minimum of 1.
                                  'drift_factor': 1,  # Values > distance factor prefer paths matching shape - minimum of 1. Calibrated using CTA 52.
                                  'ends_drift_factor': 1,  # Applies to first and last points of paths.
                                  'shortest_path_attribute': 'length'}
        else:
            mapmatching_criteria={'simplify_tolerance': 0.15,  # Recommended starting point of 0.01 - in miles. Calibrated using PACE 607.
                                  'max_number_of_paths': 10,  # Decrease to improve runtime - minimum of 5.
                                  'max_number_of_points': 10,  # Decrease to improve runtime - minimum of 5.
                                  'primary_radius': 0.157,  # Recommended starting point 0.625 - minimum of 0.001 in miles. Calibrated using PACE 381.
                                  'outlier_radius': 0.0625,  # Recommended starting point 0.3125 - in miles. Only used to generate warnings.
                                  'distance_factor': 1,  # Values > drift_factor prefer shorter paths - minimum of 1.
                                  'drift_factor': 29,  # Values > distance factor prefer paths matching shape - minimum of 1. Calibrated using CTA 52.
                                  'ends_drift_factor': 30,  # Applies to first and last points of paths.
                                  'shortest_path_attribute': 'length'}
        if trip_aggregation:
            import_from_gtfs(gtfs_dir=feed_dir,
                             gtfs_information=available_info,
                             selection=selection,
                             route_representation=route_rep,  # {route type ID: {ttf: ttf ID, vehicle: vehicle ID}}.
                             mapmatching_criteria=mapmatching_criteria,
                             use_shapes=use_shapes,
                             headway_calc_type='PROPORTION_OF_TRIP',
                             stop_variance=0,
                             split_times=True,
                             shapefile_dir=feed_dir.joinpath('modes', mode),
                             scenario=scenario,
                             period_headways=[{'hdw': '@hdw_nt1',
                                               'start_time': '00:00',
                                               'end_time': '06:00'},
                                              {'hdw': '@hdw_am',
                                               'start_time': '06:00',
                                               'end_time': '09:00'},
                                              {'hdw': '@hdw_md',
                                               'start_time': '09:00',
                                               'end_time': '16:00'},
                                              {'hdw': '@hdw_pm',
                                               'start_time': '16:00',
                                               'end_time': '18:00'},
                                              {'hdw': '@hdw_nt2',
                                               'start_time': '18:00',
                                               'end_time': '24:00'}])
        else:
            import_schedule_from_gtfs(gtfs_dir=feed_dir,
                                      gtfs_information=available_info,
                                      selection=selection,
                                      route_representation=route_rep,  # {route type ID: vehicle ID}.
                                      function_id='1',
                                      overwrite=True,
                                      mapmatching_criteria=mapmatching_criteria,
                                      use_shapes=use_shapes,
                                      shapefile_dir=feed_dir.joinpath('modes', mode),
                                      scenario=scenario)

def recode_transit_line_ids(scenario):
    cta_rail_route_codes = [('Blue','bl'),
                            ('Brn', 'br'),
                            ('G', 'gr'),
                            ('Org', 'or'),
                            ('Pink', 'pk'),
                            ('P', 'pr'),
                            ('Red', 'rd'),
                            ('Y', 'ye')]
    metra_route_codes = [('BNSF', 'bn'),
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
    nictd_route_codes = [('so_shore', 'ss'),
                         ('SSL', 'ss')]
    route_codes = dict(cta_rail_route_codes + metra_route_codes + nictd_route_codes)
    id_numbers = dict()
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        if transit_line.mode.id in ['C', 'M']:
            route_short_name = transit_line['#route_name'].split(' - ')[0]
            id_characters = transit_line.mode.id.lower() + route_codes[route_short_name]
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
    transit_tod_hours = {'1': list(range(18, 24)) + list(range(0, 6)),
                         '2': list(range(6, 9)),
                         '3': list(range(9, 16)),
                         '4': list(range(16, 18))}
    scenario.create_extra_attribute(type='TRANSIT_SEGMENT',
                                    id='@stop_departure_hour')
    scenario.create_extra_attribute(type='TRANSIT_LINE',
                                    id='@transit_tod')
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        segment_hours = list()
        for transit_segment in transit_line.segments():
            transit_segment['@stop_departure_hour'] = int(transit_segment['@stop_departure']/3600)
            segment_hours.append(transit_segment['@stop_departure_hour'])
        segment_tods = list()
        for hour in segment_hours:
            for tod, tod_hours in transit_tod_hours.items():
                if hour in tod_hours:
                    segment_tods.append(tod)
        line_tod = statistics.mode(segment_tods)
        transit_line['@transit_tod'] = int(line_tod)
    scenario.publish_network(network)

def compute_metra_segment_fares(scenario, station_zone_file, zone_fare_file):
    # Read fare zones and nodes into a dictionary.
    station_zone_data = station_zone_file.read_text()
    rows = station_zone_data.splitlines()
    fare_zone_nodes = dict()
    for row in rows[1:]:
        cols = row.split(',')
        fare_zone = cols[3]
        node = cols[4]
        if fare_zone in fare_zone_nodes:
            fare_zone_nodes[fare_zone] += [node]
        else:
            fare_zone_nodes[fare_zone] = [node]
    # Read monthly zonal fares into a dictionary.
    zone_fare_data = zone_fare_file.read_text()
    rows = zone_fare_data.splitlines()
    zone_fares = dict()
    for row in rows[1:]:
        cols = row.split(',')
        dest_zone = cols[1]
        monthly_fare_from_a = float(cols[2])
        zone_fares[dest_zone] = monthly_fare_from_a
    # Update @zfare for Metra segments.
    network = scenario.get_network()
    for transit_line in network.transit_lines():
        if transit_line.mode.id == 'M' and transit_line.description.find('South Shore Line') == -1:
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
