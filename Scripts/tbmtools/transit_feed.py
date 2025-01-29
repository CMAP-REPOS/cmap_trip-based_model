from pathlib import Path
import pandas as pd

agency_modes = dict(CTA=['rail', 'express_bus', 'regular_bus'],
                    METRA=['rail'],
                    PACE=['local_bus',
                          'express_bus',
                          'regular_bus'
                         ])

def get_agency_name(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('agency.txt'), skipinitialspace=True)
    return df.at[0, 'agency_name']

def get_agency_id(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('agency.txt'), skipinitialspace=True)
    return df.at[0, 'agency_id']

def get_route_id(feed_dir, route_short_name):
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    s = df.loc[df['route_short_name'] == route_short_name, 'route_id']
    return s.iat[0]

def get_shape_ids(feed_dir, route_id):
    df = pd.read_csv(Path(feed_dir).joinpath('trips.txt'), skipinitialspace=True)
    s = df.loc[df['route_id'] == route_id, 'shape_id']
    return s.drop_duplicates().to_list()

def get_shape(feed_dir, shape_id):
    df = pd.read_csv(Path(feed_dir).joinpath('shapes.txt'), skipinitialspace=True)
    return df.loc[df['shape_id'] == shape_id]

def has_shape(feed_dir, route_short_name):
    route_id = get_route_id(feed_dir, route_short_name)
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
                pace_cleaner(file, df)
            clean_feed_dir = Path(out_dir, agency_name)
            clean_feed_dir.mkdir(exist_ok=True)
            df.to_csv(clean_feed_dir.joinpath(file.name), index=False)
    return clean_feed_dir

def get_route_short_names(feed_dir):
    df = pd.read_csv(feed_dir + '/routes.txt',
                     dtype={'route_short_name': 'str'})
    route_short_names = df['route_short_name'].tolist()
    return route_short_names

def get_local_routes(feed_dir):
    agency_id = get_agency_id(feed_dir)
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    if agency_id == 'PACE':
        local_routes = df.loc[(df['route_long_name'].str.contains('circulator', case=False))
                              #| (df['route_long_name'].str.contains('exp.', case=False))
                              #| (df['route_short_name'] == '10')
                              #| (df['route_short_name'] == '28')
                              #| (df['route_short_name'] == 'J14')
                             ]
    return local_routes['route_id'].tolist()

def get_express_routes(feed_dir):
    agency_id = get_agency_id(feed_dir)
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    if agency_id == 'CTA':
        express_routes = df.loc[(df['route_long_name'].str.contains('express', case=False))
                                | (df['route_long_name'].str.contains('exp.', case=False))
                                | (df['route_short_name'] == '10')
                                | (df['route_short_name'] == '28')
                                | (df['route_short_name'] == 'J14')]
    elif agency_id == 'PACE':
        express_routes = df.loc[(df['route_long_name'].str.contains('express', case=False))
                                | (df['route_long_name'].str.contains('pulse', case=False))
                                #| (df['route_short_name'] == '100')
                                #| (df['route_short_name'] == '28')
                                #| (df['route_short_name'] == 'J14')
                               ]
    return express_routes['route_id'].tolist()

def get_regular_routes(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('routes.txt'), skipinitialspace=True)
    s = df['route_id']
    local_routes = get_local_routes(feed_dir)
    print(f'local routes: {local_routes}')
    express_routes = get_express_routes(feed_dir)
    print(f'express routes: {express_routes}')
    regular_routes = [i for i in s.tolist() if i not in local_routes + express_routes]
    return regular_routes

def get_bus_routes(agency, feeddir):
    """
    Categorizes GTFS bus routes into route types.
    Returns a dictionary containing a list of route IDs for each route type.
    """
    routesfile = feeddir.joinpath('routes.txt')
    # Read GTFS routes file.
    routes = pd.read_csv(routesfile)
    # Categorize bus routes based on agency.
    if agency == 'cta':
        # Exclude seasonal/special event/pilot routes.
        bus_routes = routes.loc[(routes['route_type'] == 3)
                                # & (~routes['route_id'].isin(['10', '19', '130', '171']))]
                                # Keep routes for PART 50% scenario.
                                & (routes['route_id'].isin(['3', '4', '8', '9', '12',
                                                            '20', '22', '36', '49', '50',
                                                            '53', '54', '60', '63', '66',
                                                            '72', '74', '76', '77', '79',
                                                            '80', '81', '82', '85', '91',
                                                            '92', '94', '146', '147', '151',
                                                            '152', '155']))]
        # Express routes.
        express_routes = bus_routes.loc[(bus_routes['route_long_name'].str.contains('express', case=False))
                                        | (bus_routes['route_long_name'].str.contains('exp.', case=False))
                                        | (bus_routes['route_id'] == 'J14')
                                        | (bus_routes['route_id'] == '28')]
        # Regular routes.
        regular_routes = bus_routes.loc[~bus_routes['route_id'].isin(express_routes['route_id'])]
        # Create dictionary of categorized routes.
        route_lists = {'regular': regular_routes['route_id'].tolist(),
                       'express': express_routes['route_id'].tolist(),
                       'local': None}
    elif agency == 'pace':
        # Exclude seasonal/special event/pilot/unfixed routes.
        bus_routes = routes.loc[(routes['route_type'] == 3)
                                # & (~routes['route_short_name'].isin(['182', '183', '184', '185', '186', '187']))]
                                # Keep routes for PART 50% scenario.
                                & (routes['route_short_name'].isin(['100', '208', '213', '215', '223',
                                                                    '226', '250', '272', '290', '301',
                                                                    '303', '307', '309', '313', '316',
                                                                    '318', '322', '330', '331', '349',
                                                                    '350', '352', '353', '359', '364',
                                                                    '379', '381', '383', '385', '386',
                                                                    '422', '423', '565', '568', '569',
                                                                    '572', '606', '755', '811', '850']))]
        # Local routes.
        local_routes = bus_routes.loc[(bus_routes['route_long_name'].str.contains('local', case=False))
                                      | (bus_routes['route_long_name'].str.contains('shuttle', case=False))
                                      | (bus_routes['route_long_name'].str.contains('circulator', case=False))
                                      | (bus_routes['route_long_name'].str.contains('direct', case=False))
                                      | (bus_routes['route_short_name'].isin(['412', '620', '640']))]
        # Express routes.
        express_routes = bus_routes.loc[(bus_routes['route_long_name'].str.contains('express', case=False))
                                        | (bus_routes['route_long_name'].str.contains('limited', case=False))
                                        | (bus_routes['route_short_name'].isin(['604', '850', '851', '360']))
                                        & (~bus_routes['route_id'].isin(local_routes['route_id']))]
        # Regular routes.
        regular_routes = bus_routes.loc[(~bus_routes['route_id'].isin(local_routes['route_id']))
                                        & (~bus_routes['route_id'].isin(express_routes['route_id']))]
        # Create dictionary of categorized routes.
        route_lists = {'regular': regular_routes['route_id'].tolist(),
                       'express': express_routes['route_id'].tolist(),
                       'local': local_routes['route_id'].tolist()}
    return route_lists

def load_feed(feed_dir, date, scenario, modeller):
    """ Loads GTFS schedule onto scenario network."""
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
        if agency_id.upper() == 'CTA':
            if mode == 'rail':
                route_types=['1']
                route_ids='ALL'
                route_rep={'1': '3'}
            elif mode == 'express_bus':
                route_types=['3']
                route_ids=get_express_routes(feed_dir)
                # route_ids=['X49']
                route_rep={'3': '32'}
            elif mode == 'regular_bus':
                route_types=['3']
                route_ids=get_regular_routes(feed_dir)
                # route_ids=['1', '5', '92']
                route_rep={'3': '26'}
            else:
                raise ValueError(f'Unknown {agency_id} mode -- review transit_feed.agency_modes')
        elif agency_id.upper() == 'METRA':
            route_types = ['2']
            route_ids = 'ALL'
            route_rep =  {'2': '11'}
        elif agency_id.upper() == 'PACE':
            if mode == 'local_bus':
                route_types = ['3']
                route_ids = get_local_routes(feed_dir)
                route_rep = {'3': '30'}
            elif mode == 'express_bus':
                route_types = ['3']
                route_ids = get_express_routes(feed_dir)
                route_rep = {'3': '29'}
            elif mode == 'regular_bus':
                route_types = ['3']
                route_ids = get_regular_routes(feed_dir)
                route_rep = {'3': '28'}
            else:
                raise ValueError(f'Unknown {agency_id} mode -- review transit_feed.agency_modes')
        elif agency_id.upper() == 'NICTD':
            route_types = ['2']
            route_rep = {'2': '21'}
        else:
            raise ValueError('Unknown agency ID -- review agency.txt')
        feed_dir.joinpath('modes', mode).mkdir(parents=True, exist_ok=True)
        import_schedule_from_gtfs(gtfs_dir=feed_dir,
                                  gtfs_information=available_info,
                                  selection={'route_types': route_types,
                                             'date': date,
                                             'start_time': '00:00',
                                             'end_time': '23:59',
                                             'agency_ids': [agency_id],
                                             'route_ids': route_ids},
                                  route_representation=route_rep,  # {route type ID: vehicle ID}.
                                  function_id='9',
                                  overwrite=True,
                                  mapmatching_criteria={'simplify_tolerance': 0.01,  # Recommended starting point of 0.01 - in miles.
                                                        'max_number_of_paths': 7,  # Decrease to improve runtime - minimum of 5.
                                                        'max_number_of_points': 7,  # Decrease to improve runtime - minimum of 5.
                                                        'primary_radius': 0.25,  # Recommended starting point 0.625 - minimum of 0.001 in miles.
                                                        'outlier_radius': 0.0625,  # Recommended starting point 0.3125 - in miles. Only used to generate warnings.
                                                        'distance_factor': 1,  # Values > drift_factor prefer shorter paths - minimum of 1.
                                                        'drift_factor': 6,  # Values > distance factor prefer paths matching shape - minimum of 1.
                                                        'ends_drift_factor': 15,  # Applies to first and last points of paths.
                                                        'shortest_path_attribute': 'length'},
                                  use_shapes=feed_dir.joinpath('shapes.txt').exists(),
                                  shapefile_dir=feed_dir.joinpath('modes', mode),
                                  scenario=scenario)



