from pathlib import Path
import pandas as pd


def get_agency(feed_dir):
    df = pd.read_csv(Path(feed_dir).joinpath('agency.txt'), skipinitialspace=True)
    return df.at[0, 'agency_name']

def cta_cleaner(file, df):
    if file.name in ['agency.txt', 'routes.txt']:
        if 'agency_id' not in df.columns:
            # Add agency_id.
            df['agency_id'] = 'CTA'
            print('added agency_id column')
            # Move agency_id to first column.
            cols = df.columns.tolist()
            df = df[cols[-1:] + cols[:-1]]
    if file.name == 'routes.txt':
        # Fill missing route_short_name values with route_id values.
        df.loc[df['route_short_name'].isna(), 'route_short_name'] = df['route_id']
        print('filled missing route_short_name values')
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
        print('patched NCS #120')
    return df

def pace_cleaner(file, df):
    if file.name == 'routes.txt':
        if 'agency_id' not in df.columns:
            # Add agency_id.
            df['agency_id'] = 'PACE'
            print('added agency_id column')
            # Move agency_id to first column.
            cols = df.columns.tolist()
            df = df[cols[-1:] + cols[:-1]]
    elif file.name == 'shapes.txt':
        if 'shape_dist_traveled' not in df.columns:
            # Add shape_dist_traveled.
            df['shape_dist_traveled'] = ''
            print('added shape_dist_traveled column')

def clean_feed(feed_dir, out_dir):
    """
    Fills missing information and corrects formatting errors in GTFS feed files.
    """
    Path(out_dir).mkdir(exist_ok=True)
    print(feed_dir)
    agency = get_agency(feed_dir)
    print(agency)
    feed_files = Path(feed_dir).glob('*.txt')
    for file in feed_files:
        if 'license' not in file.name:
            print(file.name)
            # Read file.
            df = pd.read_csv(file,
                             dtype={'route_short_name': 'str'},
                             skipinitialspace=True,
                             low_memory=False)
            # Clean file as needed based on agency.
            if agency == 'Chicago Transit Authority':
                df = cta_cleaner(file, df)
            # elif agency == 'Metra':
            #     metra_cleaner(file, df)
            elif agency == 'PACE':
                pace_cleaner(file, df)
            sub_dir = Path(out_dir, agency)
            sub_dir.mkdir(exist_ok=True)
            df.to_csv(sub_dir.joinpath(file.name), index=False)

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

def load_feed(feed_dir, outdir, modeller):
    """ Loads GTFS schedule onto scenario network."""
    import_from_gtfs = modeller.tool('inro.emme.data.network.transit.import_from_gtfs')
    agency = get_agency(feed_dir)
    if agency == 'Chicago Transit Authority':
        route_types = ['1']
        route_ids = 'ALL'
        available_info = {'route_name': 'TRANSIT_LINE#route_name',
                          'trip_id': 'TRANSIT_LINE#trip_id',
                          'stop_name': 'TRANSIT_SEGMENT#stop_name',
                          'agency_name': 'TRANSIT_LINE#agency_name',
                          'bikes_allowed': '@bikes_allowed',
                          'direction_id': '@direction',
                          'line_offset': '@first_departure',
                          'trip_headsign': 'TRANSIT_LINE#trip_headsign',
                          'wheelchair_accessible': '@wheelchair_access'}
    import_from_gtfs(gtfs_dir=feed_dir,
                     selection={'route_types': route_types,
                                'date': '20190717',
                                'start_time': '00:00',
                                'end_time': '23:59',
                                'agency_ids': [],
                                'route_ids': route_ids},
                     route_representation={'1': {'ttf': 'ft1',
                                                 'vehicle': '3'}},
                     mapmatching_criteria={'simplify_tolerance': 0.01,  # Recommended starting point of 0.01 - in miles.
                                           'max_number_of_paths': 5,  # Decrease to improve runtime - minimum of 5.
                                           'max_number_of_points': 5,  # Decrease to improve runtime - minimum of 5.
                                           'primary_radius': 0.25,  # Recommended starting point 0.625 - minimum of 0.001 in miles.
                                           'outlier_radius': 0.0625,  # Recommended starting point 0.3125 - in miles. Only used to generate warnings.
                                           'distance_factor': 1,  # Values > drift_factor prefer shorter paths - minimum of 1.
                                           'drift_factor': 12.1875,  # Values > distance factor prefer paths matching shape - minimum of 1.
                                           #'drift_factor': 1,  # Values > distance factor prefer paths matching shape - minimum of 1.
                                           'ends_drift_factor': 15,  # Applies to first and last points of paths.
                                           'shortest_path_attribute': 'length'},
                     use_shapes=True,
                     gtfs_information=available_info,
                     headway_calc_type='PROPORTION_OF_TRIP',
                     stop_variance=0,
                     split_times=True,
                     shapefile_dir=outdir,
                     scenario=modeller.emmebank.scenario(998)#  ,
                #  period_headways=[{'hdw': '@hdw_p0',
                #                    'start_time': '18:00',
                #                    'end_time': '24:00'},
                #                   {'hdw': '@hdw_p1',
                #                    'start_time': '00:00',
                #                    'end_time': '06:00'},
                #                   {'hdw': '@hdw_p2',
                #                    'start_time': '06:00',
                #                    'end_time': '09:00'},
                #                   {'hdw': '@hdw_p3',
                #                    'start_time': '09:00',
                #                    'end_time': '16:00'},
                #                   {'hdw': '@hdw_p4',
                #                    'start_time': '16:00',
                #                    'end_time': '18:00'}]
                )


