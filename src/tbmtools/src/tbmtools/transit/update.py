import time
from zipfile import ZipFile

from tbmtools import utils
from tbmtools.transit import (
    config,
    gtfs_scenario,
    transit_feed,
    transit_tables,
)


def verify_env(env_name):
    python_exe = utils.verify_proenv(env_name)
    utils.install_tbmtools(python_exe)


def clean_feeds(feed_zips):
    """Extract and clean feed files."""
    cleaned_feeds = []
    for feed_zip in feed_zips:
        raw_feed_dir = config.DB_DIR.joinpath('raw_transit_feeds', feed_zip.stem)
        with ZipFile(feed_zip, 'r') as zip:
            zip.extractall(path=raw_feed_dir)
        # Handle nested directories.
        for item in raw_feed_dir.iterdir():
            if item.is_dir():
                for file in item.iterdir():
                    file.replace(raw_feed_dir.joinpath(file.name))
                item.rmdir()
        print('Cleaning', raw_feed_dir)
        cleaned_feed = transit_feed.clean(feed_dir=raw_feed_dir,
                                          out_dir=config.DB_DIR.joinpath('cleaned_transit_feeds'))
        cleaned_feeds.append(cleaned_feed)
    
    return cleaned_feeds


def load_feeds(cleaned_feeds, date, scenario, trip_aggregation):
    """Load GTFS data."""
    for feed in cleaned_feeds:
        print('Loading', feed)
        # # TODO add parameter for feeds to use different dates.
        # if transit_feed.get_agency_name(feed).upper() == 'METRA':
        #     date = '20221026'
        # else:
        #     date = args.date
        transit_feed.load_feed(feed_dir=feed,
                               date=date,
                               scenario=scenario,
                               trip_aggregation=trip_aggregation)


def verify_network(cleaned_feeds, scenario):
    """Verify network."""
    export_network_as_shapefile = config.MODELLER.tool('inro.emme.data.network.export_network_as_shapefile')
    export_network_as_shapefile(export_path=config.PROJ_DIR.joinpath('output', 'network'),
                                transit_shapes='LINES_AND_SEGMENTS',
                                scenario=scenario)
    network_shp_dir = config.PROJ_DIR.joinpath('output', 'network')
    feeds = ','.join(str(feed) for feed in cleaned_feeds)
    note_file = config.PROJ_DIR.joinpath('route_verification_notes.yml')
    print(f'Verifying {scenario.title} scenario network')
    python_command = ["python", "-m", "tbmtools.transit.verify_transit_network"]
    python_args = ["--network_shp_dir", network_shp_dir,
                   "--feeds", feeds,
                   "--notes", note_file,
                   "--out_dir", config.PROJ_DIR.joinpath('output')]
    utils.conda_env("tbmtools-transit", python_command, python_args)


def run(args):
    env_name = "tbmtools-transit"
    verify_env(env_name)
    python_command = ["python", "-m", "tbmtools.transit.extract_rail_network"]
    python_args = ["--mrn", str(config.mrn), "--year", config.year]
    utils.conda_env(env_name, python_command, python_args)
    start = time.perf_counter()

    # Clean transit feed files.
    feed_zips=[
               config.cta_feed,
               config.metra_feed,
               config.pace_feed,
               config.nictd_feed
              ]
    cleaned_feeds = clean_feeds(feed_zips)

    # # Create a new scenario to represent the GTFS transit network.
    config.init_modeller()
    create_scenario = config.MODELLER.tool('inro.emme.data.scenario.create_scenario')
    update_scenario = create_scenario('900',
                                      scenario_title='GTFS Transit Network',
                                      overwrite=True)

    # Build a base network for mapping GTFS transit lines.
    gtfs_scenario.build_base_network(rail_network_file=config.PROJ_DIR.joinpath('output', f'mrn_{config.year}.txt'),
                                     scenario=update_scenario)

    # Configure the scenario to store GTFS data.
    gtfs_scenario.configure_schema(scenario=update_scenario)

    # Scale the database to support the amount of GTFS data.
    gtfs_scenario.modify_database(emmebank=config.MODELLER.emmebank)

    # Load GTFS data.
    load_feeds(cleaned_feeds, config.date, update_scenario, args.trip_aggregation)
    end = time.perf_counter()
    elapsed = end - start
    print(f'Finished update - {elapsed / 60:.6f} minutes')

    # Verify accuracy of the transit network.
    verify_network(cleaned_feeds, update_scenario)
    transit_networks = ["rail"]
    if config.cta_feed in feed_zips or config.pace_feed in feed_zips:
        transit_networks.append("bus")
    transit_tables.run(transit_networks, args.trip_aggregation)
