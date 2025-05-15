import sys
from pathlib import Path
import argparse
from zipfile import ZipFile
import subprocess
import time
sys.path.append(str(Path(__file__).resolve().parents[3]))
from tbmtools import project as tbm
from tbmtools import transit_feed
from tbmtools.prep import scenarios

_src_dir = Path(__file__).resolve().parent
_in_dir = _src_dir.parent.joinpath('input')
_out_dir = _src_dir.parent.joinpath('output')
_proj_dir = _src_dir.parents[3]


def main():
    start = time.perf_counter()
    # Parse arguments.
    parser = argparse.ArgumentParser(description='prepare GTFS files to be read by Emme')
    parser.add_argument('--cta_feed',
                        help='name of ZIP archive containing static GTFS files for CTA')
    parser.add_argument('--metra_feed',
                        help='name of ZIP archive containing static GTFS files for Metra')
    parser.add_argument('--pace_feed',
                        help='name of ZIP archive containing static GTFS files for Pace')
    parser.add_argument('--nictd_feed',
                        help='name of ZIP archive containing static GTFS files for NICTD')
    parser.add_argument('--date',
                        help='date to use for selecting GTFS schedules in YYYYMMDD format')
    parser.add_argument('--rail_network',
                        help='path to base network transaction file for rail nodes and links')
    parser.add_argument('--highway_network_nodes',
                        help='name of base network transaction file for bus nodes')
    parser.add_argument('--highway_network_links',
                        help='name of base network transaction file for bus links')
    parser.add_argument('--link_shape',
                        help='name of link shape transaction file')
    parser.add_argument('--trip_aggregation',
                        help='Boolean value controlling number of trips represented by a transit line')
    args = parser.parse_args()
    feed_zips = {
                 'cta': _in_dir.joinpath(args.cta_feed),
                 'metra': _in_dir.joinpath(args.metra_feed),
                 'pace': _in_dir.joinpath(args.pace_feed),
                 'nictd': _in_dir.joinpath(args.nictd_feed)
                }
    # Extract and clean feed files.
    clean_feeds = []
    for feed_zip in feed_zips.values():
        feed_dir = _in_dir.joinpath(feed_zip.stem)
        with ZipFile(feed_zip, 'r') as zip:
            zip.extractall(path=feed_dir)
        for item in feed_dir.iterdir():
            if item.is_dir():  # Handle nested directories.
                for file in item.iterdir():
                    file.replace(feed_dir.joinpath(file.name))
                item.rmdir()
        print('Cleaning', feed_dir)
        clean_feeds.append(transit_feed.clean_feed(feed_dir, _out_dir))
    # Start Modeller in the Emme project.
    modeller = tbm.connect(_proj_dir)
    # Construct Modeller tools.
    create_scenario = modeller.tool('inro.emme.data.scenario.create_scenario')
    copy_scenario = modeller.tool('inro.emme.data.scenario.copy_scenario')
    change_db_dims = modeller.tool('inro.emme.data.database.change_database_dimensions')
    export_net_shp = modeller.tool('inro.emme.data.network.export_network_as_shapefile')
    # Create a GTFS scenario--
    if modeller.emmebank.scenario('900'):
        modeller.emmebank.delete_scenario('900')
    # --from transaction files--
    # gtfs_scenario = create_scenario('900',
    #                                 scenario_title='GTFS Transit Network')
    # scenarios.build_gtfs_base_network(highway_modes=_in_dir.joinpath('modes.in'),
    #                                   highway_nodes=_in_dir.joinpath(args.highway_network_nodes),
    #                                   highway_links=_in_dir.joinpath(args.highway_network_links),
    #                                   turns=_in_dir.joinpath('turnp07202016.txt'),
    #                                   transit_modes=_in_dir.joinpath('tranmodes.txt'),
    #                                   rail_network=Path(args.rail_network).resolve(),
    #                                   link_shape=_in_dir.joinpath(args.link_shape),
    #                                   vehicles=_in_dir.joinpath('transveh.txt'),
    #                                   scenario=gtfs_scenario,
    #                                   modeller=modeller)
    # sys.exit()
    # --or from an edited GFTS scenario.
    # TODO automate network edits.
    gtfs_scenario = copy_scenario(from_scenario='902',
                                  scenario_id='900',
                                  scenario_title='GTFS Transit Network')
    # Configure the rail network scenario for GTFS.
    scenarios.configure_gtfs_schema(scenario=gtfs_scenario,
                                    trip_aggregation=eval(args.trip_aggregation),
                                    modeller=modeller)
    # Adjust database dimensions to accommodate GTFS data.
    dims = modeller.emmebank.dimensions
    new_dims = False
    if dims['transit_lines'] < 40000:
        dims['transit_lines'] = 40000
        new_dims = True
    if dims['transit_segments'] < 2000000:
        dims['transit_segments'] = 2000000
        new_dims = True
    if dims['extra_attribute_values'] < 20000000:
        dims['extra_attribute_values'] = 20000000
        new_dims = True
    if new_dims:
        change_db_dims(emmebank_dimensions=dims,
                        keep_backup=False)
    # Import GTFS data.
    for feed in clean_feeds:
        print('Loading', feed)
        # TODO add parameter for feeds to use different dates.
        if transit_feed.get_agency_name(feed).upper() == 'METRA':
            date = '20221026'
        else:
            date = args.date
        transit_feed.load_feed(feed_dir=feed,
                               date=date,
                               scenario=gtfs_scenario,
                               modeller=modeller,
                               trip_aggregation=eval(args.trip_aggregation))
    # Export network to shapefile.
    export_net_shp(export_path=_out_dir.joinpath('network'),
                   transit_shapes='LINES_AND_SEGMENTS',
                   scenario=gtfs_scenario)
    end = time.perf_counter()
    elapsed = end - start
    print(f'Finished update - {elapsed / 60:.6f} minutes')
    # Verify network.
    # proenv = Path(r'C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3')
    activate_pro_env = r'"C:\Program Files\ArcGIS\Pro\bin\Python\condabin\activate.bat" "arcgispro-py3"'
    pyscript = _src_dir.joinpath('verify_transit_network.py')
    arcgis_project = _src_dir.parent.joinpath('verify_transit_network/verify_transit_network.aprx')
    network_shp_dir = _out_dir.joinpath('network')
    feeds = ','.join(str(feed) for feed in clean_feeds)
    note_file = _in_dir.joinpath('route_verification_notes.yml')
    pyargs = f'--arcgis_project "{arcgis_project}" --network_shp_dir "{network_shp_dir}" --feeds "{feeds}" --trip_aggregation "{args.trip_aggregation}" --notes "{note_file}" --out_dir "{_out_dir}"'
    # cmd = f'conda run -p "{proenv}" python {pyscript} {pyargs}'
    cmd = f'call {activate_pro_env} & python {pyscript} {pyargs}'
    print(f'Verifying {gtfs_scenario.title} scenario network')
    cmd_output = subprocess.run(cmd,
                                shell=True,
                                capture_output=True,
                                text=True)
    print(cmd_output.stdout)
    print(cmd_output.stderr)


if __name__ == '__main__':
    sys.exit(main())