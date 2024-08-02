import sys
from pathlib import Path
import argparse
from zipfile import ZipFile

sys.path.append(str(Path(__file__).resolve().parents[3]))
from tbmtools import project as tbm
from tbmtools import transit_feed
from tbmtools.prep import scenarios

_src_dir = Path(__file__).resolve().parent
_in_dir = _src_dir.parent.joinpath('input')
_proj_dir = _src_dir.parents[3]


def main():
    # Parse arguments.
    parser = argparse.ArgumentParser(description='prepare GTFS files to be read by Emme')
    parser.add_argument('--cta_feed',
                        help='path to ZIP archive containing GTFS files for CTA')
    parser.add_argument('--metra_feed',
                        help='path to ZIP archive containing GTFS files for Metra')
    parser.add_argument('--pace_feed',
                        help='path to ZIP archive containing GTFS files for Pace')
    parser.add_argument('--nictd_feed',
                        help='path to ZIP archive containing GTFS files for NICTD')
    parser.add_argument('--out_dir',
                        help='path to output directory')
    parser.add_argument('--rail_network',
                        help='path to base network transaction file for rail nodes and links')
    parser.add_argument('--highway_network_nodes',
                        help='path to base network transaction file for bus nodes')
    parser.add_argument('--highway_network_links',
                        help='path to base network transaction file for bus links')
    parser.add_argument('--link_shape',
                        help='path to link shape transaction file')
    args = parser.parse_args()
    # Prepare transit feeds.
    feed_zips = {'cta': Path(args.cta_feed),
                 'metra': Path(args.metra_feed),
                 'pace': Path(args.pace_feed),
                 'nictd': Path(args.nictd_feed)}
    for feed_zip in feed_zips.values():
        feed_dir = _in_dir.joinpath(feed_zip.stem)
        with ZipFile(feed_zip, 'r') as zip:
            zip.extractall(path=feed_dir)
        for item in feed_dir.iterdir():
            if item.is_dir():
                for file in item.iterdir():
                    file.replace(feed_dir.joinpath(file.name))
                item.rmdir()
        transit_feed.clean_feed(feed_dir, Path(args.out_dir))
    # Start Modeller in the Emme project.
    modeller = tbm.connect(_proj_dir)
    # Construct Modeller tools.
    create_scenario = modeller.tool('inro.emme.data.scenario.create_scenario')
    # Create a GTFS scenario.
    gtfs_scenario = create_scenario('900',
                                    scenario_title='GTFS',
                                    overwrite=True)
    # Build base network.
    scenarios.build_gtfs_base_network(highway_modes=_in_dir.joinpath('modes.in'),
                                      highway_nodes=Path(args.highway_network_nodes),
                                      highway_links=Path(args.highway_network_links),
                                      turns=_in_dir.joinpath('turnp07202016.txt'),
                                      transit_modes=_in_dir.joinpath('tranmodes.txt'),
                                      rail_network=Path(args.rail_network),
                                      link_shape=Path(args.link_shape),
                                      vehicles=_in_dir.joinpath('transveh.txt'),
                                      scenario=gtfs_scenario,
                                      modeller=modeller)
    # Configure the rail network scenario for GTFS.
    scenarios.configure_gtfs_schema(scenario=gtfs_scenario,
                                    modeller=modeller)


if __name__ == '__main__':
    sys.exit(main())

