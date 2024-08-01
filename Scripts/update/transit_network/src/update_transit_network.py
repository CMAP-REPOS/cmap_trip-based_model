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
    # Build base networks.
    # Create a rail scenario.
    scenarios.create_transit_scenario(network_file=Path(args.rail_network),
                                      mode_file=_in_dir.joinpath('tranmodes.txt'),
                                      link_shape_file=Path(args.link_shape),
                                      scenario_id=998,
                                      scenario_title='Rail',
                                      modeller=modeller)
    # Create a bus scenario.
    scenarios.create_transit_scenario(network_file=[Path(args.highway_network_nodes), Path(args.highway_network_links)],
                                      mode_file=[_in_dir.joinpath('modes.in'), _in_dir.joinpath('tranmodes.txt')],
                                      link_shape_file=Path(args.link_shape),
                                      scenario_id=999,
                                      scenario_title='Bus',
                                      modeller=modeller,
                                      turn_file=_in_dir.joinpath('turnp07202016.txt'))
    # Load transit feeds.
    # Configure the rail network scenario for GTFS.
    scenarios.configure_gtfs_scenario(scenario_id=998, modeller=modeller)
    # Configure the bus network scenario for GTFS.
    scenarios.configure_gtfs_scenario(scenario_id=999, modeller=modeller)


if __name__ == '__main__':
    sys.exit(main())

