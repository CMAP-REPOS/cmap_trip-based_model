import sys
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[4]))
import argparse
from tbmtools import project as tbm
from tbmtools.prep import scenarios


def main():
    # Parse arguments and verify Emme project file.
    parser = argparse.ArgumentParser(description='import base network transaction file to emmebank')
    parser.add_argument('--project_file',
                        type=tbm.emme_project_file,
                        help='path to Emme project file')
    parser.add_argument('--network_file', help='path to base network transaction file')
    parser.add_argument('--mode_file', help='path to mode transaction file')
    parser.add_argument('--link_shape_file', help='path to link shape transaction file')
    args = parser.parse_args()
    # Start Modeller in the Emme project.
    modeller = tbm.connect(args.project_file)
    # Create a rail network scenario.
    scenarios.create_transit_scenario(args.network_file,
                                      args.mode_file,
                                      args.link_shape_file,
                                      scenario_id=999,
                                      scenario_title='Rail 2019',
                                      modeller=modeller)


if __name__ == '__main__':
    main()