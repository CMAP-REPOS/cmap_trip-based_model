import argparse
import os

from .data_package import run_pack_data


def main():
    # Parse config args.
    config_parser = argparse.ArgumentParser()
    config_parser.add_argument('--proj_dir',
                               required=True,
                               help='path to the target TBM project directory'),
    config_args, remaining_args = config_parser.parse_known_args()
    # Set environment variables.
    os.environ["EMME_PROJECT_DIR"] = config_args.proj_dir
    # Parse subcommand args.
    subcommand_parser = argparse.ArgumentParser()
    subparsers = subcommand_parser.add_subparsers()
    # Define `pack` subcommand.
    pack_parser = subparsers.add_parser('pack')
    pack_parser.add_argument('--series',
                             help='path to a directory containing a series of model runs')
    pack_parser.set_defaults(func=run_pack_data)
    # Define `update` subcommand.
    from tbmtools.transit import update
    update_parser = subparsers.add_parser('update')
    update_parser.add_argument('--trip_aggregation',
                               action='store_true',
                               help='option to aggregate identical trips to a single transit line')
    update_parser.set_defaults(func=update.run)
    subcommand_args = subcommand_parser.parse_args(remaining_args)
    subcommand_args.func(subcommand_args)
    