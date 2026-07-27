import argparse

from .data_package import run_pack_data

def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()
    # Define `pack` subcommand.
    pack_parser = subparsers.add_parser('pack')
    pack_parser.add_argument('--series', help='path to a directory containing a series of model runs')
    pack_parser.set_defaults(func=run_pack_data)
    args = parser.parse_args()
    args.func(args)