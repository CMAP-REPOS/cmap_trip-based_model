import sys
import os
import shutil
import time
import datetime

import numpy as np
import pandas as pd
import argparse
import cmap_modedest

def main(*args):

    parser = argparse.ArgumentParser(
        prog='cmap_modedest',
        description='mode destination and time-of-day for CMAP trip-based model',
    )
    parser.add_argument(
        'database_dir',
        type=str,
        help='the path of the current database directory',
    )
    parser.add_argument(
        '-l', '--loglevel',
        type=int,
        help='the logging level',
        default=20,
    )
    parser.add_argument(
        '--max_zone_chunk',
        type=int,
        help='maximum number of zones to process in one chunk',
        default=30,
    )
    parser.add_argument(
        '--njobs',
        type=int,
        help='number of jobs to process in parallel',
        default=3,
    )
    parser.add_argument(
        '--short',
        type=float,
        help="""
        whether to shorten runtime by processing only a subset of origin zones,
        give a fraction less than one to run that fraction of zones spread over
        the entire region, or an integer 1 or more to run in a single process
        that number of zones starting from the first zone.
        """,
    )
    parser.add_argument(
        '--subdir',
        type=str,
        help="""cache subdirectory into which results are saved""",
        default="choice_simulator_trips_out",
    )
    parser.add_argument(
        '--check',
        help='check input arguments and quit',
        action="store_true",
    )
    parser.add_argument(
        '--rebuild',
        help='rebuild all numba caches',
        action="store_true",
    )
    parser.add_argument(
        '--profile',
        help='run in profiler mode',
        action="store_true",
    )

    args = parser.parse_args()

    log_dir = os.path.join(args.database_dir, "cache", "logs")
    os.makedirs(log_dir, exist_ok=True)
    log = cmap_modedest.log_to_stderr(
        level=args.loglevel,
        log_dir=log_dir,
    )

    if args.profile:
        from pyinstrument import Profiler
        profiler = Profiler()
        profiler.start()

    from cmap_modedest.runtime import working_dir, log_info
    log.info(time.strftime("RUN STARTED %A, %d %B %Y, %I:%M:%S %p"))
    start_time = time.time()
    try:
        log_info("###################################################################")
        log_info("##   CMAP TRIP-BASED MODEL: MODE, DESTINATION, AND TIME OF DAY   ##")
        log_info("###################################################################")
        if not os.path.isdir(args.database_dir):
            raise NotADirectoryError(args.database_dir)
        log_info(f"# database_dir = {args.database_dir}")
        log_info(f"#  (full path) = {os.path.abspath(args.database_dir)}")
        log_info(f"# --max_zone_chunk = {args.max_zone_chunk}")
        log_info(f"# --njobs = {args.njobs}")
        if args.short is not None and args.short >= 1.0 and args.njobs > 1:
            log_info(f"#         -> n_jobs is set to 1 when running in sequential short mode")
        log_info(f"# --short = {args.short}")
        log_info(f"# --subdir = {args.subdir}")
        log_info(f"# --check = {args.check}")
        log_info(f"# --rebuild = {args.rebuild}")
        log_info("###################################################################")

        if args.check: return # early exit if only checking arguments

        if args.rebuild:
            import pathlib
            here = os.path.dirname(__file__)
            for p in pathlib.Path(here).rglob('*.py[co]'):
                p.unlink()
            for p in pathlib.Path(here).rglob('*.nb[ci]'):
                p.unlink()


        from cmap_modedest.application import choice_simulator_trips_many, choice_simulator_trips, assemble_trips

        dh = working_dir(args.database_dir)

        ## Rename subdir folder if it already exists.
        cache_subdir = dh.filenames.cache_dir / args.subdir
        if os.path.exists(cache_subdir):
            n = 1
            while os.path.exists(cache_subdir.with_suffix(f".{n:03d}")):
                n += 1
            shutil.move(cache_subdir, cache_subdir.with_suffix(f".{n:03d}"))


        if args.short is None:
            otaz = None
            n_jobs = args.njobs
        elif args.short < 1.0:
            otaz = np.linspace(1, 3632, int(np.round(3632 * args.short)), dtype=int)
            n_jobs = args.njobs
        else: # args.short >= 1.0:
            otaz = np.arange(0, int(args.short)) + 1
            n_jobs = 1

        choice_simulator_trips_many(
            dh,
            otaz=otaz,
            max_chunk_size=args.max_zone_chunk,
            n_jobs=n_jobs,
            cache_subdir=args.subdir,
            with_wfh=True,
        )

        sim_trips_many = assemble_trips(
            dh,
            from_dir=dh.filenames.cache_dir / args.subdir,
            pattern="choice_simulator_trips_*.pq",
            compute_auto_propensity=False,
        )

        if True:
            from cmap_modedest.validation import validation_aggregation
            validation_aggregation(dh, sim_trips_many, to_dir=dh.filenames.cache_dir / args.subdir)

        if sim_trips_many is not None:
            s = str(
                sim_trips_many
                    .groupby(["mode", "purpose"])['trips']
                    .sum()
                    .compute()
                    .unstack(0)
                    .fillna(0)
            )
            log_info(f"Trip Summary by Mode and Purpose ------\n{s}")

        log_info("#### COMPLETED: MODE, DESTINATION, AND TIME OF DAY ####")
    finally:
        log.info(time.strftime("RUN ENDED %A, %d %B %Y, %I:%M:%S %p"))
        end_time = time.time()
        log.info(f"ELAPSED TIME {datetime.timedelta(seconds=end_time-start_time)}")

    if args.profile:
        profiler.stop()
        print(profiler.output_text(unicode=True, color=True))


if __name__ == "__main__":
    sys.exit(main())