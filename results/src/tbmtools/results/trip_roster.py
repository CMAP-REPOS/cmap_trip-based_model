import subprocess
from pathlib import Path
from textwrap import dedent
import pandas as pd
from tbmtools import config

def export(projdir, outdir):
    """
    Assemble the trip roster from parquet job files and export it to a
    CSV.

    Parameters:  projdir : str or path object
                     Path to Emme project directory.

                 outdir : str or path object
                     Path to output directory.

    Returns:     path object
                     Path to output CSV.
    """
    if isinstance(projdir, str):
        projdir = Path(projdir).resolve()
    if isinstance(outdir, str):
        outdir = Path(outdir).resolve()
    pqdir = Path(projdir).joinpath('Database/cache/choice_simulator_trips_out')
    hh_types = ['typical', 'wfh', 'deadhead']
    hh_type_trip_rosters = {}
    for t in hh_types:
        jobfiles = pqdir.glob(f'*{t}.pq')
        dfs = (pd.read_parquet(f).reset_index().set_index('purpose') for f in jobfiles)
        hh_type_trip_rosters.update({t: pd.concat(dfs)})
    complete_trip_roster = pd.concat(hh_type_trip_rosters, names=['hh_type'])
    outdir.mkdir(parents=True, exist_ok=True)
    trip_roster_path = outdir.joinpath('trip_roster.csv')
    complete_trip_roster.to_csv(trip_roster_path)
    return trip_roster_path

def run_export(projdir, outdir):
    """
    Run trip_roster.export() in a custom environment to allow access to 
    parquet engines.
    """
    
    cli = subprocess.run(f'"{config.CONDAPATH}\\Scripts\\activate.bat" {config.ENVNAME} && python -m tbmtools.results.export_trip_roster {projdir} {outdir}',
                         shell=True,
                         capture_output=True,
                         text=True)
    return cli.stdout