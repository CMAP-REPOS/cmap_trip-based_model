from pathlib import Path
import pandas as pd

def export(projdir, outdir, out_filename):
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
        jobfiles = sorted(pqdir.glob(f'*{t}.pq'))
        dfs = [pd.read_parquet(f).reset_index().set_index('purpose') for f in jobfiles]
        hh_type_trip_rosters.update({t: pd.concat(dfs)})
    complete_trip_roster = pd.concat(hh_type_trip_rosters, names=['hh_type'], sort=False)
    outdir.mkdir(parents=True, exist_ok=True)
    trip_roster_path = outdir.joinpath(out_filename)
    complete_trip_roster.to_csv(trip_roster_path)
    return trip_roster_path