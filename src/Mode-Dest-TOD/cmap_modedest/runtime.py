import os
import cmap_modedest
from pathlib import Path

from .data_handlers import DataHandler


import cmap_modedest

log = cmap_modedest.log_to_stderr(level=10)


def log_info(*args):
    """Short logging function for estimation"""
    if len(args) == 1 and isinstance(args[0], str) and args[0][0] == "#":
        log.info(args[0])
    else:
        s = "\n".join(str(i) for i in args)
        s = "\n"+s
        log.info(s.replace("\n", "\n    "))


def working_dir(*candidate_emme_database_dirs, **kwargs):
    for emme_database_dir in candidate_emme_database_dirs:
        emme_database_dir = os.path.expanduser(emme_database_dir)
        if os.path.exists(emme_database_dir): break
    else:
        raise FileNotFoundError(candidate_emme_database_dirs)
    emme_database_dir = Path(emme_database_dir)
    os.makedirs(emme_database_dir/"emmemat", exist_ok=True) # must exist even if empty
    log_info("###### Set Directories and Prep Data ######")
    dh = DataHandler(
        emme_database_dir=emme_database_dir,
        cache_dir=emme_database_dir/"cache",
        zone_shapefile=emme_database_dir/"data/distr/zone17.shp",
        #emmemat_dir=emme_database_dir/"emmemat.zarr.zip",
        **kwargs,
    )
    log_info("Data Handlers Ready")
    return dh
