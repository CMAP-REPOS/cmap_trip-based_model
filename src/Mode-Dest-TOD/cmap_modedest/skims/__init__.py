import os
import zarr
from pathlib import Path
from .compression import read_compressed_skims, read_uncompressed_skims, write_compressed_skims, write_uncompressed_skims
from ..cmap_logging import getLogger

def read_skims(directory, backfill_compressed_skims=False, backfill_uncompressed_skims=False):
    """

    Parameters
    ----------
    directory : Path-like
        Where to read skims from, omitting any ".zarr" suffix.
    backfill_compressed_skims : bool, default False
        Write skims that only appear in the uncompressed directory into the
        zarr directory.
    backfill_uncompressed_skims : bool, default False
        Write skims that only appear in the compressed zarr directory into the
        uncompressed directory.

    Returns
    -------
    sh.Dataset
    """
    appended_names = []
    try:
        skims = read_compressed_skims(directory)
    except zarr.errors.GroupNotFoundError as err:
        skims = None
    except FileNotFoundError as err:
        skims = None
        backfill_compressed_skims = False
    try:
        skims, appended_names = read_uncompressed_skims(directory, skims)
    except (FileNotFoundError, NotADirectoryError) as err2:
        if skims is None:
            raise err2
    if backfill_compressed_skims and appended_names:
        write_compressed_skims(
            skims[appended_names],
            os.fspath(Path(directory).with_suffix(".zarr"))
        )
    if backfill_uncompressed_skims:
        write_uncompressed_skims(
            skims,
            directory,
        )

    return skims
