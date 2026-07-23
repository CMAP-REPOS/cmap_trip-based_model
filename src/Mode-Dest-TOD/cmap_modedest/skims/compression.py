import os
import xarray as xr
import sharrow as sh
import numpy as np
import pandas as pd
import zarr
from pathlib import Path
import logging

log = logging.getLogger('CMAP')


def compress_skim_dir(directory, output="zarr"):
    """
    Compress the `emmemat` skims directory using zarr.

    Parameters
    ----------
    directory : path to emmemat directory
    output : {"zarr.zip", "zarr"}
        File format to use for compression.  Zarr.zip puts all the
        piece in one big zip directory, while plain Zarr uses the
        native file system to store a bunch of pieces.  Both methods
        use the same compression.

    Returns
    -------
    Dataset
    """

    if output not in ("zarr", "zarr.zip"):
        raise NotImplementedError(output)

    if output == "zarr":
        if not os.path.exists(directory+".zarr"):
            os.makedirs(directory+".zarr")
    elif output == "zarr.zip":
        if os.path.exists(directory+".zarr.zip"):
            raise FileExistsError(directory+".zarr.zip")

    master = {}
    for f in os.walk(directory):
        for fi in f[2]:
            if ".emx" in fi:
                arr = np.fromfile(fi, dtype='f4')
                side = int(np.sqrt(arr.size))
                arr = arr.reshape(side, side)
                tazrange = pd.RangeIndex(1, side+1)
                master[fi.replace(".emx", "")] = xr.DataArray(
                    arr,
                    dims=['otaz', 'dtaz'],
                    coords={'otaz': tazrange, 'dtaz': tazrange}
                )

    master = sh.Dataset(master)

    if output == "zarr":
        master.to_zarr(directory+".zarr", mode='a')
    elif output == "zarr.zip":
        with zarr.ZipStore(directory+".zarr.zip", mode='w') as store:
            master.to_zarr(store)
    return master


def read_uncompressed_skims(directory, dataset=None, overload=True):
    """
    Read emmemat skims into a Dataset.

    Parameters
    ----------
    directory : Path-like
        path to emmemat directory
    dataset : Dataset, optional
        Append to these skims.  Only skims not already available
        will be loaded.
    overload : bool, default True
        Whether to overload existing skims in `dataset`.  If False,
        the existing skims (generally, those loaded from the compressed
        skims) have priority and are retained.  If True, any uncompressed
        skims have priority and existing data is overwritten in memory.

    Returns
    -------
    sh.Dataset
        Loaded skims
    appended_names : List
        Names of uncompressed skims that were added
    """
    appended_names = []
    if not os.path.isdir(directory):
        raise NotADirectoryError(directory)
    if dataset is None:
        master = {}
    else:
        master = dataset
    log.info(f'reading uncompressed skims from {directory}')
    for f in os.walk(directory):
        for fi in f[2]:
            if ".emx" in fi and (fi.replace(".emx", "") not in master or overload):
                arr = np.memmap(os.path.join(f[0], fi), dtype='f4', mode='c')
                side = int(np.sqrt(arr.size))
                arr = arr.reshape(side, side)
                tazrange = pd.RangeIndex(1, side+1)
                master[fi.replace(".emx", "")] = xr.DataArray(
                    arr,
                    dims=['otaz', 'dtaz'],
                    coords={'otaz': tazrange, 'dtaz': tazrange}
                )
                appended_names.append(fi.replace(".emx", ""))
    if len(master) == 0:
        log.error(f'no uncompressed skims read from {directory}')
        raise FileNotFoundError(directory)
    log.info(f"{len(appended_names)} uncompressed skims were added to the skims dataset")
    return sh.Dataset(master), appended_names


def read_compressed_skims(zarr_directory):
    """
    Read previously compressed skims.

    Compressed skims must be stored in a directory ending with a .zarr extension.

    Parameters
    ----------
    zarr_directory

    Returns
    -------
    Dataset
    """
    result = None
    if zarr_directory is None:
        log.warning("no `zarr_directory` given")
        raise ValueError("no `zarr_directory` given")
    zarr_directory = Path(zarr_directory)
    if os.path.isdir(zarr_directory.with_suffix(".zarr")):
        log.info(f'reading compressed skims from {zarr_directory.with_suffix(".zarr")}')
        result = sh.Dataset.from_zarr(zarr_directory.with_suffix(".zarr"))
    elif os.path.isfile(zarr_directory.with_suffix(".zarr.zip")):
        log.info(f'reading compressed skims from {zarr_directory.with_suffix(".zarr.zip")}')
        result = sh.Dataset.from_zarr(zarr_directory.with_suffix(".zarr.zip"))
    if result is not None:
        log.info(f"{len(result.data_vars)} compressed skims were added to the skims dataset")
        return result
    raise FileNotFoundError(zarr_directory)


def write_uncompressed_skims(skims, directory, overwrite=False):
    """
    Writes the .emx files for EMME in the emmemat directory.

    Parameters
    ----------
    skims : Dataset
    directory : Path-like
        uncompressed skims will go here
    overwrite : bool, default False
        Whether to overwrite existing uncompressed skims.
    """
    os.makedirs(directory, exist_ok=True)
    for k in skims:
        filename = os.path.join(directory, f"{k}.emx")
        if not os.path.exists(filename) or overwrite:
            skims[k].values.tofile(filename)


def write_compressed_skims(skims, output="emmemat.zarr"):
    """
    Compress the `emmemat` skims directory using zarr.

    Parameters
    ----------
    directory : path to emmemat directory
    output : str, ends with {"zarr.zip", "zarr"}
        File format to use for compression.  Zarr.zip puts all the
        piece in one big zip directory, while plain Zarr uses the
        native file system to store a bunch of pieces.  Both methods
        use the same compression.
    """
    known_exts = (".zarr", ".zarr.zip")
    if not any(output.endswith(k) for k in known_exts):
        raise NotImplementedError(output)
    if output.endswith(".zarr"):
        skims.to_zarr(output, mode='a')
    elif output.endswith(".zarr.zip"):
        if os.path.exists(output):
            raise FileExistsError(output)
        with zarr.ZipStore(output, mode='w') as store:
            skims.to_zarr(store)


