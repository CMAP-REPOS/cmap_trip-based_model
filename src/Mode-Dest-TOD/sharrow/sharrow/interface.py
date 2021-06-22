import os
import sys
import importlib
import hashlib
import base64
import pandas as pd
import pyarrow as pa
import xarray as xr

from .dataset import Dataset


class SharedData:
    def __init__(
        self, main=None, extra_funcs=(), extra_vars=None, **datasets,
    ):
        """
        SharedData establishes a domain to evaluate data flows.

        Parameters
        ----------
        main : Dataset or Table or DataFrame or Sequence[str], optional
            This is the main set of data that defines this object.
            Only the main data can be replaced with similarly formatted
            other data when flowing results.
        **datasets : Dict[str, Dataset]
            Other `Dataset` inputs for flowing data can be provided.
            The flow closes on any other data (i.e. it is immutable and cannot
            be replaced when flowing).  Although "main" can be certain non-Dataset
            types (i.e. a DataFrame or a Table), datasets must be
            given as `Dataset` objects with appropriate `match_names` set.
        """
        self.main = main
        self.datasets = datasets
        try:
            from sharrow_pro import Table
        except ImportError:
            table_types = (pa.Table,)
        else:
            table_types = (pa.Table, Table)
        for k in self.datasets:
            arr = self.datasets[k]
            if not isinstance(arr, Dataset):
                if isinstance(arr, pd.DataFrame):
                    arr = Dataset.from_dataframe(arr)
                elif isinstance(arr, table_types):
                    arr = Dataset.from_table(arr)
                elif isinstance(arr, xr.Dataset):
                    arr = Dataset(arr)
                self.datasets[k] = arr

    def setup_flow(
        self, definition_spec, cache_dir, name, dtype="float32",
    ):
        """

        Parameters
        ----------
        definition_spec : Dict[str,str]
            Gives the names and definitions for the columns to
            create in our generated table.

        Returns
        -------
        module
        """
        if not os.path.isdir(cache_dir):
            raise NotADirectoryError(cache_dir)
        if not os.path.isdir(os.path.join(cache_dir, name)):
            raise ModuleNotFoundError(name)
        if not os.path.isfile(os.path.join(cache_dir, name, "__init__.py")):
            raise ModuleNotFoundError(name)

        sys.path.insert(0, str(cache_dir))
        importlib.invalidate_caches()
        module = importlib.import_module(name)
        sys.path = sys.path[1:]
        module.set_shared_data(**self.datasets)

        if definition_spec and hasattr(module, 'defs_hash'):
            defs_hash = hashlib.md5()
            for k, v in definition_spec.items():
                defs_hash.update(k.encode("utf8"))
                defs_hash.update(v.encode("utf8"))
            if module.defs_hash != (base64.b32encode(defs_hash.digest())).decode():
                raise ValueError(
                    "definitions are not consistent with precompiled data flows"
                )

        return module
