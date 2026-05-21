import numpy as np
import xarray as xr
import pandas as pd


def one_based(n):
    return pd.RangeIndex(1, n + 1)


def zero_based(n):
    return pd.RangeIndex(0, n)


class Dataset(xr.Dataset):

    __slots__ = ()

    def update(self, other):
        super().update(other)
        if isinstance(other, Dataset):
            match_names = self.match_names
            match_names.update(other.match_names)
            self.match_names = match_names
        return self  # deprecated return for consistency until xarray 0.19

    @classmethod
    def from_table(
        cls, tbl, index_name="index", index=None,
    ):
        """
        Convert a pyarrow.Table into an xarray.Dataset

        Parameters
        ----------
        tbl : Table
            Table from which to use data and indices.
        index_name : str, default 'index'
            This name will be given to the default dimension index, if
            none is given.  Ignored if `index` is given explicitly.
        index : Index-like, optional
            Use this index instead of a default RangeIndex.

        Returns
        -------
        New Dataset.
        """
        if len(set(tbl.column_names)) != len(tbl.column_names):
            raise ValueError("cannot convert Table with non-unique columns")
        if index is None:
            index = pd.RangeIndex(len(tbl), name=index_name)
        else:
            if len(index) != len(tbl):
                raise ValueError(
                    f"length of index ({len(index)}) does not match length of table ({len(tbl)})"
                )
        if isinstance(index, pd.MultiIndex) and not index.is_unique:
            raise ValueError(
                "cannot attach a non-unique MultiIndex and convert into xarray"
            )
        arrays = [
            (tbl.column_names[n], np.asarray(tbl.column(n)))
            for n in range(len(tbl.column_names))
        ]
        result = cls()
        if isinstance(index, pd.MultiIndex):
            dims = tuple(
                name if name is not None else "level_%i" % n
                for n, name in enumerate(index.names)
            )
            for dim, lev in zip(dims, index.levels):
                result[dim] = (dim, lev)
        else:
            index_name = index.name if index.name is not None else "index"
            dims = (index_name,)
            result[index_name] = (dims, index)

        result._set_numpy_data_from_dataframe(index, arrays, dims)
        return result

    @classmethod
    def from_omx(
        cls, omx, index_names=("otaz", "dtaz"), indexes="one-based", renames=None,
    ):
        # handle both larch.OMX and openmatrix.open_file versions
        if "larch" in type(omx).__module__:
            omx_data = omx.data
            omx_shape = omx.shape
        else:
            omx_data = omx.root["data"]
            omx_shape = omx.shape()

        arrays = {}
        if renames is None:
            for k in omx_data._v_children:
                arrays[k] = omx_data[k][:]
        elif isinstance(renames, dict):
            for new_k, old_k in renames.items():
                arrays[new_k] = omx_data[old_k][:]
        else:
            for k in renames:
                arrays[k] = omx_data[k][:]
        d = {
            "dims": index_names,
            "data_vars": {k: {"dims": index_names, "data": arrays[k]} for k in arrays},
        }
        if indexes == "one-based":
            indexes = {
                index_names[0]: one_based(omx_shape[0]),
                index_names[1]: one_based(omx_shape[1]),
            }
        elif indexes == "zero-based":
            indexes = {
                index_names[0]: zero_based(omx_shape[0]),
                index_names[1]: zero_based(omx_shape[1]),
            }
        if indexes is not None:
            d["coords"] = {
                index_name: {"dims": index_name, "data": index}
                for index_name, index in indexes.items()
            }
        return cls.from_dict(d)

    @classmethod
    def from_amx(
        cls, amx, index_names=("otaz", "dtaz"), indexes="one-based", renames=None,
    ):
        arrays = {}
        if renames is None:
            for k in amx.list_matrices():
                arrays[k] = amx[k][:]
        elif isinstance(renames, dict):
            for new_k, old_k in renames.items():
                arrays[new_k] = amx[old_k]
        else:
            for k in renames:
                arrays[k] = amx[k]
        d = {
            "dims": index_names,
            "data_vars": {k: {"dims": index_names, "data": arrays[k]} for k in arrays},
        }
        if indexes == "one-based":
            indexes = {index_names[i]: "1" for i in range(len(index_names))}
        elif indexes == "zero-based":
            indexes = {index_names[i]: "0" for i in range(len(index_names))}
        if isinstance(indexes, (list, tuple)):
            indexes = dict(zip(index_names, indexes))
        if isinstance(indexes, dict):
            for n, i in enumerate(index_names):
                if indexes.get(i) == "1":
                    indexes[i] = one_based(amx.shape[n])
                elif indexes.get(i) == "0":
                    indexes[i] = zero_based(amx.shape[n])
        if indexes is not None:
            d["coords"] = {
                index_name: {"dims": index_name, "data": index}
                for index_name, index in indexes.items()
            }
        return cls.from_dict(d)

    @classmethod
    def from_zarr(cls, store, *args, **kwargs):
        return cls(xr.open_zarr(store, *args, **kwargs))

    def to_zarr(self, *args, **kwargs):
        """
        Write dataset contents to a zarr group.

        Parameters
        ----------
        store : MutableMapping, str or Path, optional
            Store or path to directory in file system.  If given with a
            ".zarr.zip" extension, and keyword arguments limited to 'mode' and
            'compression', then a ZipStore will be created, populated, and then
            immediately closed.
        chunk_store : MutableMapping, str or Path, optional
            Store or path to directory in file system only for Zarr array chunks.
            Requires zarr-python v2.4.0 or later.
        mode : {"w", "w-", "a", None}, optional
            Persistence mode: "w" means create (overwrite if exists);
            "w-" means create (fail if exists);
            "a" means override existing variables (create if does not exist).
            If ``append_dim`` is set, ``mode`` can be omitted as it is
            internally set to ``"a"``. Otherwise, ``mode`` will default to
            `w-` if not set.
        synchronizer : object, optional
            Zarr array synchronizer.
        group : str, optional
            Group path. (a.k.a. `path` in zarr terminology.)
        encoding : dict, optional
            Nested dictionary with variable names as keys and dictionaries of
            variable specific encodings as values, e.g.,
            ``{"my_variable": {"dtype": "int16", "scale_factor": 0.1,}, ...}``
        compute : bool, optional
            If True write array data immediately, otherwise return a
            ``dask.delayed.Delayed`` object that can be computed to write
            array data later. Metadata is always updated eagerly.
        consolidated : bool, optional
            If True, apply zarr's `consolidate_metadata` function to the store
            after writing metadata.
        append_dim : hashable, optional
            If set, the dimension along which the data will be appended. All
            other dimensions on overriden variables must remain the same size.
        region : dict, optional
            Optional mapping from dimension names to integer slices along
            dataset dimensions to indicate the region of existing zarr array(s)
            in which to write this dataset's data. For example,
            ``{'x': slice(0, 1000), 'y': slice(10000, 11000)}`` would indicate
            that values should be written to the region ``0:1000`` along ``x``
            and ``10000:11000`` along ``y``.

            Two restrictions apply to the use of ``region``:

            - If ``region`` is set, _all_ variables in a dataset must have at
              least one dimension in common with the region. Other variables
              should be written in a separate call to ``to_zarr()``.
            - Dimensions cannot be included in both ``region`` and
              ``append_dim`` at the same time. To create empty arrays to fill
              in with ``region``, use a separate call to ``to_zarr()`` with
              ``compute=False``. See "Appending to existing Zarr stores" in
              the reference documentation for full details.
        compression : int, optional
            Only used for ".zarr.zip" files.  By default zarr uses blosc
            compression for chunks, so adding another layer of compression here
            is typically redundant.

        References
        ----------
        https://zarr.readthedocs.io/

        Notes
        -----
        Zarr chunking behavior:
            If chunks are found in the encoding argument or attribute
            corresponding to any DataArray, those chunks are used.
            If a DataArray is a dask array, it is written with those chunks.
            If not other chunks are found, Zarr uses its own heuristics to
            choose automatic chunk sizes.
        """
        if (
            len(args) == 1
            and isinstance(args[0], str)
            and args[0].endswith(".zarr.zip")
        ):
            if {"compression", "mode"}.issuperset(kwargs.keys()):
                import zarr

                with zarr.ZipStore(args[0], **kwargs) as store:
                    self.to_zarr(store)
                return
        return super().to_zarr(*args, **kwargs)

    def iat(self, *, _names=None, _load=False, _index_name=None, **idxs):
        loaders = {}
        if _index_name is None:
            _index_name = "index"
        for k, v in idxs.items():
            loaders[k] = xr.DataArray(v, dims=[_index_name])
        if _names:
            ds = self[_names]
        else:
            ds = self
        if _load:
            ds = ds.load()
        return ds.isel(**loaders)

    def at(self, *, _names=None, _load=False, _index_name=None, **idxs):
        loaders = {}
        if _index_name is None:
            _index_name = "index"
        for k, v in idxs.items():
            loaders[k] = xr.DataArray(v, dims=[_index_name])
        if _names:
            ds = self[_names]
        else:
            ds = self
        if _load:
            ds = ds.load()
        return ds.sel(**loaders)

    def at_df(self, df):
        """
        Extract values by label on the coordinates indicated by columns of a DataFrame.

        Parameters
        ----------
        df : pd.DataFrame or Mapping[str, array-like]
            The columns (or keys) of `df` should match the named dimensions of
            this Dataset.  The resulting extracted DataFrame will have one row
            per row of `df`, columns matching the data variables in this dataset,
            and each value is looked up by the labels.

        Returns
        -------
        pd.DataFrame
        """
        result = self.at(**df).reset_coords(drop=True).to_dataframe()
        if isinstance(df, pd.DataFrame):
            result.index = df.index
        return result

    def iat_df(self, df):
        """
        Extract values by position on the coordinates indicated by columns of a DataFrame.

        Parameters
        ----------
        df : pd.DataFrame or Mapping[str, array-like]
            The columns (or keys) of `df` should match the named dimensions of
            this Dataset.  The resulting extracted DataFrame will have one row
            per row of `df`, columns matching the data variables in this dataset,
            and each value is looked up by the positions.

        Returns
        -------
        pd.DataFrame
        """
        result = self.iat(**df).reset_coords(drop=True).to_dataframe()
        if isinstance(df, pd.DataFrame):
            result.index = df.index
        return result

    def select_and_rename(self, name_dict=None, **names):
        """
        Select and rename variables from this Dataset

        Parameters
        ----------
        name_dict, **names: dict
            The keys or keyword arguments give the current names of the
            variables that will be selected out of this Dataset.  The values
            give the new names of the same variables in the resulting Dataset.

        Returns
        -------
        Dataset
        """
        if name_dict is None:
            name_dict = names
        else:
            name_dict.update(names)
        return self[list(name_dict.keys())].rename(name_dict)

    def squash_index(self, indexes_dict=None, *, set_match_names=True, **indexes):
        if indexes_dict is None:
            indexes_dict = indexes
        else:
            indexes_dict.update(indexes)
        ds = self.reset_index(list(indexes_dict.keys()), drop=True)
        ds = ds.rename(**indexes_dict)
        if set_match_names:
            ds = ds.set_match_names({v: v for v in indexes_dict.values()})
        return ds

    def _repr_html_(self):
        html = super()._repr_html_()
        html = html.replace("xarray.Dataset", "sharrow.Dataset")
        return html

    def __repr__(self):
        r = super().__repr__()
        r = r.replace("xarray.Dataset", "sharrow.Dataset")
        return r

    @property
    def match_names(self):
        """
        A mapping

        If a match_name target begins with an '@', the match is a dynamic
        match, where the particular index-position values are created based
        on data in the main or other source[s]. This allows for match columns
        that do not exist yet, including columns where the key column exists
        but is a label-based or offset-based match that needs to be processed
        into index-position values.

        """
        result = {}
        for k in self.attrs.keys():
            if k.startswith("match_names_"):
                result[k[12:]] = self.attrs.get(k)
        for k in self.indexes.keys():
            if k not in result:
                result[k] = None
        return result

    @match_names.setter
    def match_names(self, names):
        if names is None:
            existing_match_name_keys = list(self.match_names.keys())
            for k in existing_match_name_keys:
                del self.attrs[k]
            return
        if isinstance(names, str):
            dims = list(self.dims.keys())
            assert len(dims) == 1
            names = {dims[0]: names}
        for k in names.keys():
            if k not in self.dims:
                raise ValueError(f"'{k}' not in dims")
        for k, v in names.items():
            if v is not None:
                self.attrs[f"match_names_{k}"] = v
            elif f"match_names_{k}" in self.attrs:
                del self.attrs[f"match_names_{k}"]

    def set_match_names(self, names):
        """
        Create a copy of this dataset with the given match_names for flowing.

        Parameters
        ----------
        names : Sequence[str] or Mapping[str,str]

        Returns
        -------
        sharrow.shared.Dataset
        """
        result = self.copy()
        result.match_names = names
        return result

    def keep_dims(self, keep_dims, *, errors="raise"):
        """
        Keep only certain dimensions and associated variables from this dataset.

        Parameters
        ----------
        keep_dims : hashable or iterable of hashable
            Dimension or dimensions to keep.
        errors : {"raise", "ignore"}, optional
            If 'raise' (default), raises a ValueError error if any of the
            dimensions passed are not in the dataset. If 'ignore', any given
            labels that are in the dataset are dropped and no error is raised.

        Returns
        -------
        obj : Dataset
            The dataset without the given dimensions (or any variables
            containing those dimensions)
        errors : {"raise", "ignore"}, optional
            If 'raise' (default), raises a ValueError error if
            any of the dimensions passed are not
            in the dataset. If 'ignore', any given dimensions that are in the
            dataset are dropped and no error is raised.
        """
        if isinstance(keep_dims, str):
            keep_dims = {keep_dims}
        else:
            keep_dims = set(keep_dims)
        all_dims = set(self.dims)
        if errors == "raise":
            missing_dims = keep_dims - all_dims
            if missing_dims:
                raise ValueError(
                    "Dataset does not contain the dimensions: %s" % missing_dims
                )
        return self.drop_dims([i for i in all_dims if i not in keep_dims])
