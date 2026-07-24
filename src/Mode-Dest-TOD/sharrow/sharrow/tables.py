import numpy as np
import pandas as pd
import pyarrow as pa


class Table:
    def __new__(cls, init, *args, **kwargs):
        if init is None:
            return None
        if isinstance(init, cls):
            return init
        return super().__new__(cls)

    def __init__(self, init, nthreads=None):
        if isinstance(init, Table):
            self._table = init._table
        elif isinstance(init, pd.DataFrame):
            self._table = pa.Table.from_pandas(init, nthreads=nthreads)
        elif isinstance(init, dict):
            self._table = pa.Table.from_pydict(init)
        elif isinstance(init, pa.Table):
            self._table = init
        elif isinstance(init, int):
            self._table = pa.table([])
            self._tentative_len = init
        else:
            raise TypeError(f"init is invalid type {type(init)}")

    def __repr__(self):
        r = repr(self._table)
        return r.replace("pyarrow.Table", "sharrow.Table")

    def __dir__(self):
        return dir(self._table) + [
            "concat",
        ]

    def append_column(self, name, value, overwrite=True):
        """
        Append a column to the Table.

        Unlike a regular pyarrow Table, by default this will remove any
        existing column with the same name.

        Parameters
        ----------
        name : str
        value : array-like
        overwrite : bool, default True

        Returns
        -------
        self
        """
        if len(self._table):
            if overwrite:
                try:
                    position = self._table.column_names.index(name)
                except ValueError:
                    pass
                else:
                    self._table = self._table.remove_column(position)
            self._table = self._table.append_column(name, value)
        else:
            self._table = pa.table({name: value})
        return self

    def __getattr__(self, item):
        return getattr(self._table, item)

    def __getitem__(self, item):
        return self._table[item]

    def __setitem__(self, item, value):
        if not isinstance(value, np.ndarray):
            value = np.asarray(value)
        result = np.broadcast_to(value, (self._table.num_rows))
        self._table = self._table.append_column(item, [result])

    def __len__(self):
        return len(self._table) or getattr(self, "_tentative_len", 0)

    def concat(self, others):
        tables = [self._table]
        for other in others:
            if isinstance(other, Table):
                other = other._table
            tables.append(other)
        self._table = pa.concat_tables(tables)

    @classmethod
    def from_pydict(cls, mapping):
        return cls(mapping)


def concat_tables(tables):
    _tables = []
    for t in tables:
        if isinstance(t, Table):
            t = t._table
        _tables.append(t)
    return Table(pa.concat_tables(_tables))
