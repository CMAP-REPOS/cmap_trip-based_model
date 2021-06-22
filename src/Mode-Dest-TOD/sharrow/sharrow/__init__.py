__version__ = "2021.4"

import os
from .interface import SharedData
from .dataset import Dataset
from .local_dir import local_cache
from .tables import concat_tables, Table
from xarray import DataArray

try:
    _pro = int(os.environ.get("SHARROW_PRO", "1"))
except:
    _pro = True

if _pro:
    try:
        from sharrow_pro import *
    except ImportError:
        pass
