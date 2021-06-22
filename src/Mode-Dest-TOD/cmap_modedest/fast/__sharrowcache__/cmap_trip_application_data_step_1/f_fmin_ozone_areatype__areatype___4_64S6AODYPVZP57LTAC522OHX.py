
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def fmin_ozone_areatype__areatype___4_64S6AODYPVZP57LTAC522OHX(
    _args, 
    _inputs, 
    _outputs,
    __dzone__zone_type, __ozone__zone_type
):
    return min(__ozone__zone_type[_args[0],], __dzone__zone_type[_args[1],]) == 4
