
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def fmin_ozone_areatype__areatype___1_OTNZJFEFZT7N3JG6YKEUWGTF(
    _args, 
    _inputs, 
    _outputs,
    __dzone__zone_type, __ozone__zone_type
):
    return min(__ozone__zone_type[_args[0],], __dzone__zone_type[_args[1],]) == 1
