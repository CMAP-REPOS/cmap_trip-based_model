
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def taxi_wait_time_PEAK_auto_dist_PEAK_CBYSKDURSGAJ4RHX2ASS76Q2(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__am_dist, __ozone__taxi_wait_pk
):
    return __ozone__taxi_wait_pk[_args[0],] / __auto_skims__am_dist[_args[0], _args[1]]
