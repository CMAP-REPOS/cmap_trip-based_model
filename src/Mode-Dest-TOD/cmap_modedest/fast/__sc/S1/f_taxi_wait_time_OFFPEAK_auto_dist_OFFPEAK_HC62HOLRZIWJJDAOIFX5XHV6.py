
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def taxi_wait_time_OFFPEAK_auto_dist_OFFPEAK_HC62HOLRZIWJJDAOIFX5XHV6(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__md_dist, __ozone__taxi_wait_op
):
    return __ozone__taxi_wait_op[_args[0],] / __auto_skims__md_dist[_args[0], _args[1]]
