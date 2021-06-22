
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def tnc_pool_wait_time_PEAK_auto_dist_PEAK_NHA453KUTMUKTUCSMA2XQQZV(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__am_dist, __ozone__tnc_pool_wait_pk
):
    return __ozone__tnc_pool_wait_pk[_args[0],] / __auto_skims__am_dist[_args[0], _args[1]]
