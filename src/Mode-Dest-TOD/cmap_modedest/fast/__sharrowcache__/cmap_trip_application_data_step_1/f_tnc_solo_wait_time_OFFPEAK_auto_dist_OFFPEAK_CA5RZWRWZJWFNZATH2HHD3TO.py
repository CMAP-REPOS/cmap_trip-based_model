
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def tnc_solo_wait_time_OFFPEAK_auto_dist_OFFPEAK_CA5RZWRWZJWFNZATH2HHD3TO(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__md_dist, __ozone__tnc_solo_wait_op
):
    return __ozone__tnc_solo_wait_op[_args[0],] / __auto_skims__md_dist[_args[0], _args[1]]
