
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def piece_auto_dist_PEAK_10_None__L3GKJNRUTTFCDUYV65K7KFQD(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__am_dist
):
    return piece(__auto_skims__am_dist[_args[0], _args[1]], 10, None)
