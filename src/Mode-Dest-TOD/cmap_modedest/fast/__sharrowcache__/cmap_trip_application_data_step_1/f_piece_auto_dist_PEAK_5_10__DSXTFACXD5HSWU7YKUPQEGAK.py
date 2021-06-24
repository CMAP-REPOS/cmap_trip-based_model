
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def piece_auto_dist_PEAK_5_10__DSXTFACXD5HSWU7YKUPQEGAK(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__am_dist
):
    return piece(__auto_skims__am_dist[_args[0], _args[1]], 5, 10)
