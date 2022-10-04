
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def piece_auto_dist_OFFPEAK_None_5__YWXA4HRZAVEZKC7EV5M2AX3B(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__md_dist
):
    return piece(__auto_skims__md_dist[_args[0], _args[1]], None, 5)
