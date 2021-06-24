
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def transit_approach_drivetime_PEAK_auto_dist_PEAK_SVETJXGBSVYEYM7OVBAQW2PX(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[8] / _inputs[1]
