
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_approach_waittime_PEAK_auto_dist_PEAK_YHJNF2B3VEICSZL64Y2XLV4S(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[10] / _inputs[1]
