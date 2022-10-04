
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def transit_approach_walktime_OFFPEAK_auto_dist_OFFPEAK_ITAE2YOSBIY76DBTPIPT6QD2(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[11] / _inputs[0]
