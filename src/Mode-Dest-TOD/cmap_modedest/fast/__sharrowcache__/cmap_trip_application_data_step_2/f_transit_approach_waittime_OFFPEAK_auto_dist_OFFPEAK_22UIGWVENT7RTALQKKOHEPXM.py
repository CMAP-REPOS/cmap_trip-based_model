
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_approach_waittime_OFFPEAK_auto_dist_OFFPEAK_22UIGWVENT7RTALQKKOHEPXM(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[9] / _inputs[0]
