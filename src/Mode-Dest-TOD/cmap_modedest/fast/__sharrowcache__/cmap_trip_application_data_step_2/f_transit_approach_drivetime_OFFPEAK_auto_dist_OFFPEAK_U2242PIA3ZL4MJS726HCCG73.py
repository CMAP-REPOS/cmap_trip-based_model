
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_approach_drivetime_OFFPEAK_auto_dist_OFFPEAK_U2242PIA3ZL4MJS726HCCG73(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[7] / _inputs[0]
