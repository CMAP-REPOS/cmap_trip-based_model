
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def transit_ovtt_OFFPEAK_auto_dist_OFFPEAK_IY4UAVBMS2HPHNYZJLSMWG3V(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[15] / _inputs[0]
