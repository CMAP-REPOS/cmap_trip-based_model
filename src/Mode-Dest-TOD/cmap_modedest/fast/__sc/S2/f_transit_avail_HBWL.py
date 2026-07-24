
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def transit_avail_HBWL(
    _args, 
    _inputs, 
    _outputs,
    
):
    return (_inputs[14] < 999) & (_inputs[12] < 999) & (_inputs[8] < 999) & (_inputs[5] > -9998)
