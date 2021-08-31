
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def auto_avail_HBWH(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[4] > -9998
