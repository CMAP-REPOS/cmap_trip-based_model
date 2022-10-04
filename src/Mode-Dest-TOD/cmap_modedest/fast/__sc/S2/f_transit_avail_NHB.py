
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def transit_avail_NHB(
    _args, 
    _inputs, 
    _outputs,
    
):
    return (_inputs[13] < 999) & (_inputs[11] < 999) & (_inputs[7] < 999) & (_inputs[6] > -9998)
