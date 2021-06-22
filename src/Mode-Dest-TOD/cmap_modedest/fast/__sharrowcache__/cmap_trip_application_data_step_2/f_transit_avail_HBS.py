
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_avail_HBS(
    _args, 
    _inputs, 
    _outputs,
    
):
    return (_inputs[13] < 999) & (_inputs[11] < 999) & (_inputs[7] < 999) & (_inputs[3] > -9998)