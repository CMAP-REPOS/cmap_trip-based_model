
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_avail_HBWH(
    _args, 
    _inputs, 
    _outputs,
    
):
    return (_inputs[14] < 999) & (_inputs[12] < 999) & (_inputs[8] < 999) & (_inputs[4] > -9998)
