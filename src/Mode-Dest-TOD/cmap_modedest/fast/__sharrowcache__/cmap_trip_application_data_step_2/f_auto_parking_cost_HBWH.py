
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def auto_parking_cost_HBWH(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 0.0
