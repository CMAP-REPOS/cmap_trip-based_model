
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def auto_parking_cost_HBWL(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 0.0
