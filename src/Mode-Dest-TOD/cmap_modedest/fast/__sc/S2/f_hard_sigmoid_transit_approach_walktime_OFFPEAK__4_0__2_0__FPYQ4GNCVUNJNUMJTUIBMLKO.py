
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def hard_sigmoid_transit_approach_walktime_OFFPEAK__4_0__2_0__FPYQ4GNCVUNJNUMJTUIBMLKO(
    _args, 
    _inputs, 
    _outputs,
    
):
    return hard_sigmoid(_inputs[11], 4.0, 2.0)
