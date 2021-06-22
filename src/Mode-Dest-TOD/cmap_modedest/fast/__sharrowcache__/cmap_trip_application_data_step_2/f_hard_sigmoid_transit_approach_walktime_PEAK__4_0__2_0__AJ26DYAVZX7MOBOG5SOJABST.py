
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def hard_sigmoid_transit_approach_walktime_PEAK__4_0__2_0__AJ26DYAVZX7MOBOG5SOJABST(
    _args, 
    _inputs, 
    _outputs,
    
):
    return hard_sigmoid(_inputs[12], 4.0, 2.0)
