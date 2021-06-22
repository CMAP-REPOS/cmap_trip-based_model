
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_approach_walktime_PEAK_auto_dist_PEAK_T52CC4SUBMFSLGDNMCSO7A7J(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[12] / _inputs[1]
