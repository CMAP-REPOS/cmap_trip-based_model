
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_ovtt_PEAK_auto_dist_PEAK_UIAXX4VIE5PP53PJMCSMEFOP(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[16] / _inputs[1]