
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def _1_auto_avail_HBS_JA6SY3LPJJDVZQF7IFNL4EWX(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 1 - _outputs[31]
