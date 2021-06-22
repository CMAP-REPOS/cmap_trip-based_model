
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def _1_auto_avail_NHB_XHHWLREMVWOEBM6NUQF7EFH6(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 1 - _outputs[37]
