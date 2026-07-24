
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def _1_transit_avail_NHB_WN7OPNQOSVQ544PG5Q2ZGTNF(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 1 - _outputs[24]
