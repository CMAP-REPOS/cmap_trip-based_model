
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid



@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def log_1_samp_wgt__PLSMOODNF77UOPHIVH6I2WBL(
    _args, 
    _inputs, 
    _outputs,
    
):
    return 0.0
