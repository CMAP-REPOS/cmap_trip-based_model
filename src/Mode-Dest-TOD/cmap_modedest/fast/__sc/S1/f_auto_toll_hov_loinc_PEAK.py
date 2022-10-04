
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def auto_toll_hov_loinc_PEAK(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__am_toll_hov_loinc
):
    return np.fmax(__auto_skims__am_toll_hov_loinc[_args[0], _args[1]], 0)
