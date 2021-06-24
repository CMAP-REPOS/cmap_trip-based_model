
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def auto_time_OFFPEAK(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__md_time
):
    return __auto_skims__md_time[_args[0], _args[1]]
