
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def tnc_solo_wait_time_PEAK(
    _args, 
    _inputs, 
    _outputs,
    __ozone__tnc_solo_wait_pk
):
    return __ozone__tnc_solo_wait_pk[_args[0],]
