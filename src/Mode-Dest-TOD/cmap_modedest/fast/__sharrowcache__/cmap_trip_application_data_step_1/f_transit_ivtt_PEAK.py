
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_ivtt_PEAK(
    _args, 
    _inputs, 
    _outputs,
    __transit_pk__ivtt
):
    return __transit_pk__ivtt[_args[0], _args[1]]
