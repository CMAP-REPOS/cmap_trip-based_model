
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def transit_fare_OFFPEAK(
    _args, 
    _inputs, 
    _outputs,
    __transit_op__fare
):
    return __transit_op__fare[_args[0], _args[1]]
