
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def log_attractions_HBWL____666_IQGLCTAX7EZOXKEFCIBK33BD(
    _args, 
    _inputs, 
    _outputs,
    __attractions__HBWL
):
    return log(__attractions__HBWL[_args[1],]) > -666
