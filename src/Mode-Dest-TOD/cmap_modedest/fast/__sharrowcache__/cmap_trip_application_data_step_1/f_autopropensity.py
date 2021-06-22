
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def autopropensity(
    _args, 
    _inputs, 
    _outputs,
    __d_autopropensity__autopropensity
):
    return __d_autopropensity__autopropensity[_args[1],]
