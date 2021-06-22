
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def auto_toll_OFFPEAK(
    _args, 
    _inputs, 
    _outputs,
    __auto_skims__md_toll
):
    return np.fmax(__auto_skims__md_toll[_args[0], _args[1]], 0)
