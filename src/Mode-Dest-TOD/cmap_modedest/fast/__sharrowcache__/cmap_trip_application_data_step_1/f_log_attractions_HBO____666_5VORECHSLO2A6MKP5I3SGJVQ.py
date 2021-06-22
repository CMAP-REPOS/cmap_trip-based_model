
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def log_attractions_HBO____666_5VORECHSLO2A6MKP5I3SGJVQ(
    _args, 
    _inputs, 
    _outputs,
    __attractions__HBO
):
    return log(__attractions__HBO[_args[1],]) > -666
