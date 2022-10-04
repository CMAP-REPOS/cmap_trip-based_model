
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def piece_transit_ivtt_OFFPEAK__None__20__54N5BOFSYCN6SMRBKXM4V4TW(
    _args, 
    _inputs, 
    _outputs,
    
):
    return piece(_outputs[2], None, 20)
