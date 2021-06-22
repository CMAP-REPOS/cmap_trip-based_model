
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.njit(cache=True, error_model='numpy', boundscheck=False)
def piece_transit_ivtt_OFFPEAK__20__None__2SYXPZCTOY3PKAO2L4ALLHLI(
    _args, 
    _inputs, 
    _outputs,
    
):
    return piece(_outputs[2], 20, None)
