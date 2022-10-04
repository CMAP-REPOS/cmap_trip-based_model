
import numba
import numpy as np
from numpy import log, exp, log1p, expm1
from sharrow.maths import piece, hard_sigmoid
from .extra_funcs import *
from .extra_vars import *

@numba.jit(cache=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def o_zone____dtaz_HXCH5HUPIDPV3EWONWFXBZHM(
    _args, 
    _inputs, 
    _outputs,
    
):
    return _inputs[1] == _inputs[0]
