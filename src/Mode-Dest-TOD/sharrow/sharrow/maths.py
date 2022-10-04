from numba import njit


@njit(cache=True)
def piece(x, low_bound, high_bound=None):
    """
    Clip the values in an array.

    This function differs from the usual `numpy.clip`
    in that the result is shifted by `low_bound` if it is
    given, so that the valid result range is a contiguous
    block of non-negative values starting from 0.

    Parameters
    ----------
    x : array-like
        Array containing elements to clip
    low_bound, high_bound : scalar, array-like, or None
        Minimum and maximum values. If None, clipping is not
        performed on the corresponding edge. Both may be
        None, which results in a noop. Both are broadcast
        against `x`.

    Returns
    -------
    clipped_array : array-like
    """
    if low_bound is None:
        if high_bound is None:
            return x
        else:
            if x < high_bound:
                return x
            else:
                return high_bound
    else:
        if high_bound is None:
            if x > low_bound:
                return x - low_bound
            else:
                return 0.0
        else:
            if x > low_bound:
                if x < high_bound:
                    return x - low_bound
                else:
                    return high_bound - low_bound
            else:
                return 0.0


@njit(cache=True)
def hard_sigmoid(x, zero_bound, one_bound):
    """
    Apply a piecewise linear sigmoid function.

    Parameters
    ----------
    x : array-like
        Array containing elements to clip
    zero_bound, one_bound : scalar, array-like, or None
        Inflection points of the piecewise linear sigmoid
        function.

    Returns
    -------
    clipped_array : array-like
    """
    if zero_bound < one_bound:
        if x <= zero_bound:
            return 0.0
        if x >= one_bound:
            return 1.0
        return (x - zero_bound) / (one_bound - zero_bound)
    else:
        if x >= zero_bound:
            return 0.0
        if x <= one_bound:
            return 1.0
        return (zero_bound - x) / (zero_bound - one_bound)
