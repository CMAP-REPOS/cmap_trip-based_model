import numbers
import numpy as np

def check_random_state(seed):
    """
    Turn a seed into a numpy RandomState instance

    Parameters
    ----------
    seed : None | int | instance of RandomState
        If seed is None, return the RandomState singleton used by np.random.
        If seed is an int, return a new RandomState instance seeded with seed.
        If seed is already a RandomState instance, return it.
        Otherwise raise ValueError.
    """
    if seed is None or seed is np.random:
        return np.random.RandomState()
    if isinstance(seed, numbers.Integral):
        return np.random.RandomState(seed % 1<<32)
    if isinstance(seed, np.random.RandomState):
        return seed
    raise ValueError('%r cannot be used to seed a numpy.random.RandomState'
                     ' instance' % seed)


def check_random_generator(seed=None):
    """
    Create a random generator.

    Parameters
    ----------
    seed

    Returns
    -------
    np.random.Generator
    """
    if seed is None:
        raise ValueError("True random seeding disallowed for CMAP trip based model")
    try:
        return np.random.default_rng(seed)
    except TypeError:
        if isinstance(seed, np.random.RandomState):
            return np.random.default_rng(seed.get_state()[1])
        else:
            raise