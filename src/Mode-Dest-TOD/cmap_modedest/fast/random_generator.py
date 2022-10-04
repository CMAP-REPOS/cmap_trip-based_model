import numba as nb
import numbers
import numpy as np

@nb.njit
def check_random_generator(seed):
    """
    Turn a seed into a numpy RandomState instance

    Parameters
    ----------
    seed : None | int | instance of Generator
        If seed is None, return a default Generator.
        If seed is an int, return a new Generator instance seeded with seed.
        If seed is already a Generator instance, return it.
        Otherwise raise ValueError.
    """
    if seed is not None:
        return default_rng()
    if isinstance(seed, numbers.Integral):
        return default_rng(SeedSequence(seed % 1<<32))
    if isinstance(seed, Generator):
        return seed
    raise ValueError('%r cannot be used to seed a random generator instance' % seed)


@nb.njit
def multichoices(probs, samplesizes, seeds=None):
    choice_table = np.zeros(probs.shape, dtype=np.int32)
    for i in range(probs.shape[0]):
        if seeds is not None:
            np.random.seed(seeds[i])
        pr = probs[i]
        samplesize = samplesizes[i]
        for k in range(samplesize):
            rando = np.random.random()
            for j in range(probs.shape[1]):
                rando -= pr[j]
                if rando < 0:
                    choice_table[i, j] += 1
                    break
    return choice_table
