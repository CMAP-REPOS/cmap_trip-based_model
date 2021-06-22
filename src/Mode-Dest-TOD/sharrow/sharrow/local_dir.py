import os
import inspect


def local_cache(dirname="__sharrowcache__"):
    return os.path.join(os.path.dirname(inspect.stack()[1][1]), dirname,)
