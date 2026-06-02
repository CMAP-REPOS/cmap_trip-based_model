
# Numexpr's caching sometimes fails on dask multi-threaded


class CacheDictSafe(dict):
    """
    A dictionary that prevents itself from growing too much.
    """

    def __init__(self, maxentries):
        self.maxentries = maxentries
        super(CacheDictSafe, self).__init__(self)

    def __setitem__(self, key, value):
        # Protection against growing the cache too much
        if len(self) > self.maxentries * 2:
            # The cache is overflowing, wipe it completely
            super(CacheDictSafe, self).clear()
        if len(self) > self.maxentries:
            # Remove a 10% of (arbitrary) elements from the cache
            entries_to_remove = self.maxentries // 10
            for k in list(self.keys())[:entries_to_remove]:
                try:
                    super(CacheDictSafe, self).__delitem__(k)
                except KeyError:
                    pass
        super(CacheDictSafe, self).__setitem__(key, value)


import numexpr as ne
ne.necompiler._numexpr_cache = CacheDictSafe(256)
