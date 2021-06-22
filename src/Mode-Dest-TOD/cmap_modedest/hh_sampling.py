import numpy as np
import pandas as pd
from cmap_modedest.random_states import check_random_state


def sample_hh_from_zone(dh, zone, n_hh, random_state=None, attr=('N_ADULTS', 'N_VEHICLES')):
    pool = dh.hh_tabulation.loc[zone]
    hhv_types = dh.hhv_types
    pool_sum = pool.sum()
    if pool_sum:
        hhv_prob = pool / pool_sum
    else:
        # No households in this zone to enumerate, sample from region instead to avoid NaNs
        pool = dh.hh_tabulation.sum()
        pool_sum = pool.sum()
        hhv_prob = pool / pool_sum
    prng = check_random_state(random_state)
    draws = prng.choice(624, size=n_hh, p=hhv_prob)
    result = pd.DataFrame(-1, dtype=np.int8, columns=attr, index=pd.RangeIndex(n_hh))
    for i, a in enumerate(attr):
        if a == 'N_ADULTS':
            result.values[:, i] = hhv_types[a].cat.codes.iloc[draws] + 1
        else:
            result.values[:, i] = hhv_types[a].cat.codes.iloc[draws]
    return result
