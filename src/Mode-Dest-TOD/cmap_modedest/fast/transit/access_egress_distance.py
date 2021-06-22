import numpy as np
import numba as nb

from ...modecodes import *
FRONT_END = 0
BACK_END = 1
N_TRIP_ENDS = 2


@nb.njit
def trunc_normal(loc, scale):
    rv = np.float32(np.random.normal(loc, scale))
    return max(rv, 0.06)


@nb.njit
def sloped_linear(x_min, x_max, ratio=1.0):
    if ratio == 1.0:
        return np.float32(np.random.uniform(x_min, x_max))
    else:
        span = x_max - x_min
        if span != 0:
            slope = (1.0-ratio)/span
        else:
            slope = (1.0-ratio)
        area =.5*(1+ratio)*span
        y = np.float32(np.random.uniform(0.0, 1.0)) * area
        zp = np.sqrt(ratio * ratio + 2 * slope * y)
        if slope:
            return ((zp - ratio) / slope) + x_min
        else:
            return (zp - ratio) + x_min


@nb.njit
def simulate_ae_dist(p1,p2,p3):

    # use_normal = (p3 == 101)
    if p3 == 101:
        return trunc_normal(p1,p2)

    # use_slopey = (p3 < 101)
    if p3 < 101:
        return sloped_linear(p1, p2, p3)

    #use_nan = (p3 == 999)
    return 255.0



def compile_simulate_approach_distances(
        dh,
):
    """

    Parameters
    ----------
    zone : array-like
        Zone id (1-based)
    attached_mode : int
        Number for first or last mode (as matches this approach)
    trip_purpose : {'HW','HO','NH'}
        Trip purpose, used to select DISTR table and possibly filter
        approach modes
    trip_end : {0,1}
        Zero if approach to first mode, one if approach from last mode
    out : array-like
        Output array must already exist, as a float dtype,
        with shape [replications, N_APPROACH_MODES, ]

    """
    # random_state = check_random_state(random_state)

    distr_array = np.stack([
        dh.distr['HW'],
        dh.distr['HO'],
        dh.distr['NH'],
    ]).reshape((3,-1,5,3)).astype(np.float32)

    _BUS = dh.distr['HW'].loc[1].index.get_loc('bus')
    _CTARAIL = dh.distr['HW'].loc[1].index.get_loc('ctarail')
    _FEEDERBUS = dh.distr['HW'].loc[1].index.get_loc('feederbus')
    _METRA = dh.distr['HW'].loc[1].index.get_loc('metra')
    _PNR = dh.distr['HW'].loc[1].index.get_loc('pnr')

    @nb.njit(parallel=True)
    def simulate_approach_distances_arr(
            zone,
            attached_mode,
            trip_purpose,
            trip_end,
            out,
    ):
        """

        Parameters
        ----------
        zone : int
        attached_mode : int
        trip_purpose : int
        trip_end : int
        out : array[float32], shape=(5,)

        Returns
        -------

        """
        assert out.shape[-1] == 5, "last dimension must be 5"
        if trip_purpose == 'HW':
            distr_ = distr_array[0,zone-1]
        elif trip_purpose == 'HO':
            distr_ = distr_array[1,zone-1]
        else: #if trip_purpose == 'NH':
            distr_ = distr_array[2,zone-1]
        for J in range(N_DIST_TO_TYPES):
            # OBTAIN APPROACH DISTANCES TO FIVE MODES
            if (J == DIST_TO_BUS):
                distr_params = distr_[_BUS]
            elif (J == DIST_TO_CTA_RAIL) and (attached_mode == TransitModeCode_CTA_RAIL):
                # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS CTA RAIL
                distr_params = distr_[_CTARAIL]
            elif (J == DIST_TO_METRA) and (attached_mode == TransitModeCode_METRA_RAIL):
                # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
                distr_params = distr_[_METRA]
            elif (J == DIST_TO_FEEDER_BUS) and (attached_mode == TransitModeCode_METRA_RAIL):
                # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
                distr_params = distr_[_FEEDERBUS]
            elif (J == DIST_TO_PARK_N_RIDE_STATION) and (trip_end == FRONT_END):
                # PARK AND RIDE STATION DISTANCE OBTAINED WHEN TRIP END IS FRONT
                distr_params = distr_[_PNR]
            else:
                distr_params = np.full(3, 999, dtype=np.float32)
            if out.ndim == 2:
                for k in range(out.shape[0]):
                    out[k,J] = simulate_ae_dist(
                        distr_params[0],
                        distr_params[1],
                        distr_params[2],
                    )
            else:
                out[J] = simulate_ae_dist(
                    distr_params[0],
                    distr_params[1],
                    distr_params[2],
                )

    @nb.njit(parallel=True)
    def simulate_approach_distances_multi(
            zones,
            attached_modes,
            trip_purpose,
            trip_end,
            outs,
    ):
        assert zones.shape[0] == attached_modes.shape[0] == outs.shape[0]
        for i in nb.prange(zones.shape[0]):
            simulate_approach_distances_arr(
                zones[i],
                attached_modes[i],
                trip_purpose,
                trip_end,
                outs[i],
            )

    return simulate_approach_distances_arr, simulate_approach_distances_multi