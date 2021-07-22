import numpy as np
import pandas as pd
import warnings
from .time_of_day_model import time_period_names, time_period_codes

import logging
log = logging.getLogger('CMAP')

def compute_deadhead_trip_table(
        dh,
        trips,
        intrazonal_factor=0.5,
        traveltime_factor=-1.0,
        stats=False,
):
    """
    Compute a trip table of deadhead trips taken by taxis and TNCs.

    Parameters
    ----------
    dh : DataHandler
    trips : dask.DataFrame
        The assembled primary trips (not including hired car deadheading)
        as a dask DataFrame.
    intrazonal_factor : float
        The pseudo-value of travel time for intrazonal trips, which include
        implied travel time as well as a constant adjustment.
    traveltime_factor : float, default -1.0
        A parameter to calibrate the overall length of deadhead trips, used as
        the coefficient in a gravity model.

    Returns
    -------
    DataFrame
    """
    result = {}

    for tname, tcode in zip(time_period_names, time_period_codes):
        log.info(f"compute_deadhead_trip_table: {tname}")

        # We try this twice, as there is a rare sync problem in numexpr that
        # makes this fail.
        try:
            trips_by_od = (
                trips
                .query("mode in (4,5,6)")
                .query(f"timeperiod in ('{tname}',)")
                .groupby(["o_zone", "d_zone"])['trips']
                .sum()
                .compute()
                .unstack(0)
                .fillna(0)
            ).reindex(
                index=dh.skims.raw[f'mf46{tcode}'].indexes['otaz'],
                columns=dh.skims.raw[f'mf46{tcode}'].indexes['dtaz'],
            ).fillna(0)
        except KeyError:
            import numexper.necompiler
            numexper.necompiler._numexpr_cache.clear()
            trips_by_od = (
                trips
                .query("mode in (4,5,6)")
                .query(f"timeperiod in ('{tname}',)")
                .groupby(["o_zone", "d_zone"])['trips']
                .sum()
                .compute()
                .unstack(0)
                .fillna(0)
            ).reindex(
                index=dh.skims.raw[f'mf46{tcode}'].indexes['otaz'],
                columns=dh.skims.raw[f'mf46{tcode}'].indexes['dtaz'],
            ).fillna(0)

        deadhead_dests = trips_by_od.sum()  # hired car trip origins
        deadhead_origs = trips_by_od.sum(1)  # hired car trip destinations

        auto_travel_time = dh.skims.raw[f'mf46{tcode}'].values
        intrazonal_value = np.eye(auto_travel_time.shape[0]) * intrazonal_factor
        wgt_matrix = np.exp(traveltime_factor*(auto_travel_time + intrazonal_value))

        # Fratar the deadhead weights to the deadhead productions and attractions
        for iter in range(10):
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", category=RuntimeWarning)
                w = np.nan_to_num(wgt_matrix / wgt_matrix.sum(0)) * np.expand_dims(deadhead_dests, 0)
                wgt_matrix = np.nan_to_num(w / np.expand_dims(w.sum(1), 1)) * np.expand_dims(deadhead_origs, 1)

        chx = {}

        triptable = np.zeros(wgt_matrix.shape, dtype=np.int32)

        for row in range(wgt_matrix.shape[0]):
            rowsum = wgt_matrix[row].sum()
            n = int(np.round(rowsum))
            if n:
                chx[row] = np.random.choice(wgt_matrix.shape[1], size=n, p=wgt_matrix[row] / rowsum)
                for k in chx[row]:
                    triptable[row, k] += 1

        triptable = pd.DataFrame(
            triptable,
            index=dh.skims.raw[f'mf46{tcode}'].indexes['otaz'],
            columns=dh.skims.raw[f'mf46{tcode}'].indexes['dtaz'],
        ).fillna(0)
        triptable.index.name = 'o_zone'
        triptable.columns.name = 'd_zone'
        triptable = triptable.stack().fillna(0).reset_index()

        triptable['timeperiod'] = tname
        triptable['mode'] = 1
        triptable['purpose'] = 'DEAD'
        triptable['a_zone'] = triptable['d_zone']

        triptable = triptable.set_index([
            'purpose', 'mode', 'o_zone', 'd_zone', 'a_zone', 'timeperiod'
        ])

        result[tname] = triptable.loc[triptable.iloc[:,0] > 0].copy()
        result[tname].columns = ['trips']

        if stats:
            import matplotlib.pyplot as plt
            from xmle import Show
            from IPython.display import display

            tt = dh.skims.raw[[f'mf46{tcode}']].at_df(
                result[tname]
                .reset_index()[['o_zone', 'd_zone']]
                .rename(columns=dict(o_zone='otaz', d_zone='dtaz'))
            )
            rr = result[tname].copy()
            rr['time'] = tt[f'mf46{tcode}'].values
            fig, ax = plt.subplots()
            ax.hist(rr['time'], weights=rr['trips'])
            ax.set_title(tname)
            ax.set_xlabel("minutes")
            display(Show(fig))
            plt.close()

    return pd.concat([r for r in result.values()])
