import pickle

import numpy as np
import pandas as pd
import os
import time
import larch
from larch.util import piece
from larch.util.dataframe import columnize
import re
from .addict import Dict
import pyarrow as pa
import pyarrow.feather as pf
import sharrow as sh
from scipy.stats import binom
from pathlib import Path

from .tnc_costs import taxi_cost, tnc_solo_cost, tnc_pool_cost
from .transit_approach import transit_approach
from .modecodes import mode9codes
from .choice_model import model_builder, alt_codes_and_names
from .random_states import check_random_state
from .data_handlers import DataHandler
from .data_handlers.tabler import Table
from .data_handlers.m01_handler import attach_areatypes, sample_hh_income_cats
from .hh_sampling import sample_hh_from_zone
from .fast.random_generator import multichoices
from .deadheading import compute_deadhead_trip_table
from .purposes import purposesA, purposes_to_5, purposes_to_3
from .time_of_day_model import time_of_day_simulator_initialize

from .cmap_logging import getLogger, get_worker_log

app_floatdtype = np.float64

log = getLogger()

n_modes = len(mode9codes)

av = {}


def _data_for_application_1(dh, otaz=1, replication=None):
    """
    
    Parameters
    ----------
    dh : DataHandler
    otaz : int
    peak : bool
    replication : int, optional
    
    Returns
    -------
    pd.DataFrame
    """
    global av

    try:
        fast_application_data = dh.fast_application_data
    except AttributeError:
        from .fast.application_data import application_data
        fast_application_data = dh.fast_application_data = application_data(dh)

    from .purposes import purposes5
    purposes = purposes5

    log.info("_data_for_application_1::prepare availability")
    n_zones = dh.n_internal_zones
    for purpose in purposes:
        av_purpose = av.get(purpose, {})
        if len(av_purpose) != n_zones * n_modes:
            av_purpose = {}
            num = n_modes
            for i in range(n_zones):
                av_purpose[num + mode9codes.AUTO] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.HOV2] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.HOV3] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.TNC1] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.TNC2] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.TAXI] = f"altdest{i + 1:04d}_auto_avail_{purpose}"
                av_purpose[num + mode9codes.TRANSIT] = f"altdest{i + 1:04d}_transit_avail_{purpose}"
                av_purpose[num + mode9codes.WALK] = f"altdest{i + 1:04d}_walk_avail_{purpose}"
                av_purpose[num + mode9codes.BIKE] = f"altdest{i + 1:04d}_bike_avail_{purpose}"
                num += n_modes
        av[purpose] = av_purpose

    if replication is None:
        replication = dh.cfg.get('n_replications', 50)

    log.info("_data_for_application_1::initialize dtaz and tbl")
    dtaz = pd.Index(np.arange(n_zones) + 1)
    otaz_series = pd.Series(otaz, index=dtaz)

    tbl = pa.table({
        'otaz_idx': np.full_like(dtaz, otaz) - 1,
        'dtaz_idx': dtaz - 1,
        'otaz': np.full_like(dtaz, otaz),
        'dtaz': dtaz,
    })

    log.info("_data_for_application_1::load t1")
    t1 = fast_application_data.load(tbl, as_table=True)

    log.info("_data_for_application_1::concat_tables t2")
    t2 = sh.concat_tables([t1] * replication)

    log.info("_data_for_application_1::transit_approach_func")
    from .fast.transit.approach import compile_transit_approach, transit_approach_wrap, transit_approach_distances
    if not dh.cfg.transit_approach_func:
        trapp_struct = compile_transit_approach(dh)
        dh.cfg.transit_approach_struct = trapp_struct

    oz = np.full(3632, otaz)
    dz = np.arange(3632) + 1

    log.debug("_data_for_application_1::trapp_dist_HW")
    trapp_dist_HW = transit_approach_distances(dh.cfg.transit_approach_struct, oz, dz, 'HW', random_seed=otaz)[0]
    log.debug("_data_for_application_1::trapp_HW")
    trapp_HW = transit_approach_wrap(dh.cfg.transit_approach_struct, oz, dz, 'HW', trapp_dist_HW, )

    log.debug("_data_for_application_1::trapp_dist_HO")
    trapp_dist_HO = transit_approach_distances(dh.cfg.transit_approach_struct, oz, dz, 'HO', random_seed=otaz << 13)[0]
    log.debug("_data_for_application_1::trapp_HO")
    trapp_HO = transit_approach_wrap(dh.cfg.transit_approach_struct, oz, dz, 'HO', trapp_dist_HO, )

    log.debug("_data_for_application_1::compute columns on t2")
    t2['transit_approach_drivetime_PEAK'] = trapp_HW['drivetime'].T.reshape(-1)
    t2['transit_approach_waittime_PEAK'] = trapp_HW['waittime'].T.reshape(-1)
    t2['transit_approach_walktime_PEAK'] = trapp_HW['walktime'].T.reshape(-1)
    t2['transit_approach_cost_PEAK'] = trapp_HW['cost'].T.reshape(-1)

    t2['transit_approach_drivetime_OFFPEAK'] = trapp_HO['drivetime'].T.reshape(-1)
    t2['transit_approach_waittime_OFFPEAK'] = trapp_HO['waittime'].T.reshape(-1)
    t2['transit_approach_walktime_OFFPEAK'] = trapp_HO['walktime'].T.reshape(-1)
    t2['transit_approach_cost_OFFPEAK'] = trapp_HO['cost'].T.reshape(-1)
    # TODO transit approach varies in HO vs NH

    try:
        fast_application_data_2 = dh.fast_application_data_2
    except AttributeError:
        log.debug("_data_for_application_1: creating new fast_application_data_2 object")
        from .fast.application_data import application_data2
        fast_application_data_2 = dh.fast_application_data_2 = application_data2(dh, t2)
    else:
        log.debug("_data_for_application_1: using stored fast_application_data_2 object")

    t2 = fast_application_data_2.merge(t2, dtype=app_floatdtype)

    hh_data = sample_hh_from_zone(dh, otaz, replication, random_state=otaz << 1, )
    hh_data['hhinc5'] = sample_hh_income_cats(dh, otaz, len(hh_data), random_state=otaz << 2,).astype(app_floatdtype)
    hh_data['hhinc5h'] = sample_hh_income_cats(dh, otaz, len(hh_data), random_state=otaz << 2, trunc_min=60_000).astype(app_floatdtype)

    # TODO hhinc3 for correct purposes HBW

    hh_data['hhinc3'] = (hh_data['hhinc5'] - 1) // 2
    hh_data['hhinc3==0'] = (hh_data['hhinc3'] == 0).astype(app_floatdtype)
    hh_data['hhinc3==1'] = (hh_data['hhinc3'] == 1).astype(app_floatdtype)
    hh_data['hhinc3==2'] = (hh_data['hhinc3'] == 2).astype(app_floatdtype)
    hh_data['hhinc3h'] = (hh_data['hhinc5h'] - 1) // 2
    hh_data['hhinc3h==1'] = (hh_data['hhinc3h'] == 1).astype(app_floatdtype)
    hh_data['hhinc3h==2'] = (hh_data['hhinc3h'] == 2).astype(app_floatdtype)
    hh_data['o_zone'] = app_floatdtype(otaz)
    hh_data['ozone_autopropensity'] = app_floatdtype(
        attach_areatypes(dh, pd.DataFrame(index=[otaz]), "", "", targetzone=[otaz])['autopropensity'].iloc[0]
    )
    hh_data['hhveh==0'] = (hh_data['N_VEHICLES'] == 0).astype(app_floatdtype)
    hh_data['hhveh>=hhadults'] = (hh_data['N_VEHICLES'] >= hh_data['N_ADULTS']).astype(app_floatdtype)

    df2 = t2.to_pandas()

    # This can be fairly fast because we're not adding new columns, just overwriting dummy columns.
    # parking cost
    from .parking_costs import parking_cost_v3, parking_is_free
    dtazs = sh.concat_tables([tbl.select(['dtaz'])] * replication).to_pandas().values
    hhincs = hh_data['hhinc5'].repeat(len(tbl)).values

    # Whether a trip gets free parking is a function of income but not destination or purpose
    paid_parking = parking_is_free(
        dh,
        hhincs,
        random_state=otaz << 5,
    )

    for purp in purposesA:
        temp_parking_cost = parking_cost_v3(
            dh,
            dtazs,
            dh.cfg.default_activity_durations[purposes_to_3[purp]],
            purposes_to_3[purp],
            random_state=otaz << 4,
        ).values
        temp_parking_cost *= paid_parking.astype(temp_parking_cost.dtype).values
        df2[f'auto_parking_cost_{purp}'] = temp_parking_cost

    df2_array = df2.to_numpy(dtype=app_floatdtype).reshape(replication, -1)

    col_names = getattr(dh, 'column_2_replacement', [])
    try:
        df3 = pd.DataFrame(
            df2_array,
            columns=col_names,
        )
    except ValueError:
        df3 = pd.DataFrame(
            df2.to_numpy(dtype=app_floatdtype).reshape(replication, -1),
            columns=pd.MultiIndex.from_product([
                [f"altdest{x:04d}" for x in range(1, n_zones + 1)],
                df2.columns,
            ])
        )
        need_to_fix_column_names = True
    else:
        need_to_fix_column_names = False
    # hh_data = sample_hh_from_zone(dh, otaz, len(df3), random_state=otaz << 1, )
    hh_data.index = df3.index
    # hh_data['hhinc5'] = sample_hh_income_cats(dh, otaz, len(hh_data), random_state=otaz << 2,).astype(app_floatdtype)
    # hh_data['hhinc3'] = (hh_data['hhinc5'] - 1) // 2
    # hh_data['hhinc3==0'] = (hh_data['hhinc3'] == 0).astype(app_floatdtype)
    # hh_data['hhinc3==1'] = (hh_data['hhinc3'] == 1).astype(app_floatdtype)
    # hh_data['hhinc3==2'] = (hh_data['hhinc3'] == 2).astype(app_floatdtype)
    # hh_data['o_zone'] = app_floatdtype(otaz)
    # hh_data['ozone_autopropensity'] = app_floatdtype(
    #     attach_areatypes(dh, pd.DataFrame(index=[otaz]), "", "", targetzone=[otaz])['autopropensity'].iloc[0]
    # )
    # hh_data['hhveh==0'] = (hh_data['N_VEHICLES'] == 0).astype(app_floatdtype)
    # hh_data['hhveh>=hhadults'] = (hh_data['N_VEHICLES'] >= hh_data['N_ADULTS']).astype(app_floatdtype)
    addon = hh_data[[
        'o_zone', 'ozone_autopropensity', 'hhveh==0', 'hhveh>=hhadults',
        'hhinc3', 'hhinc3==0', 'hhinc3==1', 'hhinc3==2',
        'hhinc3h', 'hhinc3h==1', 'hhinc3h==2',
    ]]
    df3 = pd.concat([df3, addon], axis=1)
    if need_to_fix_column_names:
        _fix_column_names(dh, df3)
    return df3


def _fix_column_names(dh, dfx):
    column_2_replacement = getattr(dh, 'column_2_replacement', [])
    if len(dfx.columns) != len(column_2_replacement):
        columns = [(f"{j[0]}_{j[1]}" if isinstance(j, tuple) else j) for j in dfx.columns]
        if columns[-1] == '_o_zone':
            columns[-1] = 'o_zone'
        s0f = lambda c: c.replace("_dtaz", "")

        s1 = re.compile("(altdest[0-9]+)_o_zone == dtaz")
        s1f = lambda c: s1.sub("o_zone == \g<1>", c)

        s2 = re.compile("(altdest[0-9]+_)piece\((.*)\)")
        s2f = lambda c: s2.sub(r"piece(\g<1>\g<2>)", c)

        s3 = re.compile(r"(altdest[0-9]+_)log\(attractions\)")
        s3f = lambda c: s3.sub(r"log(\g<1>attractions)", c)

        s4 = re.compile(r"(altdest[0-9]+_)log\(1/samp_wgt\)")
        s4f = lambda c: s4.sub(r"log(1/\g<1>samp_wgt)", c)

        s5 = re.compile("(altdest[0-9]+_)1-(.*)")
        s5f = lambda c: s5.sub(r"1-\g<1>\g<2>", c)

        s6 = re.compile(r"(altdest[0-9]+_)(.*)(time|ovtt)(.*)/auto_dist_(.*)")
        s6f = lambda c: s6.sub(r"\g<1>\g<2>\g<3>\g<4>/\g<1>auto_dist_\g<5>", c)

        s7 = re.compile("(altdest[0-9]+_)(fm..)\(ozone_areatype, areatype\)==(.*)")
        s7f = lambda c: s7.sub(r"\g<2>(ozone_areatype, \g<1>areatype)==\g<3>", c)

        s8 = re.compile("(altdest[0-9]+_)hard_sigmoid\((.*)\)")
        s8f = lambda c: s8.sub(r"hard_sigmoid(\g<1>\g<2>)", c)

        column_2_replacement = [s8f(s7f(s6f(s5f(s4f(s3f(s2f(s1f(s0f(j))))))))) for j in columns]
        dh.column_2_replacement = column_2_replacement

    dfx.columns = column_2_replacement


to_disk = False


def _data_for_application_2(dh, df2, filename):
    # _fix_column_names(dh, df2)

    n_zones = dh.n_internal_zones
    alt_codes, alt_names = alt_codes_and_names(
        n_sampled_dests=n_zones,
        include_actual_dest=False,
    )

    log.debug("_data_for_application_2::dfas")
    dfas = larch.DataFrames(
        co=df2.astype(app_floatdtype),
        alt_codes=alt_codes,
        av=True,
        # av=columnize(df2, av, inplace=False, dtype=np.int8)
    )

    if to_disk:
        dfas.to_feathers(filename)
        return _reload_data_for_application_2(dh, filename)

    log.debug("_data_for_application_2::return")
    return dfas


def _reload_data_for_application_2(dh, filename):
    return larch.DataFrames.from_feathers(filename)


def data_for_application(dh, otaz=1, replication=None):
    """

    Parameters
    ----------
    otaz : int or array-like
    peak : bool
    purpose : str
    replication : int, optional

    Returns
    -------

    """
    if replication is None:
        replication = dh.cfg.get('n_replications', 50)

    if isinstance(otaz, int):
        df2 = _data_for_application_1(dh, otaz=otaz, replication=replication)
        filename = dh.filenames.cache_dir / f"data_for_application_{otaz}"
    else:
        log.debug("data_for_application::_data_for_application_1")
        df2_list = [
            _data_for_application_1(dh, otaz=z, replication=replication)
            for z in otaz
        ]
        log.debug("data_for_application::concat")
        df2 = pd.concat(df2_list)
        log.debug("data_for_application::filename")
        filename = dh.filenames.cache_dir / f"data_for_application_{otaz[0]}_{otaz[-1]}"
    return _data_for_application_2(dh, df2.reset_index(drop=True), filename)


def blockwise_mean(a, blocksize):
    """

    Parameters
    ----------
    a : array-like
    blocksize : int

    Returns
    -------
    array
    """
    log.debug(f"blockwise_mean")
    n_blocks = a.shape[0] // blocksize + (1 if a.shape[0] % blocksize else 0)
    mean = np.zeros([n_blocks, *a.shape[1:]])
    for j in range(n_blocks):
        mean[j] = a[j * blocksize:(j + 1) * blocksize].mean(0)
    return mean


choice_simulator_global = Dict()


def choice_simulator_initialize(dh, return_simulators=True, n_threads=1, cache=True):
    global choice_simulator_global

    get_worker_log(
        os.path.join(dh.filenames.cache_dir, 'logs'),
        level=10,
    )

    log.debug(f"choice_simulator_initialize(n_threads={n_threads}), cache={cache}")
    auto_cost_per_mile = dh.cfg.auto.cost.per_mile
    n_zones = dh.n_internal_zones
    choice_model_params = dh.choice_model_params

    if len(choice_model_params) == 0:
        raise ValueError("no choice_model_params")

    pickle_name = dh.filenames.cache_dir / f"choice_models_{auto_cost_per_mile}_{n_zones}.pkl"

    if (auto_cost_per_mile, n_zones) not in choice_simulator_global:
        log.debug("choice_simulator_initialize: preloaded choice_simulator not available")
        if os.path.exists(pickle_name):
            import cloudpickle
            with open(pickle_name, 'rb') as pkl_f:
                log.debug("choice_simulator_initialize: loading pickled choice_simulator")
                choice_simulator_global[(auto_cost_per_mile, n_zones)] = cloudpickle.load(pkl_f)
            if (auto_cost_per_mile, n_zones) in choice_simulator_global:
                cache = False
        else:
            log.debug("choice_simulator_initialize: pickled choice_simulator not available")

    if (auto_cost_per_mile, n_zones) in choice_simulator_global:
        log.info("choice_simulator_initialize: using existing choice_simulator")
        choice_simulator = choice_simulator_global[(auto_cost_per_mile, n_zones)]
    else:
        log.info("choice_simulator_initialize: creating fresh choice_simulator")
        choice_simulator = Dict()
        for purpose in purposesA:
            choice_simulator[purpose] = model_builder(
                purpose=purpose,
                include_actual_dest=False,
                n_sampled_dests=n_zones,  # 3632,
                parameter_values=choice_model_params[purpose],
                constraints=False,
                n_threads=n_threads,
                explicit_av=False,
            )
        choice_simulator_global[(auto_cost_per_mile, n_zones)] = choice_simulator

    if cache and not os.path.exists(pickle_name):
        import cloudpickle
        with open(pickle_name, 'wb') as pkl_f:
            log.debug("choice_simulator_initialize: pickling choice_simulator for future reload")
            cloudpickle.dump(
                choice_simulator_global[(auto_cost_per_mile, n_zones)],
                pkl_f,
            )

    if return_simulators:
        return choice_simulator


def attach_dataframes(sim, purpose, dfa):
    if sim.dataframes is None:
        log.debug(f"attach_dataframes {purpose} attach dataframes new")
        sim.dataframes = dfa
    else:
        log.debug(f"attach_dataframes {purpose} attach dataframes direct injection")
        sim.set_dataframes(dfa, False)
    # TODO: explore using inject_feathers instead


def _sim_prob(purpose, sim):
    log.debug(f"choice_simulator_prob {purpose} simulate probability")
    sim_pr = sim.probability()
    return sim_pr


def choice_simulator_prob(
        dh,
        otaz,
        n_threads=1,
        temp_dir=None,
        purposes=None,
):
    """

    Parameters
    ----------
    otaz : int or array-like

    Returns
    -------

    """
    if purposes is None:
        purposes = purposesA
    get_worker_log(
        os.path.join(dh.filenames.cache_dir, 'logs'),
        level=10,
    )

    data_cache_file = None
    if temp_dir:
        os.makedirs(temp_dir, exist_ok=True)
        if isinstance(otaz, int):
            data_cache_file = os.path.join(temp_dir, f"cached_data_for_application_{otaz}.feathers")
        else:
            data_cache_file = os.path.join(temp_dir, f"cached_data_for_application_{otaz[0]}_{otaz[-1]}.feathers")

    log.debug("choice_simulator_prob data_for_application")
    if data_cache_file and os.path.exists(data_cache_file+".data_co"):
        log.debug("choice_simulator_prob data_for_application load")
        dfa = larch.DataFrames.from_feathers(data_cache_file)
    else:
        log.debug("choice_simulator_prob data_for_application make")
        dfa = data_for_application(dh, otaz=otaz)
        if data_cache_file:
            dfa.to_feathers(data_cache_file)

    log.debug("choice_simulator_prob settings")
    replication = dh.cfg.get('n_replications', 50)

    choice_simulator = choice_simulator_initialize(dh, n_threads=n_threads)
    simulated_probability = {}
    simulated_probability_disagg = {}

    for purpose in purposes:
        sim = choice_simulator[purpose]
        attach_dataframes(sim, purpose, dfa)
        # log.debug(f"choice_simulator_prob {purpose} attach dataframes")
        # if sim.dataframes is None:
        # 	sim.dataframes = dfa
        # else:
        # 	sim.set_dataframes(dfa, False)
        # log.debug(f"choice_simulator_prob {purpose} simulate probability")
        sim_pr = _sim_prob(purpose, sim)
        simulated_probability_disagg[purpose] = sim_pr
        simulated_probability[purpose] = blockwise_mean(sim_pr, replication)
        if np.any(np.isnan(simulated_probability[purpose])):
            raise ValueError(f"nan in simulated_probability[{purpose}]")

    #transit_approach_walktime_cols = [i for i in dfa.data_co.columns if 'transit_approach_walktime' in i and 'auto' not in i and 'sigmoid' not in i]
    validation_useful_data = pd.DataFrame(data=np.int8(0), index=dfa.data_co.index, columns=["hh_auto_own", 'hhinc3', 'hhinc3h'])
    validation_useful_data.loc[dfa.data_co['hhveh==0'] == 0, "hh_auto_own"] = 1
    validation_useful_data.loc[dfa.data_co['hhveh==0'] > 0, "hh_auto_own"] = 0
    validation_useful_data.loc[dfa.data_co['hhveh>=hhadults'] > 0, "hh_auto_own"] = 2
    validation_useful_data['hhinc3'] = dfa.data_co['hhinc3']
    validation_useful_data['hhinc3h'] = dfa.data_co['hhinc3h']
    log.debug("choice_simulator_prob complete")
    return simulated_probability, simulated_probability_disagg, validation_useful_data


def choice_simulator_trips(
        dh,
        otaz,
        purposes=None,
        random_state=None,
        n_threads=1,
        save_dir=None,
        delay=0,
        temp_dir=None,
        disagg_choices=True,
        use_wfh_pa=False,
):
    """

    Parameters
    ----------
    otaz : int or array-like
    purposes : Collection, optional

    Returns
    -------

    """
    get_worker_log(
        os.path.join(dh.filenames.cache_dir, 'logs'),
        level=10,
    )
    try:
        if delay:
            log.debug(f"DELAY choice_simulator_trips {delay})")
            time.sleep(delay)

        if use_wfh_pa:
            dh['tripclass'] = 'wfh'
        else:
            dh['tripclass'] = 'typical'

        if purposes is None:
            purposes = purposesA

        if isinstance(otaz, int):
            otaz = [otaz]

        log.debug(f"CALL choice_simulator_trips({len(otaz)} OTAZ's starting from {otaz[0]})")

        simulated_probability, simulated_probability_disagg, validation_data = choice_simulator_prob(
            dh,
            otaz=otaz,
            n_threads=n_threads,
            temp_dir=temp_dir,
        )
        simulated_choices = {}

        tod_models = time_of_day_simulator_initialize(dh, cache=True)

        random_state = check_random_state(random_state or otaz[0])
        for purpose in purposes:
            choices_data = {}
            n = 0

            for _o in otaz:
                num_productions = dh.zone_productions5.loc[_o, purpose]

                if disagg_choices:
                    c = np.empty(num_productions, dtype=np.int32)
                    hh_autos = np.zeros(num_productions, dtype=np.int8)
                    hh_inc3 = np.zeros(num_productions, dtype=np.int8)
                    c_position = 0
                    num_reps = 50
                    for _rep in range(num_reps):
                        num_productions_rep = (num_productions // num_reps) + (1 if (num_productions % num_reps)>_rep else 0)
                        p = simulated_probability_disagg[purpose][n]
                        try:
                            c_ = random_state.choice(p.size, size=num_productions_rep, p=p)
                        except ValueError as err:
                            if 'probabilities do not sum to 1' in str(err):
                                log.error(f"probabilities sum to {np.sum(p)}, correcting")
                                p /= np.sum(p)
                                c_ = random_state.choice(p.size, size=num_productions, p=p)
                            else:
                                raise
                        c[c_position:c_position+c_.size] = c_
                        hh_autos[c_position:c_position+c_.size] = validation_data["hh_auto_own"][n]
                        if purpose == 'HBWH':
                            hh_inc3[c_position:c_position+c_.size] = validation_data["hhinc3h"][n]
                        elif purpose == 'HBWL':
                            hh_inc3[c_position:c_position+c_.size] = 0 # all low income
                        else:
                            hh_inc3[c_position:c_position+c_.size] = validation_data["hhinc3"][n]
                        c_position += c_.size
                        n += 1

                else:
                    p = simulated_probability[purpose][n]
                    try:
                        c = random_state.choice(p.size, size=num_productions, p=p)
                    except ValueError as err:
                        if 'probabilities do not sum to 1' in str(err):
                            log.error(f"probabilities sum to {np.sum(p)}, correcting")
                            p /= np.sum(p)
                            c = random_state.choice(p.size, size=num_productions, p=p)
                        else:
                            raise
                    n += 1
                    hh_autos = np.zeros_like(c)
                    hh_inc3 = np.zeros_like(c)

                choices_data[_o] = pd.DataFrame(dict(
                    mode=(c % n_modes) + 1,
                    zone=(c // n_modes) + 1,

                    # other things to track for calibration / validation
                    hh_autos=hh_autos,
                    hh_inc3=hh_inc3,

                )).value_counts().sort_index().rename(_o).astype(np.int16)

            full_index = pd.MultiIndex.from_product(
                [
                    np.arange(n_modes) + 1,
                    np.arange(dh.n_internal_zones) + 1,
                    np.arange(3),
                    np.arange(3),
                ],
                names=[
                    'mode',
                    'a_zone',
                    'hh_autos',
                    'hh_inc3',
                ],
            )

            simulated_choices_purpose = pd.concat(
                choices_data,
                axis=1,
            ).reindex(full_index).fillna(0).astype(np.int16)
            simulated_choices_purpose.columns.name = 'p_zone'

            # simulated_choices_purpose is now a dataframe, with columns giving
            #  the production zones (by batch) and a dense 2-level index (mode, a_zone)

            simtrips = simulated_choices_purpose.stack().rename("trips").reset_index()

            reg_auto_trips = simtrips.query("(mode in (1,2,3)) and (trips > 0)")

            def apply_tod(base_trips, is_hired_car=0.0):

                time_data = dh.skims.raw[[f'mf46{j}' for j in range(1, 9)]].iat_df(
                    base_trips.rename(columns={'a_zone': 'dtaz', 'p_zone': 'otaz'})[['otaz', 'dtaz']] - 1
                )
                time_data["mode9 in ('TAXI','TNC1','TNC2')"] = is_hired_car
                time_data["paFlip"] = 0.0
                time_data["(mode9 in ('TAXI','TNC1','TNC2'))*paFlip"] = 0.0
                time_dfs = larch.DataFrames(
                    co=time_data.astype(app_floatdtype),
                    av=1,
                    alt_codes=[1, 2, 3, 4, 5, 6, 7, 8],
                    alt_names=['EA', 'AM1', 'AM2', 'AM3', 'MD', 'PM1', 'PM2', 'PM3'],
                )
                tod_model = tod_models[purposes_to_5[purpose]]
                tod_model.dataframes = time_dfs
                tod_model_pr = tod_model.probability().copy()
                tod_model.dataframes.data_co["paFlip"] = 1.0
                tod_model.dataframes.data_co["(mode9 in ('TAXI','TNC1','TNC2'))*paFlip"] = is_hired_car
                tod_model_pr_flip = tod_model.probability().copy()
                n_flipped_trips = binom.rvs(
                    n=base_trips["trips"].values,
                    p=dh.cfg.time_of_day.pa_split.get(purpose, 0.0 if purpose=='NHB' else 0.5),
                    random_state=random_state,
                )
                n_unflipped_trips = base_trips["trips"].values - n_flipped_trips

                # forward trips, which go from production to attraction (PA = OD)
                reg_auto_trips_tod = pd.DataFrame(
                    multichoices(
                        tod_model_pr,
                        n_unflipped_trips,
                        seeds=base_trips.index.values+base_trips["p_zone"].values<<16,
                    ),
                    index=base_trips.index,
                    columns=['EA', 'AM1', 'AM2', 'AM3', 'MD', 'PM1', 'PM2', 'PM3'],
                )
                reg_auto_trips_fwd = pd.concat([base_trips.drop(columns=['trips']), reg_auto_trips_tod], axis=1)
                reg_auto_trips_fwd_= reg_auto_trips_fwd.rename(columns={'p_zone': 'o_zone', 'a_zone': 'd_zone'})
                reg_auto_trips_fwd_['a_zone'] = reg_auto_trips_fwd['a_zone']
                reg_auto_trips_fwd = reg_auto_trips_fwd_.set_index(['mode', 'o_zone', 'd_zone', 'a_zone', 'hh_autos', 'hh_inc3'])
                reg_auto_trips_fwd.columns.name = "timeperiod"
                reg_auto_trips_fwd = reg_auto_trips_fwd.stack().rename("trips")

                # flipped trips, which go from attraction to production
                if isinstance(n_flipped_trips, int):
                    n_flipped_trips = np.asarray([n_flipped_trips])
                try:
                    reg_auto_trips_tod_r = pd.DataFrame(
                        multichoices(
                            tod_model_pr_flip,
                            n_flipped_trips,
                            seeds=base_trips.index.values+base_trips["p_zone"].values<<18,
                        ),
                        index=base_trips.index,
                        columns=['EA', 'AM1', 'AM2', 'AM3', 'MD', 'PM1', 'PM2', 'PM3'],
                    )
                except:
                    log.error(f"{tod_model_pr_flip=}")
                    log.error(f"{n_flipped_trips=}")
                    log.exception("error in reg_auto_trips_tod_r")
                    raise
                reg_auto_trips_bwd = pd.concat([base_trips.drop(columns=['trips']), reg_auto_trips_tod_r], axis=1)
                reg_auto_trips_bwd_ = reg_auto_trips_bwd.rename(columns={'p_zone': 'd_zone', 'a_zone': 'o_zone'})
                reg_auto_trips_bwd_['a_zone'] = reg_auto_trips_bwd['a_zone']
                reg_auto_trips_bwd = reg_auto_trips_bwd_.set_index(['mode', 'o_zone', 'd_zone', 'a_zone', 'hh_autos', 'hh_inc3'])
                reg_auto_trips_bwd.columns.name = "timeperiod"
                reg_auto_trips_bwd = reg_auto_trips_bwd.stack().rename("trips")

                return pd.concat([
                    reg_auto_trips_fwd[reg_auto_trips_fwd > 0],
                    reg_auto_trips_bwd[reg_auto_trips_bwd > 0],
                ])


            hired_auto_trips = simtrips.query("(mode in (4,5,6)) and (trips > 0)")

            reg_auto_trips = apply_tod(reg_auto_trips, 0.0)
            hired_auto_trips = apply_tod(hired_auto_trips, 1.0)

            # non-auto trips
            non_auto_trips = simtrips.query("(mode in (7,8,9)) and (trips > 0)")
            non_auto_trips = non_auto_trips.reset_index(drop=True)
            non_auto_trips["timeperiod"] = "NA"
            n_flipped_trips = binom.rvs(
                n=non_auto_trips["trips"].values,
                p=dh.cfg.time_of_day.pa_split.get(purpose, 0.5),
                random_state=random_state,
            )
            n_unflipped_trips = non_auto_trips["trips"].values - n_flipped_trips

            non_auto_trips_attractions = non_auto_trips['a_zone']

            non_auto_trips_r = non_auto_trips.copy(deep=True)
            non_auto_trips_r["trips"] = n_flipped_trips
            non_auto_trips_r = non_auto_trips_r.rename(columns={'p_zone': 'o_zone', 'a_zone': 'd_zone'})
            non_auto_trips_r['a_zone'] = non_auto_trips_attractions
            non_auto_trips_r = non_auto_trips_r.set_index(['mode', 'o_zone', 'd_zone', 'a_zone', 'hh_autos', 'hh_inc3', 'timeperiod'])["trips"]

            non_auto_trips["trips"] = n_unflipped_trips
            non_auto_trips = non_auto_trips.rename(columns={'p_zone': 'd_zone', 'a_zone': 'o_zone'})
            non_auto_trips['a_zone'] = non_auto_trips_attractions
            non_auto_trips = non_auto_trips.set_index(['mode', 'o_zone', 'd_zone', 'a_zone', 'hh_autos', 'hh_inc3', 'timeperiod'])["trips"]

            non_auto_trips = pd.concat([non_auto_trips[non_auto_trips>0], non_auto_trips_r[non_auto_trips_r>0]])
            #non_auto_trips = non_auto_trips.reset_index()

            sim_mode_dest_tod = pd.concat([reg_auto_trips, hired_auto_trips, non_auto_trips])
            if isinstance(sim_mode_dest_tod, pd.Series):
                sim_mode_dest_tod = pd.DataFrame(sim_mode_dest_tod)
            # if save_dir is not None:
            #     sim_mode_dest_tod.to_parquet(os.path.join(save_dir, f"choice_simulator_trips_{purpose}_{otaz[0]}_{otaz[-1]}.pq"))
            simulated_choices[purpose] = sim_mode_dest_tod

            if purpose == 'NHB':

                # Visitor trips
                visitor_choices_data = []
                for otaz_n, otaz_ in enumerate(otaz):

                    visitor_pr = simulated_probability['NHB'][otaz_n]
                    k = 0
                    for dtaz_ in range(1, dh.n_internal_zones+1):
                        n_visitor_trips_here = dh.visitor_trips.iloc[otaz_-1, dtaz_-1]
                        if n_visitor_trips_here == 0:
                            k += 9
                            continue
                        else:
                            visitor_mode_pr = np.nan_to_num(visitor_pr[k:k+9])
                            visitor_mode_pr[mode9codes.TAXI-1] *= dh.cfg.get('visitor_taxi_multiple', 5.0)
                            visitor_mode_pr[mode9codes.TNC1-1] *= dh.cfg.get('visitor_tnc1_multiple', 5.0)
                            visitor_mode_pr[mode9codes.TNC2-1] *= dh.cfg.get('visitor_tnc2_multiple', 5.0)
                            k += 9
                        visitor_mode_pr_sum = visitor_mode_pr.sum()
                        if visitor_mode_pr_sum == 0: continue # this OD pair is not valid
                        visitor_mode_pr /= visitor_mode_pr_sum
                        c = random_state.choice(
                            9,
                            size=n_visitor_trips_here,
                            p=visitor_mode_pr,
                        )
                        visitor_choices_data.append(pd.DataFrame(dict(
                            mode=c+1,
                            p_zone=otaz_,
                            a_zone=dtaz_,
                            hh_autos=-1,
                            hh_inc3=-1,
                        )).value_counts().sort_index().rename('trips').astype(np.int16))

                visitor_choices = pd.concat(visitor_choices_data).reset_index()
                visitor_reg_auto_trips = apply_tod(visitor_choices.query("(mode in (1,2,3)) and (trips > 0)"), 0.0)
                visitor_hired_auto_trips = apply_tod(visitor_choices.query("(mode in (4,5,6)) and (trips > 0)"), 1.0)
                visitor_nonauto_trips = visitor_choices.query("(mode in (7,8,9)) and (trips > 0)").copy()
                visitor_nonauto_trips['timeperiod'] = 'NA'
                visitor_nonauto_trips = visitor_nonauto_trips.rename(columns={'p_zone': 'o_zone', 'a_zone': 'd_zone'})
                visitor_nonauto_trips['a_zone'] = visitor_nonauto_trips['d_zone']
                visitor_choices_with_time = pd.concat([
                    visitor_reg_auto_trips.reset_index(),
                    visitor_hired_auto_trips.reset_index(),
                    visitor_nonauto_trips,
                ], ignore_index=True).set_index(['mode','o_zone','d_zone','a_zone','hh_autos','hh_inc3','timeperiod'])
                simulated_choices['VISIT'] = visitor_choices_with_time

        concatd = pd.concat(simulated_choices)
        concatd.index.set_names('purpose', level=0, inplace=True)
        if save_dir is not None:
            os.makedirs(save_dir, exist_ok=True)
            concise = pd.DataFrame(concatd)
            concise.to_parquet(os.path.join(
                save_dir,
                f"choice_simulator_trips_{otaz[0]}_{otaz[-1]}_{'_'.join(purposes)}_{dh['tripclass']}.pq"
            ))
        return concatd
    except:
        log.exception(f"error in choice_simulator_trips for otaz={otaz}")
        raise

def stagger_starts(iterable, delay=3, n_jobs=4):
    pause = 0
    for n, i in enumerate(iterable, start=1):
        yield (pause, i)
        if n >= n_jobs:
            pause = 0
        else:
            pause += delay


def choice_simulator_trips_many(
        dh,
        otaz=None,
        max_chunk_size=20,
        n_jobs=5,
        thread_saturation=1,
        cache_subdir="choice_simulator_trips",
        temp_dir=None,
        with_nonhome_auto=False,
        disagg_choices=True,
        with_wfh=False,
):
    if otaz is None:
        otaz = np.arange(dh.n_internal_zones) + 1

    # auto chunk size calculation
    n_chunks_per_job = 0
    chunk_size = np.inf
    while chunk_size > max_chunk_size:
        n_chunks_per_job += 1
        chunk_size = int(np.ceil(len(otaz) / n_jobs / n_chunks_per_job))
        if chunk_size == 1:
            break

    log.debug(f"choice_simulator_trips_many: chunk_size={chunk_size}")

    otaz_chunks = [otaz[i:i + chunk_size] for i in range(0, len(otaz), chunk_size)]
    # inits = [None for _ in range(0, min(len(otaz), n_jobs))]

    import joblib
    save_dir = dh.filenames.cache_dir / cache_subdir
    os.makedirs(save_dir, exist_ok=True)
    n_threads = max(int(thread_saturation * joblib.cpu_count() // n_jobs), 1)
    log.debug(f"choice_simulator_trips_many: n_threads={n_threads} (per job)")

    # if temp_dir is None:
    #     temp_dir = save_dir/"temp"
    if temp_dir is not None:
        os.makedirs(temp_dir, exist_ok=True)

    if with_nonhome_auto:
        first_purposes = tuple(j for j in purposesA if 'NHB' not in j)
        second_purposes = tuple(j for j in purposesA if j not in first_purposes)
    else:
        first_purposes = purposesA
        second_purposes = ()

    with joblib.Parallel(n_jobs=n_jobs, verbose=100) as parallel:
        # IMPORTANT: Do not run the initialize step as show below
        #            the memory usage of the worker will be much smaller
        #            than for a real step, and loky will think there is
        #            a memory leak and kill the worker.
        # if init_step:
        # 	log.info("joblib model init starting")
        # 	_ = parallel(
        # 		joblib.delayed(choice_simulator_initialize)(dh, False)
        # 		for _ in inits
        # 	)
        # 	log.info("joblib model init complete")
        # else:
        log.info("joblib model body starting")
        parallel(
            joblib.delayed(choice_simulator_trips)(
                dh,
                otaz_chunk,
                purposes=first_purposes,
                save_dir=save_dir,
                n_threads=n_threads,
                delay=delay,
                temp_dir=temp_dir,
                disagg_choices=disagg_choices,
            )
            for delay, otaz_chunk in stagger_starts(otaz_chunks, delay=3, n_jobs=n_jobs)
        )
        if with_wfh:
            parallel(
                joblib.delayed(choice_simulator_trips)(
                    dh,
                    otaz_chunk,
                    purposes=first_purposes,
                    save_dir=save_dir,
                    n_threads=n_threads,
                    delay=delay,
                    temp_dir=temp_dir,
                    disagg_choices=disagg_choices,
                    use_wfh_pa=True,
                )
                for delay, otaz_chunk in stagger_starts(otaz_chunks, delay=3, n_jobs=n_jobs)
            )
        log.info("joblib first purposes complete")

    log.info("computing auto propensity by zone")
    trips = assemble_trips(
        dh,
        from_dir=save_dir,
        pattern="choice_simulator_trips_*.pq",
        compute_auto_propensity=True,
    )
    if second_purposes:
        with joblib.Parallel(n_jobs=n_jobs, verbose=100) as parallel:
            parallel(
                joblib.delayed(choice_simulator_trips)(
                    dh,
                    otaz_chunk,
                    purposes=second_purposes,
                    save_dir=save_dir,
                    n_threads=n_threads,
                    delay=delay,
                    temp_dir=temp_dir,
                    disagg_choices=disagg_choices,
                )
                for delay, otaz_chunk in stagger_starts(otaz_chunks, delay=3, n_jobs=n_jobs)
            )
            if with_wfh:
                parallel(
                    joblib.delayed(choice_simulator_trips)(
                        dh,
                        otaz_chunk,
                        purposes=second_purposes,
                        save_dir=save_dir,
                        n_threads=n_threads,
                        delay=delay,
                        temp_dir=temp_dir,
                        disagg_choices=disagg_choices,
                        use_wfh_pa=True,
                    )
                    for delay, otaz_chunk in stagger_starts(otaz_chunks, delay=3, n_jobs=n_jobs)
                )

            log.info("joblib second purposes complete")
        trips = assemble_trips(
            dh,
            from_dir=save_dir,
            pattern="choice_simulator_trips_*.pq",
            compute_auto_propensity=False,
        )

    log.info("computing deadhead trips by hired cars")
    try:
        deadheads = compute_deadhead_trip_table(
            dh,
            trips,
        )
    except KeyError:
        import numexpr as ne
        ne.necompiler._numexpr_cache.clear()
        deadheads = compute_deadhead_trip_table(
            dh,
            trips,
        )
    if disagg_choices:
        deadheads['hh_autos'] = -1
        deadheads['hh_inc3'] = -1
    deadheads.to_parquet(os.path.join(save_dir, "choice_simulator_trips_deadhead.pq"))

    return assemble_trips(
        dh,
        from_dir=save_dir,
        pattern="choice_simulator_trips_*.pq",
        compute_auto_propensity=False,
    )



def assemble_trips(
        dh,
        from_dir,
        pattern="choice_simulator_trips_*.pq",
        compute_auto_propensity=False,
        validation_dump=None,
):
    from_dir = Path(from_dir)

    import dask.dataframe as ddf
    import glob

    trips = ddf.read_parquet(glob.glob(os.fspath(from_dir/pattern)))

    if compute_auto_propensity:
        homebased_trips_by_mode = (
            trips
                .query("purpose not in ('NHB', 'NHBR', 'NHBS')")
                .groupby(["mode", "a_zone"])['trips']
                .sum()
                .compute()
                .unstack(0)
                .fillna(0)
        )
        auto_propensity = (
            homebased_trips_by_mode.loc[:, [1, 2, 3]].sum(1) / homebased_trips_by_mode.sum(1)
        )
        auto_propensity = auto_propensity.reindex(pd.RangeIndex(1, 3632 + 1)).fillna(0.95).rename("auto_propensity")
        auto_propensity.index.name = "TAZ"
        auto_propensity.to_csv(dh.filenames.emme_database_dir / "computed_auto_propensity.csv")

    if validation_dump is not None:
        trips.to_csv(
            os.fspath(from_dir/validation_dump),
            single_file=True,
            compression='gzip' if validation_dump.endswith(".gz") else None,
        )

    return trips
