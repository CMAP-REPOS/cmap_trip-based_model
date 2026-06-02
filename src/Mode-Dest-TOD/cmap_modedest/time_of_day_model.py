import os
from larch.numba import Model, P, X
import yaml
from .addict import Dict

from .cmap_logging import getLogger, get_worker_log
from .purposes import purposes5

time_period_names = ['EA', 'AM1', 'AM2', 'AM3', 'MD', 'PM1', 'PM2', 'PM3']
time_period_codes = [  1,    2,     3,     4,     5,    6,     7,     8  ]

def tod_model_builder(
        purpose,
        data_co=None,
        ch_name=None,
        model_parameters=None,
):
    from larch.numba import DataFrames
    if model_parameters is None:
        model_parameters = {}

    d = DataFrames(
        co=data_co,
        ch=ch_name,
        alt_codes=time_period_codes,
        alt_names=time_period_names,
        av=1,
    )

    tod_model = Model(d)
    tod_model.title = f"{purpose} Time of Day Choice"

    for tname, tcode in zip(time_period_names, time_period_codes):
        tod_model.utility_co[tcode] = (
                + P(f"ASC_{tname}")
                + P.time * X(f"mf46{tcode}")
                + P(f"hiredcar_{tname}") * X("mode9 in ('TAXI','TNC1','TNC2')")
        )
        if purpose[:2].upper() == "HB":
            if purpose.upper() == "HBS":
                tod_model.utility_co[tcode] += (
                        + P(f"ASC_{tname}_r") * X("paFlip")
                        # no reverse-trip hired car parameters, insufficient survey data
                )
            else:
                tod_model.utility_co[tcode] += (
                        + P(f"ASC_{tname}_r") * X("paFlip")
                        + P(f"hiredcar_{tname}_r") * X("mode9 in ('TAXI','TNC1','TNC2')") * X("paFlip")
                )

    tod_model.lock_value('ASC_AM2', 0.0)
    tod_model.lock_value('hiredcar_AM2', 0.0)
    if purpose.upper() == "HBS":
        tod_model.lock_value('ASC_AM2_r', 0.0)
    elif purpose[:2].upper() == "HB":
        tod_model.lock_value('ASC_AM2_r', 0.0)
        tod_model.lock_value('hiredcar_AM2_r', 0.0)
    tod_model.set_cap()

    if purpose == 'NHB':
        tod_model.set_value('time', -0.02, maximum=-0.02)

    for k,v in model_parameters.items():
        tod_model.set_value(k, v)

    return tod_model




time_of_day_simulator_global = Dict()


def time_of_day_simulator_initialize(dh, cache=True):
    global time_of_day_simulator_global

    log = get_worker_log(
        os.path.join(dh.filenames.cache_dir, 'logs'),
        level=10,
    )

    log.debug(f"time_of_day_simulator_initialize(cache={cache}")
    n_zones = dh.n_internal_zones
    choice_model_params = dh.choice_model_params

    if len(choice_model_params) == 0:
        raise ValueError("no choice_model_params")

    pickle_name = dh.filenames.cache_dir / f"tod_models_{n_zones}.pkl"
    tod_param_file = dh.filenames.tod_model_param_file
    if tod_param_file and os.path.exists(tod_param_file):
        with open(tod_param_file, 'r') as f:
            tod_model_params = Dict(yaml.load(f, Loader=yaml.SafeLoader)) # TODO addicty
    else:
        tod_model_params = Dict()

    if len(time_of_day_simulator_global) == 0:
        log.debug("time_of_day_simulator_initialize: preloaded tod_simulator not available")
        if os.path.exists(pickle_name):
            import cloudpickle
            with open(pickle_name, 'rb') as pkl_f:
                log.debug("time_of_day_simulator_initialize: loading pickled tod_simulator")
                time_of_day_simulator_global = cloudpickle.load(pkl_f)
            cache = False
        else:
            log.debug("time_of_day_simulator_initialize: pickled tod_simulator not available")

    if time_of_day_simulator_global:
        log.info("time_of_day_simulator_initialize: using existing tod_simulator")
        tod_simulator = time_of_day_simulator_global
    else:
        log.info("time_of_day_simulator_initialize: creating fresh tod_simulator")
        tod_simulator = Dict()
        for purpose in purposes5:
            tod_simulator[purpose] = tod_model_builder(
                purpose=purpose,
                model_parameters=tod_model_params.get(purpose, {}),
            )
        time_of_day_simulator_global = tod_simulator

    if cache and not os.path.exists(pickle_name):
        import cloudpickle
        with open(pickle_name, 'wb') as pkl_f:
            log.debug("time_of_day_simulator_initialize: pickling tod_simulator for future reload")
            cloudpickle.dump(
                time_of_day_simulator_global,
                pkl_f,
            )

    return tod_simulator
