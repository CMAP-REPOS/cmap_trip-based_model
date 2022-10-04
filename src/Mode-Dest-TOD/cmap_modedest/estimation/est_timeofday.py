import numpy as np
import pandas as pd
from pathlib import Path
from larch.numba import P, X, Model, OMX, DataFrames
# from larch import P, X, Model, OMX, DataFrames
from IPython.display import display, HTML
from larch.util.data_expansion import piecewise_linear

from ..time_of_day_model import tod_model_builder

def estimate_tod(dh, mods, to_pickle=True):
    cached_todmodel_filename = lambda purpose: dh.filenames.cache_dir / f"choicemodel_{purpose}_timeofday.xlsx"

    tod_models = {}
    tod_skims = dh.skims.raw[[f'mf46{j}' for j in range(1,9)]]

    for purpose, mode_dest_model in mods.items():
        co = mode_dest_model.dataservice.data_co.query("mode9 in ('AUTO','HOV2','HOV3','TAXI','TNC1','TNC2')").copy()
        co['timeperiod1'] = co['timeperiod'] + 1
        co = co.join(
            tod_skims.iat_df(
                pd.DataFrame({
                    'otaz': co['o_zone'] - 1,
                    'dtaz': co['d_zone'] - 1,
                })
            )
        )
        tod_model = tod_model_builder(purpose, data_co=co, ch_name='timeperiod1')
        tod_model.estimate()
        display(HTML(f"<h3>{tod_model.title}</h3>"))
        display(tod_model.parameter_summary())
        display(tod_model.estimation_statistics())

        xl = tod_model.to_xlsx(
            cached_todmodel_filename(purpose),
            save_now=True
        )
        tod_models[purpose] = tod_model

    if to_pickle:
        if to_pickle is True:
            to_pickle = dh.filenames.cache_dir / "tod_models.pkl"
        import cloudpickle
        with open(to_pickle, "wb") as f:
            cloudpickle.dump(tod_models, f)

    # save cached parameters
    param_yaml_file = dh.filenames.cache_dir / "tod_model_parameters.yaml"
    with open(param_yaml_file, 'w', encoding="utf-8") as cmp_yaml:
        print("---", file=cmp_yaml)
        for purpose in tod_models.keys():
            print(f"{purpose}:", file=cmp_yaml)
            for k, v in tod_models[purpose].pf.value.items():
                if ':' in k:
                    k = f'"{k}"'
                print(f'  {k:24s}: {v}', file=cmp_yaml)
            print("", file=cmp_yaml)
        print("...", file=cmp_yaml)

    return tod_models