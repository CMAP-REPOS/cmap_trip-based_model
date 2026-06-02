import os
import pandas as pd

def load_household_types():

    hhv_cats = {
        'N_CHILDREN': ['0', '1', '2', '3+'],
        'N_WORKERS': ['0', '1', '2', '3+'],
        'N_ADULTS': ['1', '2', '3', '4+'],
        'N_VEHICLES': ['0', '1', '2', '3+'],
        'HOUSEHOLDER_AGE': ['<35', '35-64', '>65'],
    }

    hhv_types = pd.read_csv(
        os.path.join(os.path.dirname(__file__), "household_types.csv"),
        index_col=0,
    )

    for k, v in hhv_cats.items():
        hhv_types[k] = hhv_types[k].astype(pd.CategoricalDtype(v, ordered=True))

    return hhv_types

