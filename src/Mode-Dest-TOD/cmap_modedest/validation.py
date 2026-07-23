import pandas as pd
import numpy as np
from pathlib import Path
import dask.dataframe as dd
from .modecodes import mode9codes, mode9names


def attach_superdistricts_to_trip_table(dh, trip_table):
    if isinstance(trip_table, dd.DataFrame):
        trip_table = trip_table.compute()
    if 'o_zone' not in trip_table.columns:
        trip_table = trip_table.reset_index()
    if 'o_district' not in trip_table:
        zone_info = pd.read_csv(
            dh.filenames.zone_districts,
            index_col=0,
        )
        trip_table['o_district'] = trip_table['o_zone'].map(zone_info.district)
        trip_table['d_district'] = trip_table['d_zone'].map(zone_info.district)
    if 'o_superdistrict' not in trip_table:
        super_districts = dh.cfg.super_districts
        trip_table['o_superdistrict'] = trip_table['o_district'].map(super_districts)
        trip_table['d_superdistrict'] = trip_table['d_district'].map(super_districts)
    return trip_table

def superdistrict_flow_summary(dh, trip_table, dimensions):
    trip_table = attach_superdistricts_to_trip_table(dh, trip_table)

    summary_table = trip_table.groupby([
        *dimensions,
        "o_superdistrict",
        "d_superdistrict",
    ])['trips'].sum().unstack(-1).fillna(0)

    z = slice(None)
    summary_output = []

    for k in dh.cfg.super_district_flow_summaries:
        v1, v2 = k.split("-")
        temp = (
            summary_table
            .loc[(*tuple([z]*len(dimensions)), list(v1)), list(v2)]
            .sum(1)
            .groupby(summary_table.index.names[:-1])
            .sum()
            .rename(k)
        )
        if v1 != v2:
            temp = temp.add(
                summary_table
                .loc[(*tuple([z]*len(dimensions)), list(v2)), list(v1)]
                .sum(1)
                .groupby(summary_table.index.names[:-1])
                .sum(),
                fill_value=0,
            ).rename(k)
        summary_output.append(temp)

    return pd.concat(summary_output, axis=1).fillna(0)


def to_excel(df, *args, **kwargs):
    if isinstance(df, dd.DataFrame):
        df = df.compute()
    return df.to_excel(*args, **kwargs)

def validation_aggregation(dh, trips, to_dir=None):

    if to_dir is None:
        to_dir = dh.filenames.cache_dir
    to_dir = Path(to_dir)

    if isinstance(trips, dd.DataFrame):
        trips = trips.compute()

    trips = attach_superdistricts_to_trip_table(dh, trips)
    internal_trips = trips.query("o_superdistrict != 'X'").query("d_superdistrict != 'X'")

    agg1 = internal_trips.groupby(['purpose', 'mode', 'hh_autos'])[['trips']].sum().unstack(-1)
    agg2 = internal_trips.groupby(['purpose', 'mode', 'hh_inc5'])[['trips']].sum().unstack(-1)
    agg3 = internal_trips.groupby(['purpose', 'mode', 'timeperiod'])[['trips']].sum().unstack(-1)
    agg4 = superdistrict_flow_summary(dh, trips, ['purpose', 'mode']).T
    agg4.index.name = 'segments'
    agg5 = agg4.unstack().reset_index().pivot(index=['purpose', 'segments'], columns='mode')
    agg5.columns = pd.MultiIndex.from_arrays([
        agg5.columns.get_level_values(1),
        mode9names,
    ], names=['modecode', 'modename'])
    agg5pct = agg5.div(agg5.sum(1), axis=0).style.format("{:.2%}")

    with pd.ExcelWriter(to_dir / 'validation_data.xlsx', engine='xlsxwriter') as writer:
        to_excel(agg1, writer, sheet_name='Autos', merge_cells=False)
        to_excel(agg2, writer, sheet_name='Incomes', merge_cells=False)
        to_excel(agg3, writer, sheet_name='TimePeriod', merge_cells=False)
        to_excel(agg4, writer, sheet_name='FlowsByPurp&Mode', merge_cells=False)
        to_excel(agg5pct, writer, sheet_name='FlowsByPurp&Mode%', merge_cells=False)
