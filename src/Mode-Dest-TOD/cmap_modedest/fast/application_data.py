import larch
import numpy as np
from numba import njit, vectorize
import pandas as pd
import pyarrow as pa
from sharrow import Dataset, SharedData, Table, local_cache

from .tnc_los import tnc_los_functions, taxi_cost, tnc_cost
from ..cmap_logging import getLogger, getSubLogger
from ..purposes import purposesA
log = getLogger()

def application_data(dh):
    log = getSubLogger("APPDATA")

    log.info("preparing application_data")

    #taxi_cost, tnc_solo_cost, tnc_pool_cost = \
    tnc_los_functions(dh)

    otazi, dtazi = np.random.choice(3000, 60000), np.random.choice(3000, 60000)

    tbl = Table({
        'otaz_idx': otazi,
        'dtaz_idx': dtazi,
        'otaz': otazi + 1,
        'dtaz': dtazi + 1,
        'chooser_row': np.arange(otazi.size)
    })

    ozones = Dataset.from_dataframe(
        dh.m01.set_index(
            dh.m01.index.rename("otaz"),
        ),
    ).squash_index(otaz='otaz_idx')

    attractions = Dataset.from_dataframe(
        dh.trip_attractions5.astype(np.float64).set_index(
            dh.trip_attractions5.index.rename("dtaz"),
        ),
    ).squash_index(dtaz='dtaz_idx')

    omx_auto = dh.skims.raw.select_and_rename(
        {v:k for k,v in dh.skims.auto.col_mapping.items()}
    ).squash_index(otaz='otaz_idx', dtaz='dtaz_idx')

    # inject intrazonal distance into auto skims
    # set intrazonal auto_dist
    intrazonal_zone_area = dh.zone_shp.area  # square feet
    intrazonal_dist = np.sqrt(intrazonal_zone_area) / 5280 * 0.667
    np.fill_diagonal(
        omx_auto['am_dist'].values,
        intrazonal_dist,
    )
    np.fill_diagonal(
        omx_auto['md_dist'].values,
        intrazonal_dist,
    )

    omx_transit_pk = dh.skims.raw.select_and_rename(
        {v:k for k,v in dh.skims.transit_pk.col_mapping.items()}
    ).squash_index(otaz='otaz_idx', dtaz='dtaz_idx')

    omx_transit_op = dh.skims.raw.select_and_rename(
        {v:k for k,v in dh.skims.transit_op.col_mapping.items()}
    ).squash_index(otaz='otaz_idx', dtaz='dtaz_idx')

    # load lastest computed autopropensity as a pd.DataFrame
    from ..data_handlers.m01_handler import attach_areatypes
    autopropensity = attach_areatypes(
        dh,
        pd.DataFrame(index=dh.m01.index),
        "",
        "",
        targetzone=dh.m01.index,
    )[['autopropensity']]
    o_autopropensity = Dataset.from_dataframe(
        autopropensity.set_index(
            autopropensity.index.rename("otaz"),
        )
    ).squash_index(otaz='otaz_idx')
    d_autopropensity = Dataset.from_dataframe(
        autopropensity.set_index(
            autopropensity.index.rename("dtaz"),
        )
    ).squash_index(dtaz='dtaz_idx')


    tg = SharedData(
        tbl,
        **{
            'transit_pk': omx_transit_pk,
            'transit_op': omx_transit_op,
            'auto_skims': omx_auto,
            'ozone': ozones,
            'dzone': ozones.set_match_names({"otaz_idx":'dtaz_idx'}),
            'attractions': attractions,
            'o_autopropensity': o_autopropensity,
            'd_autopropensity': d_autopropensity,
        },
        extra_funcs=(
            taxi_cost, tnc_cost,
        ),
        extra_vars=dict(
            taxi_cost_struct=dh.cfg['taxi_cost_struct'],
            tnc_solo_peak_struct=dh.cfg['tnc_cost_struct_solo_peak'],
            tnc_solo_offpeak_struct=dh.cfg['tnc_cost_struct_solo_peak'],
            tnc_pool_peak_struct=dh.cfg['tnc_cost_struct_pool_peak'],
            tnc_pool_offpeak_struct=dh.cfg['tnc_cost_struct_pool_peak'],
        ),
    )

    log.info("setup application data flow")
    ss = tg.setup_flow({

        'o_zone == dtaz': 'otaz == dtaz', #### fails dtype

        'transit_ivtt_PEAK'   : 'transit_pk.ivtt',
        'transit_ivtt_OFFPEAK': 'transit_op.ivtt',
        'transit_ovtt_PEAK'   : 'transit_pk.ovtt',
        'transit_ovtt_OFFPEAK': 'transit_op.ovtt',
        'transit_fare_PEAK'   : 'transit_pk.fare',
        'transit_fare_OFFPEAK': 'transit_op.fare',

        'piece(transit_ivtt_OFFPEAK, None, 20)': 'piece(transit_ivtt_OFFPEAK, None, 20)',
        'piece(transit_ivtt_OFFPEAK, 20, None)': 'piece(transit_ivtt_OFFPEAK, 20, None)',
        'autopropensity': 'd_autopropensity.autopropensity',

        'auto_time_PEAK': 'auto_skims.am_time',
        'auto_dist_PEAK': 'auto_skims.am_dist',
        'auto_time_OFFPEAK': 'auto_skims.md_time',
        'auto_dist_OFFPEAK': 'auto_skims.md_dist',
        'auto_opcost_PEAK': 'auto_skims.am_opcost',
        'auto_opcost_hov_PEAK': 'auto_skims.am_opcost_hov',
        'auto_opcost_OFFPEAK': 'auto_skims.md_opcost',
        'auto_toll_hiinc_PEAK': 'np.fmax(auto_skims.am_toll_hiinc, 0)',
        'auto_toll_loinc_PEAK': 'np.fmax(auto_skims.am_toll_loinc, 0)',
        'auto_toll_hov_hiinc_PEAK': 'np.fmax(auto_skims.am_toll_hov_hiinc, 0)',
        'auto_toll_hov_loinc_PEAK': 'np.fmax(auto_skims.am_toll_hov_loinc, 0)',
        'auto_toll_OFFPEAK': 'np.fmax(auto_skims.md_toll, 0)',

        'piece(auto_dist_PEAK,None,0.5)': 'piece(auto_skims.am_dist,None,0.5)',
        'piece(auto_dist_PEAK,0.5,1.0)' : 'piece(auto_skims.am_dist,0.5,1.0)',
        'piece(auto_dist_PEAK,1.0,None)': 'piece(auto_skims.am_dist,1.0,None)',
        'piece(auto_dist_PEAK,None,5)'  : 'piece(auto_skims.am_dist,None,5)',
        'piece(auto_dist_PEAK,5,10)'    : 'piece(auto_skims.am_dist,5,10)',
        'piece(auto_dist_PEAK,10,None)' : 'piece(auto_skims.am_dist,10,None)',

        'piece(auto_dist_OFFPEAK,None,0.5)': 'piece(auto_skims.md_dist,None,0.5)',
        'piece(auto_dist_OFFPEAK,0.5,1.0)' : 'piece(auto_skims.md_dist,0.5,1.0)',
        'piece(auto_dist_OFFPEAK,1.0,None)': 'piece(auto_skims.md_dist,1.0,None)',
        'piece(auto_dist_OFFPEAK,None,5)'  : 'piece(auto_skims.md_dist,None,5)',
        'piece(auto_dist_OFFPEAK,5,10)'    : 'piece(auto_skims.md_dist,5,10)',
        'piece(auto_dist_OFFPEAK,10,None)' : 'piece(auto_skims.md_dist,10,None)',

        'taxi_fare_PEAK': 'taxi_cost(auto_skims.am_time, auto_skims.am_dist, otaz, dtaz, taxi_cost_struct)',
        'taxi_fare_OFFPEAK': 'taxi_cost(auto_skims.md_time, auto_skims.md_dist, otaz, dtaz, taxi_cost_struct)',
        'tnc_solo_fare_PEAK': 'tnc_cost(auto_skims.am_time, auto_skims.am_dist, otaz, dtaz, tnc_solo_peak_struct)',
        'tnc_solo_fare_OFFPEAK': 'tnc_cost(auto_skims.md_time, auto_skims.md_dist, otaz, dtaz, tnc_solo_offpeak_struct)',
        'tnc_pool_fare_PEAK': 'tnc_cost(auto_skims.am_time, auto_skims.am_dist, otaz, dtaz, tnc_pool_peak_struct)',
        'tnc_pool_fare_OFFPEAK': 'tnc_cost(auto_skims.md_time, auto_skims.md_dist, otaz, dtaz, tnc_pool_offpeak_struct)',

        'taxi_wait_time_PEAK'       : 'ozone.taxi_wait_pk',
        'taxi_wait_time_OFFPEAK'    : 'ozone.taxi_wait_op',
        'tnc_solo_wait_time_PEAK'   : 'ozone.tnc_solo_wait_pk',
        'tnc_solo_wait_time_OFFPEAK': 'ozone.tnc_solo_wait_op',
        'tnc_pool_wait_time_PEAK'   : 'ozone.tnc_pool_wait_pk',
        'tnc_pool_wait_time_OFFPEAK': 'ozone.tnc_pool_wait_op',

        'taxi_wait_time_PEAK/auto_dist_PEAK'          : 'ozone.taxi_wait_pk/auto_skims.am_dist',
        'taxi_wait_time_OFFPEAK/auto_dist_OFFPEAK'    : 'ozone.taxi_wait_op/auto_skims.md_dist',
        'tnc_solo_wait_time_PEAK/auto_dist_PEAK'      : 'ozone.tnc_solo_wait_pk/auto_skims.am_dist',
        'tnc_solo_wait_time_OFFPEAK/auto_dist_OFFPEAK': 'ozone.tnc_solo_wait_op/auto_skims.md_dist',
        'tnc_pool_wait_time_PEAK/auto_dist_PEAK'      : 'ozone.tnc_pool_wait_pk/auto_skims.am_dist',
        'tnc_pool_wait_time_OFFPEAK/auto_dist_OFFPEAK': 'ozone.tnc_pool_wait_op/auto_skims.md_dist',

        'fmax(ozone_areatype, areatype)==1': 'max(ozone.zone_type, dzone.zone_type)==1',
        'fmax(ozone_areatype, areatype)==2': 'max(ozone.zone_type, dzone.zone_type)==2',
        'fmax(ozone_areatype, areatype)==3': 'max(ozone.zone_type, dzone.zone_type)==3',
        'fmax(ozone_areatype, areatype)==4': 'max(ozone.zone_type, dzone.zone_type)==4',

        'fmin(ozone_areatype, areatype)==1': 'min(ozone.zone_type, dzone.zone_type)==1',
        'fmin(ozone_areatype, areatype)==2': 'min(ozone.zone_type, dzone.zone_type)==2',
        'fmin(ozone_areatype, areatype)==3': 'min(ozone.zone_type, dzone.zone_type)==3',
        'fmin(ozone_areatype, areatype)==4': 'min(ozone.zone_type, dzone.zone_type)==4',

        'log_attractions_HBWH': 'log(attractions.HBWH)',
        'log_attractions_HBWL': 'log(attractions.HBWL)',
        'log_attractions_HBS': 'log(attractions.HBS)',
        'log_attractions_HBO': 'log(attractions.HBO)',
        'log_attractions_NHB': 'log(attractions.NHB)',
        # 'log_attractions_HBOR': 'log(attractions.HBOR)',
        # 'log_attractions_NHBR': 'log(attractions.NHBR)',
        # 'log_attractions_NHBS': 'log(attractions.NHBS)',

        'log_attractions_HBWH > -666': 'log(attractions.HBWH) > -666',
        'log_attractions_HBWL > -666': 'log(attractions.HBWL) > -666',
        'log_attractions_HBS > -666': 'log(attractions.HBS) > -666',
        'log_attractions_HBO > -666': 'log(attractions.HBO) > -666',
        'log_attractions_NHB > -666': 'log(attractions.NHB) > -666',
        # 'log_attractions_HBOR > -666': 'log(attractions.HBOR) > -666',
        # 'log_attractions_NHBR > -666': 'log(attractions.NHBR) > -666',
        # 'log_attractions_NHBS > -666': 'log(attractions.NHBS) > -666',

    }, dtype="float64", cache_dir=local_cache(), name="cmap_trip_application_data_step_1")

    try:
        log.debug("     application_data::load first run")
        df = ss.load(tbl, as_dataframe=True, dtype=np.float64)
        log.debug("     application_data::load second run")
        df2 = ss.load(tbl, as_dataframe=True, dtype=np.float64)
        log.debug("     application_data::load check equal")
        try:
            pd.testing.assert_frame_equal(df, df2)
        except AssertionError as err:
            log.exception(err)
        from ..cmap_logging import log_df
        log_df(df2, logger=log, verbose=1, level=10)

        return ss
    finally:
        log.info("application_data flow ready")




def application_data2(dh, in_table):
    log = getSubLogger("APPDATA2")
    log.info("prepare application_data2")

    coldefs = {

        'transit_approach_drivetime_PEAK/auto_dist_PEAK': f'transit_approach_drivetime_PEAK/auto_dist_PEAK',
        'transit_approach_waittime_PEAK/auto_dist_PEAK': f'transit_approach_waittime_PEAK/auto_dist_PEAK',
        'transit_approach_walktime_PEAK/auto_dist_PEAK': f'transit_approach_walktime_PEAK/auto_dist_PEAK',
        'transit_ovtt_PEAK/auto_dist_PEAK': f'transit_ovtt_PEAK/auto_dist_PEAK',

        'transit_approach_drivetime_OFFPEAK/auto_dist_OFFPEAK': f'transit_approach_drivetime_OFFPEAK/auto_dist_OFFPEAK',
        'transit_approach_waittime_OFFPEAK/auto_dist_OFFPEAK': f'transit_approach_waittime_OFFPEAK/auto_dist_OFFPEAK',
        'transit_approach_walktime_OFFPEAK/auto_dist_OFFPEAK': f'transit_approach_walktime_OFFPEAK/auto_dist_OFFPEAK',
        'transit_ovtt_OFFPEAK/auto_dist_OFFPEAK': f'transit_ovtt_OFFPEAK/auto_dist_OFFPEAK',

        'hard_sigmoid(transit_approach_walktime_PEAK, 4.0, 2.0)': f'hard_sigmoid(transit_approach_walktime_PEAK, 4.0, 2.0)',
        'hard_sigmoid(transit_approach_walktime_OFFPEAK, 4.0, 2.0)': f'hard_sigmoid(transit_approach_walktime_OFFPEAK, 4.0, 2.0)',

        # # TODO fix auto parking cost
        # # _parking_cost, _free_parking = parking_cost_v2(

        'auto_parking_cost_HBWH': '0.0',
        'auto_parking_cost_HBWL': '0.0',
        'auto_parking_cost_HBO': '0.0',
        'auto_parking_cost_HBS': '0.0',
        'auto_parking_cost_NHB': '0.0',
        'auto_parking_cost_HBOR': '0.0',
        'auto_parking_cost_NHBR': '0.0',
        'auto_parking_cost_NHBS': '0.0',

        'samp_wgt': '1.0',
        'log(1/samp_wgt)': '0.0',

        'transit_avail_HBWH': (
            "(transit_ivtt_PEAK < 999)"
            "& (transit_approach_walktime_PEAK < 999)"
            "& (transit_approach_drivetime_PEAK < 999)"
            "& (log_attractions_HBWH > -9998)"
        ),
        'transit_avail_HBWL': (
            "(transit_ivtt_PEAK < 999)"
            "& (transit_approach_walktime_PEAK < 999)"
            "& (transit_approach_drivetime_PEAK < 999)"
            "& (log_attractions_HBWL > -9998)"
        ),
        'transit_avail_HBS': (
            "(transit_ivtt_OFFPEAK < 999)"
            "& (transit_approach_walktime_OFFPEAK < 999)"
            "& (transit_approach_drivetime_OFFPEAK < 999)"
            "& (log_attractions_HBS > -9998)"
        ),
        'transit_avail_HBO': (
            "(transit_ivtt_OFFPEAK < 999)"
            "& (transit_approach_walktime_OFFPEAK < 999)"
            "& (transit_approach_drivetime_OFFPEAK < 999)"
            "& (log_attractions_HBO > -9998)"
        ),
        'transit_avail_NHB': (
            "(transit_ivtt_OFFPEAK < 999)"
            "& (transit_approach_walktime_OFFPEAK < 999)"
            "& (transit_approach_drivetime_OFFPEAK < 999)"
            "& (log_attractions_NHB > -9998)"
        ),
    }

    for purpose in purposesA:
        coldefs.update({
            f'auto_avail_{purpose}': f'log_attractions_{purpose} > -9998',
            f'1-auto_avail_{purpose}': f'1-auto_avail_{purpose}',
            f'1-transit_avail_{purpose}': f'1-transit_avail_{purpose}',
        })

    log.info("setup application data step 2 flow")
    processor = SharedData(in_table).setup_flow(
        coldefs,
        cache_dir=local_cache(),
        name="cmap_trip_application_data_step_2",
        dtype="float64",
    )

    try:
        log.debug("     application_data2::load first hit")
        df = processor.load(in_table, as_dataframe=True)
        log.debug("     application_data2::load second hit")
        df2 = processor.load(in_table, as_dataframe=True)
        log.debug("     application_data2::check equal")
        try:
            pd.testing.assert_frame_equal(df, df2)
        except AssertionError as err:
            log.exception(err)
        return processor
    finally:
        log.info("application_data2 flow ready")

