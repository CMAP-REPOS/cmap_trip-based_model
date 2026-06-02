import numba as nb
import numpy as np
import pandas as pd
import yaml
from ..addict import Dict
from collections import namedtuple
from typing import NamedTuple

zone_to_cents_t = nb.types.DictType(nb.int32, nb.float32)


class TaxiCostStruct(NamedTuple):
    flag_pull: float
    per_minute: float
    per_mile: float
    airport_pickup_fee: float
    airport_pickup_zones: np.ndarray


class TncCostStruct(NamedTuple):
    base_fare: float
    per_minute: float
    per_mile: float
    min_fare: float
    booking_fee: float
    special_fee: float
    downtown_fee: float
    special_zones: np.ndarray
    downtown_zones: np.ndarray


@nb.njit(cache=True)
def taxi_cost(auto_time, auto_dist, o_zone, d_zone, taxi_cost_struct):
    """
    Compute taxi fare.

    A single set of rates (Chicago medallion rates for in-city trips)
    is used; fares for taxi trips outside Chicago are close to this
    rate and rare enough that more precision is unneeded.

    Parameters
    ----------
    auto_time, auto_dist : array-like

    Returns
    -------
    fare : array-like
    """
    cost = (
            taxi_cost_struct.flag_pull
            + auto_time * taxi_cost_struct.per_minute
            + auto_dist * taxi_cost_struct.per_mile
    )
    if taxi_cost_struct.airport_pickup_fee:
        bucket_applies = False
        for i in range(taxi_cost_struct.airport_pickup_zones.size):
            if taxi_cost_struct.airport_pickup_zones[i] == o_zone:
                bucket_applies = True
                break
        if bucket_applies:
            cost += taxi_cost_struct.airport_pickup_fee

    return cost

@nb.njit(cache=True)
def tnc_cost(auto_time, auto_dist, o_zone, d_zone, tnc_cost_struct):
    """
    Compute the TNC cost.

    Parameters
    ----------
    auto_time, auto_dist : float
        The auto travel time and distance for a set of trips.
    o_zone, d_zone : int
        Zone numbers for origin and destination
    tnc_cost_struct : TncCostStruct
        Use the correct struct for peak/offpeak and solo/shared.

    Returns
    -------
    float
    """
    cost = (
            tnc_cost_struct.per_minute * auto_time
            + tnc_cost_struct.per_mile * auto_dist
            + tnc_cost_struct.base_fare
    )
    cost = max(cost, tnc_cost_struct.min_fare) + tnc_cost_struct.booking_fee

    if tnc_cost_struct.special_fee:
        bucket_applies = False
        for i in range(tnc_cost_struct.special_zones.size):
            if tnc_cost_struct.special_zones[i] == o_zone or tnc_cost_struct.special_zones[i] == d_zone:
                bucket_applies = True
                break
        if bucket_applies:
            cost += tnc_cost_struct.special_fee

    if tnc_cost_struct.downtown_fee:
        bucket_applies = False
        for i in range(tnc_cost_struct.downtown_zones.size):
            if tnc_cost_struct.downtown_zones[i] == o_zone or tnc_cost_struct.downtown_zones[i] == d_zone:
                bucket_applies = True
                break
        if bucket_applies:
            cost += tnc_cost_struct.downtown_fee

    return cost


### CONFIG ###

def tnc_los_functions(dh):

    with open(dh.filenames.config, 'rt') as yf:
        cfg = Dict(yaml.safe_load(yf))
    cfg.freeze()

    taxi_cost_flag_pull  = cfg.taxi.cost.flag_pull
    taxi_cost_per_minute = cfg.taxi.cost.per_minute
    taxi_cost_per_mile   = cfg.taxi.cost.per_mile

    tnc_cost_peak_per_minute    = cfg.tnc.cost.peak.per_minute
    tnc_cost_peak_per_mile      = cfg.tnc.cost.peak.per_mile
    tnc_cost_peak_base_fare     = cfg.tnc.cost.peak.base_fare
    tnc_cost_peak_min_fare      = cfg.tnc.cost.peak.min_fare
    tnc_cost_peak_booking_fee   = cfg.tnc.cost.peak.booking_fee
    tnc_cost_offpeak_per_minute = cfg.tnc.cost.offpeak.per_minute
    tnc_cost_offpeak_per_mile   = cfg.tnc.cost.offpeak.per_mile
    tnc_cost_offpeak_base_fare  = cfg.tnc.cost.offpeak.base_fare
    tnc_cost_offpeak_min_fare   = cfg.tnc.cost.offpeak.min_fare
    tnc_cost_offpeak_booking_fee= cfg.tnc.cost.offpeak.booking_fee

    tnc_surcharge_rates = {}
    for k,v in cfg.tnc.surcharge_rates.items():
        tnc_surcharge_rates[k] = np.float32(v)

    tnc_surcharge_zones = {}
    for k, v in cfg.tnc.surcharge_zones.items():
        tnc_surcharge_zones[k] = np.asarray(v, dtype=np.int32)

    tnc_surcharges = tuple(
        (k, tnc_surcharge_rates[k], tnc_surcharge_zones[k])
        for k in tnc_surcharge_zones
    )

    tncpool_cost_peak_per_minute    = cfg.tnc_pooled.cost.peak.per_minute
    tncpool_cost_peak_per_mile      = cfg.tnc_pooled.cost.peak.per_mile
    tncpool_cost_peak_base_fare     = cfg.tnc_pooled.cost.peak.base_fare
    tncpool_cost_peak_min_fare      = cfg.tnc_pooled.cost.peak.min_fare
    tncpool_cost_peak_booking_fee   = cfg.tnc_pooled.cost.peak.booking_fee
    tncpool_cost_offpeak_per_minute = cfg.tnc_pooled.cost.offpeak.per_minute
    tncpool_cost_offpeak_per_mile   = cfg.tnc_pooled.cost.offpeak.per_mile
    tncpool_cost_offpeak_base_fare  = cfg.tnc_pooled.cost.offpeak.base_fare
    tncpool_cost_offpeak_min_fare   = cfg.tnc_pooled.cost.offpeak.min_fare
    tncpool_cost_offpeak_booking_fee= cfg.tnc_pooled.cost.offpeak.booking_fee

    tncpool_surcharge_rates = {}
    for k,v in cfg.tnc_pooled.surcharge_rates.items():
        tncpool_surcharge_rates[k] = np.float32(v)

    tncpool_surcharge_zones = {}
    for k, v in cfg.tnc_pooled.surcharge_zones.items():
        tncpool_surcharge_zones[k] = np.asarray(v, dtype=np.int32)

    tncpool_surcharges = tuple(
        (k, tncpool_surcharge_rates[k], tncpool_surcharge_zones[k])
        for k in tncpool_surcharge_zones
    )

    dh.cfg['taxi_cost_struct'] = TaxiCostStruct(
        flag_pull  = cfg.taxi.cost.flag_pull,
        per_minute = cfg.taxi.cost.per_minute,
        per_mile   = cfg.taxi.cost.per_mile,
        airport_pickup_fee = cfg.taxi.cost.airport_departure,
        airport_pickup_zones=np.asarray(cfg.taxi.cost.airport_zones),
    )

    dh.cfg['tnc_cost_struct_solo_peak'] = TncCostStruct(
        base_fare=cfg.tnc.cost.peak.base_fare,
        per_minute=cfg.tnc.cost.peak.per_minute,
        per_mile=cfg.tnc.cost.peak.per_mile,
        min_fare=cfg.tnc.cost.peak.min_fare,
        booking_fee=cfg.tnc.cost.peak.booking_fee,
        special_fee=cfg.tnc.surcharge_rates.special,
        special_zones=np.asarray(cfg.tnc.surcharge_zones.special),
        downtown_fee=cfg.tnc.surcharge_rates.downtown,
        downtown_zones=np.asarray(cfg.tnc.surcharge_zones.downtown),
    )

    dh.cfg['tnc_cost_struct_solo_offpeak'] = TncCostStruct(
        base_fare=cfg.tnc.cost.offpeak.base_fare,
        per_minute=cfg.tnc.cost.offpeak.per_minute,
        per_mile=cfg.tnc.cost.offpeak.per_mile,
        min_fare=cfg.tnc.cost.offpeak.min_fare,
        booking_fee=cfg.tnc.cost.offpeak.booking_fee,
        special_fee=cfg.tnc.surcharge_rates.special,
        special_zones=np.asarray(cfg.tnc.surcharge_zones.special),
        downtown_fee=cfg.tnc.surcharge_rates.downtown,
        downtown_zones=np.asarray(cfg.tnc.surcharge_zones.downtown),
    )

    dh.cfg['tnc_cost_struct_pool_peak'] = TncCostStruct(
        base_fare=cfg.tnc_pooled.cost.peak.base_fare,
        per_minute=cfg.tnc_pooled.cost.peak.per_minute,
        per_mile=cfg.tnc_pooled.cost.peak.per_mile,
        min_fare=cfg.tnc_pooled.cost.peak.min_fare,
        booking_fee=cfg.tnc_pooled.cost.peak.booking_fee,
        special_fee=cfg.tnc_pooled.surcharge_rates.special,
        special_zones=np.asarray(cfg.tnc_pooled.surcharge_zones.special),
        downtown_fee=cfg.tnc_pooled.surcharge_rates.downtown,
        downtown_zones=np.asarray(cfg.tnc_pooled.surcharge_zones.downtown),
    )

    dh.cfg['tnc_cost_struct_pool_offpeak'] = TncCostStruct(
        base_fare=cfg.tnc_pooled.cost.offpeak.base_fare,
        per_minute=cfg.tnc_pooled.cost.offpeak.per_minute,
        per_mile=cfg.tnc_pooled.cost.offpeak.per_mile,
        min_fare=cfg.tnc_pooled.cost.offpeak.min_fare,
        booking_fee=cfg.tnc_pooled.cost.offpeak.booking_fee,
        special_fee=cfg.tnc_pooled.surcharge_rates.special,
        special_zones=np.asarray(cfg.tnc_pooled.surcharge_zones.special),
        downtown_fee=cfg.tnc_pooled.surcharge_rates.downtown,
        downtown_zones=np.asarray(cfg.tnc_pooled.surcharge_zones.downtown),
    )


    ##############
    #
    # @nb.njit(cache=True)
    # def taxi_cost(auto_time, auto_dist, taxi_cost_struct):
    #     """
    #     Compute taxi fare.
    #
    #     A single set of rates (Chicago medallion rates for in-city trips)
    #     is used; fares for taxi trips outside Chicago are close to this
    #     rate and rare enough that more precision is unneeded.
    #
    #     Parameters
    #     ----------
    #     auto_time, auto_dist : array-like
    #
    #     Returns
    #     -------
    #     fare : array-like
    #     """
    #     return (
    #             taxi_cost_struct.flag_pull
    #             + auto_time * taxi_cost_struct.per_minute
    #             + auto_dist * taxi_cost_struct.per_mile
    #     )
    #
    #
    # @nb.njit
    # def tnc_cost(auto_time, auto_dist, o_zone, d_zone, tnc_cost_struct):
    #     """
    #     Compute the TNC cost.
    #
    #     Parameters
    #     ----------
    #     auto_time, auto_dist : float
    #         The auto travel time and distance for a set of trips.
    #     o_zone, d_zone : int
    #         Zone numbers for origin and destination
    #     tnc_cost_struct : TncCostStruct
    #         Use the correct struct for peak/offpeak and solo/shared.
    #
    #     Returns
    #     -------
    #     float
    #     """
    #     cost = (
    #         tnc_cost_struct.per_minute * auto_time
    #         + tnc_cost_struct.per_mile * auto_dist
    #         + tnc_cost_struct.base_fare
    #     )
    #     cost = max(cost, tnc_cost_struct.min_fare) + tnc_cost_struct.booking_fee
    #
    #     if tnc_cost_struct.special_fee:
    #         bucket_applies = False
    #         for i in range(tnc_cost_struct.special_zones.size):
    #             if tnc_cost_struct.special_zones[i] == o_zone or tnc_cost_struct.special_zones[i] == d_zone:
    #                 bucket_applies = True
    #                 break
    #         if bucket_applies:
    #             cost += tnc_cost_struct.special_fee
    #
    #     if tnc_cost_struct.downtown_fee:
    #         bucket_applies = False
    #         for i in range(tnc_cost_struct.downtown_zones.size):
    #             if tnc_cost_struct.downtown_zones[i] == o_zone or tnc_cost_struct.downtown_zones[i] == d_zone:
    #                 bucket_applies = True
    #                 break
    #         if bucket_applies:
    #             cost += tnc_cost_struct.downtown_fee
    #
    #     return cost
    #
    #
    #
    # @nb.njit
    # def tnc_solo_cost(auto_time, auto_dist, o_zone, d_zone, peak):
    #     """
    #     Compute the solo rider TNC cost.
    #
    #     Parameters
    #     ----------
    #     auto_time, auto_dist : float
    #         The auto travel time and distance for a set of trips.
    #     o_zone, d_zone : int
    #         Zone numbers for origin and destination
    #     peak : bool
    #         Whether this trip is peak or offpeak.
    #
    #     Returns
    #     -------
    #     float
    #     """
    #     if peak:
    #         cost = (
    #             tnc_cost_peak_per_minute * auto_time
    #             + tnc_cost_peak_per_mile * auto_dist
    #             + tnc_cost_peak_base_fare
    #         )
    #         cost = max(cost, tnc_cost_peak_min_fare) + tnc_cost_peak_booking_fee
    #     else:
    #         cost = (
    #             tnc_cost_offpeak_per_minute * auto_time
    #             + tnc_cost_offpeak_per_mile * auto_dist
    #             + tnc_cost_offpeak_base_fare
    #         )
    #         cost = max(cost, tnc_cost_offpeak_min_fare) + tnc_cost_offpeak_booking_fee
    #
    #     for bucket_name, bucket_price, bucket_zones in tnc_surcharges:
    #         if bucket_price:
    #             bucket_applies = False
    #             for i in range(bucket_zones.size):
    #                 if bucket_zones[i] == o_zone or bucket_zones[i] == d_zone:
    #                     bucket_applies = True
    #             if bucket_applies:
    #                 cost += bucket_price
    #     return cost
    #
    #
    # @nb.njit
    # def tnc_pool_cost(auto_time, auto_dist, o_zone, d_zone, peak):
    #     """
    #     Compute the solo rider TNC cost.
    #
    #     Parameters
    #     ----------
    #     auto_time, auto_dist : float
    #         The auto travel time and distance for a set of trips.
    #     o_zone, d_zone : int
    #         Zone numbers for origin and destination
    #     peak : bool
    #         Whether this trip is peak or offpeak.
    #
    #     Returns
    #     -------
    #     float
    #     """
    #     if peak:
    #         cost = (
    #             tncpool_cost_peak_per_minute * auto_time
    #             + tncpool_cost_peak_per_mile * auto_dist
    #             + tncpool_cost_peak_base_fare
    #         )
    #         cost = max(cost, tncpool_cost_peak_min_fare) + tncpool_cost_peak_booking_fee
    #     else:
    #         cost = (
    #             tncpool_cost_offpeak_per_minute * auto_time
    #             + tncpool_cost_offpeak_per_mile * auto_dist
    #             + tncpool_cost_offpeak_base_fare
    #         )
    #         cost = max(cost, tncpool_cost_offpeak_min_fare) + tncpool_cost_offpeak_booking_fee
    #
    #     for bucket_name, bucket_price, bucket_zones in tncpool_surcharges:
    #         if bucket_price:
    #             bucket_applies = False
    #             for i in range(bucket_zones.size):
    #                 if bucket_zones[i] == o_zone or bucket_zones[i] == d_zone:
    #                     bucket_applies = True
    #             if bucket_applies:
    #                 cost += bucket_price
    #     return cost
    #
    # return taxi_cost, tnc_solo_cost, tnc_pool_cost