import numpy as np
import pandas as pd

peak_tnc_pricing = {
	'OFFPEAK': 0,
	'PEAK': 1,
}


def taxi_cost(dh, auto_time, auto_dist, o_zone, d_zone):
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
	return (
		dh.cfg.taxi.cost.flag_pull
		+ auto_time * dh.cfg.taxi.cost.per_minute
		+ auto_dist * dh.cfg.taxi.cost.per_mile
	)


def tnc_solo_cost(dh, auto_time, auto_dist, o_zone, d_zone, peak):
	"""
	Compute the solo rider TNC cost.

	Parameters
	----------
	auto_time, auto_dist : array-like of float
		The auto travel time and distance for a set of trips.
		Shapes must match.
	o_zone, d_zone : pd.Series of int
		Zone numbers for origin and destination
		Shapes must match `auto_time`.
	peak : pd.Series of bool
		Whether each trip is peak or offpeak.
		Shapes must match `auto_time`.

	Returns
	-------
	ndarray
		Same shape as inputs
	"""

	peak_fare = (
		dh.cfg.tnc.cost.peak.per_minute * auto_time
		+ dh.cfg.tnc.cost.peak.per_mile * auto_dist
		+ dh.cfg.tnc.cost.peak.base_fare
	)
	peak_fare = np.fmax(peak_fare, dh.cfg.tnc.cost.peak.min_fare) + dh.cfg.tnc.cost.peak.booking_fee

	offpeak_fare = (
		dh.cfg.tnc.cost.offpeak.per_minute * auto_time
		+ dh.cfg.tnc.cost.offpeak.per_mile * auto_dist
		+ dh.cfg.tnc.cost.offpeak.base_fare
	)
	offpeak_fare = np.fmax(offpeak_fare, dh.cfg.tnc.cost.offpeak.min_fare) + dh.cfg.tnc.cost.offpeak.booking_fee

	cost = peak_fare * peak + offpeak_fare * (1-peak)
	for bucket_name, bucket_price in dh.cfg.tnc.surcharge_rates.items():
		if bucket_price:
			bucket_applies = (
				np.isin(o_zone, dh.cfg.tnc.surcharge_zones[bucket_name])
				| np.isin(d_zone, dh.cfg.tnc.surcharge_zones[bucket_name])
			).astype(float)
			cost += bucket_applies * bucket_price
	return cost


def tnc_pool_cost(dh, auto_time, auto_dist, o_zone, d_zone, peak):
	"""
	Compute the pooled rider TNC cost.

	Parameters
	----------
	auto_time, auto_dist : array-like of float
		The auto travel time and distance for a set of trips/
		Shapes must match.
	o_zone, d_zone : pd.Series of int
		Zone numbers for origin and destination
		Shapes must match `auto_time`.
	peak : pd.Series of bool
		Whether each trip is peak or offpeak.
		Shapes must match `auto_time`.

	Returns
	-------
	ndarray
		Same shape as inputs
	"""

	peak_fare = (
		dh.cfg.tnc_pooled.cost.peak.per_minute * auto_time
		+ dh.cfg.tnc_pooled.cost.peak.per_mile * auto_dist
		+ dh.cfg.tnc_pooled.cost.peak.base_fare
	)
	peak_fare = np.fmax(peak_fare, dh.cfg.tnc_pooled.cost.peak.min_fare) + dh.cfg.tnc_pooled.cost.peak.booking_fee

	offpeak_fare = (
		dh.cfg.tnc_pooled.cost.offpeak.per_minute * auto_time
		+ dh.cfg.tnc_pooled.cost.offpeak.per_mile * auto_dist
		+ dh.cfg.tnc_pooled.cost.offpeak.base_fare
	)
	offpeak_fare = np.fmax(offpeak_fare, dh.cfg.tnc_pooled.cost.offpeak.min_fare) + dh.cfg.tnc_pooled.cost.offpeak.booking_fee

	cost = peak_fare * peak + offpeak_fare * (1-peak)
	for bucket_name, bucket_price in dh.cfg.tnc_pooled.surcharge_rates.items():
		if bucket_price:
			bucket_applies = (
				np.isin(o_zone, dh.cfg.tnc_pooled.surcharge_zones[bucket_name])
				| np.isin(d_zone, dh.cfg.tnc_pooled.surcharge_zones[bucket_name])
			).astype(float)
			cost += bucket_applies * bucket_price
	return cost
