import numpy as np
import pandas as pd
import cmap_modedest
import sharrow as sh

from .est_data import dh
skims = dh.skims
aggregate_attractions = np.fmin(
	dh.trip_attractions5.sum(1),
	np.percentile(dh.trip_attractions5.sum(1), 99.5),
)
zone_shp = dh.zone_shp
m01 = dh.m01
from ..tnc_costs import taxi_cost, tnc_solo_cost, tnc_pool_cost
from ..purposes import purposes5, purposes3, purposes8, purposesA

log = cmap_modedest.log_to_stderr(level=10)


def default_weighting_by_distance(distances):
	return 1 / distances ** 2


def sample_dest_zones(trips, n_zones, n_sampled_dests=20, wgt_func=None):
	"""
	Sample destinations for a set of origins.

	Parameters
	----------
	trips : DataFrame
	n_zones : int
	n_sampled_dests : int
	wgt_func : callable, optional
		This callable takes a 1-d array of distances to candidate
		destination zones as input, and returns a same-shape array
		of sampling weights. If not provided, the default scheme is
		used, see `default_weighting_by_distance`.

	Returns
	-------
	trip_alt_dest : array[len(trips), n_sampled_dests] of int
	trip_alt_wgts : array[len(trips), n_sampled_dests] of float
	trip_obs_wgts : array[len(trips)] of float
	"""
	if wgt_func is None:
		wgt_func = default_weighting_by_distance
	trip_alt_dest = np.zeros([len(trips), n_sampled_dests], dtype=np.int32)
	trip_alt_wgts = np.zeros([len(trips), n_sampled_dests], dtype=np.float32)
	trip_obs_wgts = np.zeros([len(trips)], dtype=np.float32)
	for zone_index in range(n_zones):
		zone_id = zone_index+1
		trips_from_this_zone = trips['o_zone'] == zone_id
		n_trips_from_this_zone = trips_from_this_zone.sum()
		if n_trips_from_this_zone == 0:
			continue
		zone_rgen = np.random.default_rng(zone_id)
		from ..skims.names import skim_tags
		distances = skims.raw[skim_tags.auto_md_dist].load()[zone_index, :n_zones]
		distances[zone_index] = float(np.sqrt(zone_shp.loc[[zone_id]].area)/5280)
		#distances = np.fmax(distances, 0.33)
		zone_weight = wgt_func(distances).astype(np.float64)
		zone_weight *= aggregate_attractions
		zone_weight /= zone_weight.sum()
		samp_dest = zone_rgen.choice(
			n_zones,
			size=(n_trips_from_this_zone,n_sampled_dests),
			p=zone_weight,
		)
		trip_alt_dest[trips_from_this_zone] = samp_dest+1
		trip_alt_wgts[trips_from_this_zone] = zone_weight.values[samp_dest]
		trip_obs_wgts[trips_from_this_zone] = zone_weight.values[trips.loc[trips_from_this_zone, 'd_zone']-1]
	return trip_alt_dest, trip_alt_wgts, trip_obs_wgts


def sample_dest_zones_and_data(
		trips,
		n_zones,
		n_sampled_dests=20,
		wgt_func=None,
		ozone_col='o_zone',
		labeler=lambda i: f'altdest{i + 1:04d}',
		keep_trips_cols=(),
):
	log.debug("sample_dest_zones()")
	trip_alt_dest, trip_alt_wgts, trip_obs_wgts = sample_dest_zones(
		trips, n_zones, n_sampled_dests, wgt_func,
	)

	_keep_trips_cols = []
	for k in keep_trips_cols:
		if k in trips.columns:
			_keep_trips_cols.append(k)
		k = f"actualdest_{k}"
		if k in trips.columns:
			_keep_trips_cols.append(k)
		for t in ['PEAK', 'OFFPEAK']:
			if f"{k}_{t}" in trips.columns:
				_keep_trips_cols.append(f"{k}_{t}")
		for purpose in purposesA:
			if f"{k}_{purpose}" in trips.columns:
				_keep_trips_cols.append(f"{k}_{purpose}")
		for purpose3 in purposes3:
			if f"{k}_{purpose3}" in trips.columns:
				_keep_trips_cols.append(f"{k}_{purpose3}")

	trip_alt_dest_df = trips[[ozone_col, 'in_peak', *_keep_trips_cols]]

	log.debug("trip_alt_dest_df.join()")
	trip_alt_dest_df = trip_alt_dest_df.join(
		pd.DataFrame(
			trip_alt_dest,
			index=trips.index,
			columns=[labeler(i) for i in range(trip_alt_dest.shape[1])],
		)
	)
	trip_alt_dest_df['actualdest_samp_wgt'] = trip_obs_wgts
	trip_alt_dest_df = trip_alt_dest_df.join(
		pd.DataFrame(
			trip_alt_wgts,
			index=trips.index,
			columns=[f'{labeler(i)}_samp_wgt' for i in range(trip_alt_dest.shape[1])],
		),
	)

	flipY = trips.paFlip
	flipN = 1-trips.paFlip

	from .est_survey import attach_selected_skims_sh
	for i in range(n_sampled_dests):

		origin_zone = trip_alt_dest_df.o_zone * flipN + trip_alt_dest_df[labeler(i)] * flipY
		destin_zone = trip_alt_dest_df.o_zone * flipY + trip_alt_dest_df[labeler(i)] * flipN

		# attach auto skims
		log.debug(f"attach auto skims <{i}>")
		flow = sh.SharedData(
			['otaz', 'dtaz', 'in_peak'],
			skims=dh.skims.raw
		).setup_flow({
			f'{labeler(i)}_auto_time': f'skims.mf44 if in_peak else skims.mf46',
			f'{labeler(i)}_auto_dist': f'skims.mf45 if in_peak else skims.mf47',
			f'{labeler(i)}_auto_op_dist': 'skims.mf47',
			f'{labeler(i)}_auto_time_PEAK': 'skims.mf44',
			f'{labeler(i)}_auto_time_hov_PEAK': 'skims.mf76',
			f'{labeler(i)}_auto_time_OFFPEAK': 'skims.mf46',
			f'{labeler(i)}_auto_dist_PEAK': 'skims.mf45',
			f'{labeler(i)}_auto_dist_OFFPEAK': 'skims.mf47',
			f'{labeler(i)}_auto_toll_loinc_PEAK': f'skims.mf111',
			f'{labeler(i)}_auto_toll_hiinc_PEAK': f'skims.mf114',
			f'{labeler(i)}_auto_toll_OFFPEAK': f'skims.mf117',
			f'{labeler(i)}_auto_toll_hov_loinc_PEAK': f'skims.mf112',
			f'{labeler(i)}_auto_toll_hov_hiinc_PEAK': f'skims.mf115',
			f'{labeler(i)}_auto_opcost_PEAK': 'skims.am_opcost',
			f'{labeler(i)}_auto_opcost_hov_PEAK': 'skims.am_opcost_hov',
			f'{labeler(i)}_auto_opcost_OFFPEAK': 'skims.md_opcost',
		})
		auto_cols = flow.load(
			trip_alt_dest_df[['o_zone', labeler(i), 'in_peak']].rename(
				columns={'o_zone':'otaz', labeler(i):'dtaz'}
			),
			as_dataframe=True,
		)
		trip_alt_dest_df = pd.concat([trip_alt_dest_df, auto_cols], axis=1)

		# trip_alt_dest_df = attached_auto = attach_selected_skims_sh(
		# 	trip_alt_dest_df,
		# 	'o_zone',
		# 	labeler(i),
		# 	skims.raw,
		# 	(
		# 		("in_peak", {
		# 			'mf44': f'{labeler(i)}_auto_time',
		# 			'mf45': f'{labeler(i)}_auto_dist',
		# 		}),
		# 		("~in_peak", {
		# 			'mf46': f'{labeler(i)}_auto_time',
		# 			'mf47': f'{labeler(i)}_auto_dist',
		# 		}),
		# 		(None, {
		# 			'mf47': f'{labeler(i)}_auto_op_dist',
		# 		}),
		# 		(None, {
		# 			'mf44': f'{labeler(i)}_auto_time_PEAK',
		# 			'mf45': f'{labeler(i)}_auto_dist_PEAK',
		# 		}),
		# 		(None, {
		# 			'mf46': f'{labeler(i)}_auto_time_OFFPEAK',
		# 			'mf47': f'{labeler(i)}_auto_dist_OFFPEAK',
		# 		}),
		# 	),
		# )

		# if np.any(attached_auto.isna()):
		# 	print(attached_auto.isna().sum())
		# 	pass

		# add areatypes
		from ..data_handlers.m01_handler import attach_areatypes
		trip_alt_dest_df = attach_areatypes(dh, trip_alt_dest_df, labeler(i), '', labeler(i))  # altdest0000_areatype

		# set intrazonal auto_dist
		intrazonal = (trip_alt_dest_df['o_zone'] == trip_alt_dest_df[labeler(i)])
		intrazonal_zone_area = dh.zone_shp.loc[trip_alt_dest_df.loc[intrazonal, labeler(i)]].Shape_Area  # square feet
		intrazonal_dist = np.sqrt(intrazonal_zone_area).to_numpy() / 5280 * 0.667
		trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_dist_OFFPEAK'] = intrazonal_dist
		trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_dist_PEAK'] = intrazonal_dist

		# set intrazonal auto_time
		trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_time_PEAK'] = (
				trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_dist_PEAK']
				* 60 / (trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_areatype'].map(
			lambda y: dh.cfg.intrazonal_auto_speed.peak.get(y, 15)))
		)
		trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_time_OFFPEAK'] = (
				trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_auto_dist_OFFPEAK']
				* 60 / (trip_alt_dest_df.loc[intrazonal, f'{labeler(i)}_areatype'].map(
			lambda y: dh.cfg.intrazonal_auto_speed.offpeak.get(y, 15)))
		)

		# Add taxi and TNC wait time data
		from ..data_handlers.m01_handler import attach_hired_car_waits
		trip_alt_dest_df = attach_hired_car_waits(dh, trip_alt_dest_df, labeler(i), origin_zone)
		# taxi_wait_pk = m01['taxi_wait_pk']
		# taxi_wait_op = m01['taxi_wait_op']
		# trip_alt_dest_df[f'{labeler(i)}_taxi_wait_time_PEAK'] = origin_zone.map(taxi_wait_pk)
		# trip_alt_dest_df[f'{labeler(i)}_taxi_wait_time_OFFPEAK'] = origin_zone.map(taxi_wait_op)
		# trip_alt_dest_df[f'{labeler(i)}_taxi_wait_time'] = (
		# 		origin_zone.map(taxi_wait_pk) * trips.in_peak
		# 		+ origin_zone.map(taxi_wait_op) * ~trips.in_peak
		# )
		# tnc_solo_wait_pk = m01['tnc_solo_wait_pk']
		# tnc_solo_wait_op = m01['tnc_solo_wait_op']
		# trip_alt_dest_df[f'{labeler(i)}_tnc_solo_wait_time_PEAK'] = origin_zone.map(tnc_solo_wait_pk)
		# trip_alt_dest_df[f'{labeler(i)}_tnc_solo_wait_time_OFFPEAK'] = origin_zone.map(tnc_solo_wait_op)
		# trip_alt_dest_df[f'{labeler(i)}_tnc_solo_wait_time'] = (
		# 		origin_zone.map(tnc_solo_wait_pk) * trips.in_peak
		# 		+ origin_zone.map(tnc_solo_wait_op) * ~trips.in_peak
		# )
		# tnc_pool_wait_pk = m01['tnc_pool_wait_pk']
		# tnc_pool_wait_op = m01['tnc_pool_wait_op']
		# trip_alt_dest_df[f'{labeler(i)}_tnc_pool_wait_time_PEAK'] = origin_zone.map(tnc_pool_wait_pk)
		# trip_alt_dest_df[f'{labeler(i)}_tnc_pool_wait_time_OFFPEAK'] = origin_zone.map(tnc_pool_wait_op)
		# trip_alt_dest_df[f'{labeler(i)}_tnc_pool_wait_time'] = (
		# 		origin_zone.map(tnc_pool_wait_pk) * trips.in_peak
		# 		+ origin_zone.map(tnc_pool_wait_op) * ~trips.in_peak
		# )
		# Add taxi and TNC fare data
		for t in ['PEAK', 'OFFPEAK']:
			trip_alt_dest_df[f'{labeler(i)}_taxi_fare_{t}'] = taxi_cost(
				dh,
				trip_alt_dest_df[f'{labeler(i)}_auto_time_{t}'],
				trip_alt_dest_df[f'{labeler(i)}_auto_dist_{t}'],
				origin_zone,
				destin_zone,
			)
			trip_alt_dest_df[f'{labeler(i)}_tnc_solo_fare_{t}'] = tnc_solo_cost(
				dh,
				trip_alt_dest_df[f'{labeler(i)}_auto_time_{t}'],
				trip_alt_dest_df[f'{labeler(i)}_auto_dist_{t}'],
				origin_zone,
				destin_zone,
				1 if (t=='PEAK') else 0,
			)
			trip_alt_dest_df[f'{labeler(i)}_tnc_pool_fare_{t}'] = tnc_pool_cost(
				dh,
				trip_alt_dest_df[f'{labeler(i)}_auto_time_{t}'],
				trip_alt_dest_df[f'{labeler(i)}_auto_dist_{t}'],
				origin_zone,
				destin_zone,
				1 if (t=='PEAK') else 0,
			)

		# attach transit skims
		log.debug(f"attach transit skims <{i}>")
		skim_tags = ('ivtt','ovtt','headway','fare','firstmode','prioritymode','lastmode')

		transit_joint = {}
		for k in skim_tags:
			_pk = dh.skims.transit_pk.col_mapping[k]
			_op = dh.skims.transit_op.col_mapping[k]
			transit_joint[f'{labeler(i)}_transit_{k}'] = f"skims.{_pk} if in_peak else skims.{_op}"
			transit_joint[f'{labeler(i)}_transit_{k}_PEAK'] = f"skims.{_pk}"
			transit_joint[f'{labeler(i)}_transit_{k}_OFFPEAK'] = f"skims.{_op}"
		flow = sh.SharedData(
			['otaz', 'dtaz', 'in_peak'],
			skims=dh.skims.raw
		).setup_flow(transit_joint)
		new_cols = flow.load(
			trip_alt_dest_df[['o_zone', labeler(i), 'in_peak']].rename(columns={'o_zone':'otaz', labeler(i):'dtaz'}),
			as_dataframe=True,
		)
		trip_alt_dest_df = pd.concat([trip_alt_dest_df, new_cols], axis=1)

		# trip_alt_dest_df = attach_selected_skims_sh(
		# 	trip_alt_dest_df,
		# 	'o_zone',
		# 	labeler(i),
		# 	skims.raw,
		# 	(
		# 		("in_peak", {
		# 			skims.transit_pk.col_mapping[j]: f'{labeler(i)}_transit_{j}'
		# 			for j in skim_tags
		# 		}),
		# 		(None, {
		# 			skims.transit_pk.col_mapping[j]: f'{labeler(i)}_transit_{j}_PEAK'
		# 			for j in skim_tags
		# 		}),
		# 	),
		# )
		#
		# if np.any(trip_alt_dest_df.isna()):
		# 	print(trip_alt_dest_df.isna().sum())
		# 	pass
		# else:
		# 	pass
		#
		# trip_alt_dest_df = attach_selected_skims_sh(
		# 	trip_alt_dest_df,
		# 	'o_zone',
		# 	labeler(i),
		# 	skims.raw,
		# 	(
		# 		("~in_peak", {
		# 			skims.transit_op.col_mapping[j]: f'{labeler(i)}_transit_{j}'
		# 			for j in skim_tags
		# 		}),
		# 		(None, {
		# 			skims.transit_op.col_mapping[j]: f'{labeler(i)}_transit_{j}_OFFPEAK'
		# 			for j in skim_tags
		# 		}),
		# 	),
		# )

		if np.any(trip_alt_dest_df.isna()):
			print(trip_alt_dest_df.isna().sum())
			pass
		else:
			pass

		# clipping to set invalid skim values to NaN?, facilitates more useful statistics.
		log.debug(f"clipping to set invalid skim values to NaN <{i}>")
		for j in ['ivtt', 'ovtt', 'headway', 'fare']:
			x = trip_alt_dest_df[f'{labeler(i)}_transit_{j}']
			trip_alt_dest_df.loc[x>9999, f'{labeler(i)}_transit_{j}'] = np.nan
			for t in ['PEAK', 'OFFPEAK']:
				varname = f'{labeler(i)}_transit_{j}_{t}'
				x = trip_alt_dest_df[varname]
				trip_alt_dest_df.loc[x > 9999, varname] = np.nan

		# parking costs
		from ..parking_costs import parking_cost_v2
		from ..purposes import purposes_to_3
		for purpose in purposes5:
			_parking_cost, _free_parking = parking_cost_v2(
				dh,
				destin_zone,
				trip_alt_dest_df['hhinc_dollars'],
				dh.cfg.default_activity_durations[purposes_to_3[purpose]],
				purposes_to_3[purpose],
				random_state=hash(purpose) + 1,
			)
			trip_alt_dest_df[f'{labeler(i)}_auto_parking_cost_{purpose}'] = _parking_cost
			trip_alt_dest_df[f'{labeler(i)}_auto_parking_free_{purpose}'] = _free_parking

	return trip_alt_dest_df

