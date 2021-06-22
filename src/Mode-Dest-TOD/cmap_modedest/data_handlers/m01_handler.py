
import logging
log = logging.getLogger('CMAP')
import pandas as pd
import numpy as np
from ..addict import Dict
from .filepaths import latest_matching
from ..util import search_path

def read_m01(filename):
	raw = pd.read_csv(filename, header=None, index_col=0)

	columns = [
		'zone_type',
		'pnr_parking_cost',
		'zone_income',
		'pnr_flag',
		'first_wait_bus_peak',
		'first_wait_bus_offpeak',
		'first_wait_feeder_peak',
		'first_wait_feeder_offpeak',
	]

	# autocc column only appears in HW files
	if len(raw.columns) == len(columns)+1:
		columns.append('autocc')

	raw.columns = columns
	raw.index.name = 'zone'
	return raw


def load_m01(filenames):
	m01 = Dict()

	m01.HW = read_m01(filenames.PDHW_M01)
	# There is only one unique m01 file
	#   the others are simply copies
	# m01.HO = read_m01(filenames.PDHO_M01)
	# m01.NH = read_m01(filenames.PDNH_M01)

	m01.HW['taxi_wait_pk'] = m01.HW.zone_type.map(filenames.cfg.taxi.wait_time.peak)
	m01.HW['taxi_wait_op'] = m01.HW.zone_type.map(filenames.cfg.taxi.wait_time.offpeak)

	m01.HW['tnc_solo_wait_pk'] = m01.HW.zone_type.map(filenames.cfg.tnc.wait_time.peak)
	m01.HW['tnc_solo_wait_op'] = m01.HW.zone_type.map(filenames.cfg.tnc.wait_time.offpeak)
	m01.HW['tnc_pool_wait_pk'] = m01.HW.zone_type.map(filenames.cfg.tnc_pooled.wait_time.peak)
	m01.HW['tnc_pool_wait_op'] = m01.HW.zone_type.map(filenames.cfg.tnc_pooled.wait_time.offpeak)

	return m01.HW


def attach_areatypes(dh, df, prefix, suffix, targetzone):
	"""
	Attach area type and default auto propensity

	Parameters
	----------
	dh
	df
	prefix
	suffix
	targetzone

	Returns
	-------

	"""
	m01 = dh.m01

	if isinstance(targetzone, str):
		targetzone = np.asarray(df[targetzone])

	if prefix and prefix[-1]!="_":
		prefix = f"{prefix}_"

	if suffix and suffix[0]!="_":
		suffix = f"_{suffix}"

	def _map(x, y):
		if isinstance(x, pd.Series):
			return x.map(y)
		else:
			return y[x]

	areatype = m01['zone_type']

	# auto propensity is reloaded every time we attach to ensure it is up-to-date
	auto_propensity_file = search_path(
		dh.filenames.cache_dir / "computed_auto_propensity.csv",
		dh.filenames.emme_database_dir / "computed_auto_propensity.csv",
		dh.filenames.emme_database_dir / "default_auto_propensity.csv",
		dh.filenames.emme_database_dir / "default_auto_propensity.csv.gz",
	)
	autopropensity = pd.read_csv(auto_propensity_file, index_col=0)['auto_propensity']
	df[f'{prefix}areatype{suffix}'] = np.asarray(_map(targetzone,areatype))
	df[f'{prefix}autopropensity{suffix}'] = np.asarray(_map(targetzone,autopropensity))
	return df


def attach_hired_car_waits(dh, df, prefix, origin_zone):
	"""
	Attach taxi and TNC wait times to a dataframe.

	Parameters
	----------
	dh : DataHandler
	df : DataFrame
		New columns will be added to this dataframe.
	prefix : str
		This prefix will be added to the new columns.
	origin_zone : str or pd.Series or int
		The origin zone for each row in `df`, given as the column
		name or a separate indexed-alike Series.

	Returns
	-------
	df : DataFrame
	"""
	m01 = dh.m01

	if isinstance(origin_zone, str):
		origin_zone = df[origin_zone]

	if prefix and prefix[-1]!="_":
		prefix = f"{prefix}_"

	def _map(x, y):
		if isinstance(x, pd.Series):
			return x.map(y)
		else:
			return y[x]

	# Add taxi and TNC wait time data
	taxi_wait_pk = m01['taxi_wait_pk']
	taxi_wait_op = m01['taxi_wait_op']
	df[f'{prefix}taxi_wait_time_PEAK'] = _map(origin_zone,taxi_wait_pk)
	df[f'{prefix}taxi_wait_time_OFFPEAK'] = _map(origin_zone,taxi_wait_op)
	# df[f'{prefix}taxi_wait_time'] = (
	# 		origin_zone.map(taxi_wait_pk) * trips.in_peak
	# 		+ origin_zone.map(taxi_wait_op) * ~trips.in_peak
	# )
	tnc_solo_wait_pk = m01['tnc_solo_wait_pk']
	tnc_solo_wait_op = m01['tnc_solo_wait_op']
	df[f'{prefix}tnc_solo_wait_time_PEAK'] = _map(origin_zone,tnc_solo_wait_pk)
	df[f'{prefix}tnc_solo_wait_time_OFFPEAK'] = _map(origin_zone,tnc_solo_wait_op)
	# df[f'{prefix}tnc_solo_wait_time'] = (
	# 		origin_zone.map(tnc_solo_wait_pk) * trips.in_peak
	# 		+ origin_zone.map(tnc_solo_wait_op) * ~trips.in_peak
	# )
	tnc_pool_wait_pk = m01['tnc_pool_wait_pk']
	tnc_pool_wait_op = m01['tnc_pool_wait_op']
	df[f'{prefix}tnc_pool_wait_time_PEAK'] = _map(origin_zone,tnc_pool_wait_pk)
	df[f'{prefix}tnc_pool_wait_time_OFFPEAK'] = _map(origin_zone,tnc_pool_wait_op)
	# df[f'{prefix}tnc_pool_wait_time'] = (
	# 		origin_zone.map(tnc_pool_wait_pk) * trips.in_peak
	# 		+ origin_zone.map(tnc_pool_wait_op) * ~trips.in_peak
	# )
	return df

def sample_hh_income_cats(dh, otaz, n, income_breaks='5', random_state=None, sigma=1.2, trunc_min=None, trunc_max=None):
	from ..incomes import random_incomes
	return random_incomes(
		median_income=dh.m01.zone_income[otaz]*100,
		replications=n,
		random_state=random_state,
		sigma=sigma,
		bins=income_breaks,
		trunc_min=trunc_min,
		trunc_max=trunc_max,
	)
