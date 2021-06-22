import numpy as np
import logging
log = logging.getLogger('CMAP')


"""
# Mode Choice Model Impedances

## Highway Costs

### Times and distances

Skims:
HBW trips
	Times – mf44 (SOV AM peak), mf76 (HOV AM peak)
	Distances - mf45 (SOV AM peak), mf77 (HOV AM peak)

HBO/NHB trips
Times – mf46 (SOV midday)
Distances - mf47 (SOV midday)

### Parking Costs

HBW trips to the Central Area – Monte Carlo simulation using
costs from MCHW_CBDPARK.TXT

All other trips – zonal Park and Ride cost from MCxx_M01.TXT

Hours of parking: HBW=10, HBO=6, NHB=3

### Auto Operating Costs

Supplied by MCxx_M023.TXT in 5 MPH increments

## Transit Costs

Times and fares	Skims:
HBW trips
In-vehicle time – mf822 (AM peak)
Out of vehicle time – mf823 (AM peak) [walk transfer but not access/egress]
Headway – mf838 (AM peak)
Fare – mf828 (AM peak)

HBO/NHB trips
In-vehicle time – mf922 (midday)
Out of vehicle time – mf923 (midday) [walk transfer but not access/egress]
Headway – mf938 (midday)
Fare – mf928 (midday)

Access/egress are simulated for each traveler.
	First mode and last mode are obtained from transit skims
	(mf829|mf929 and mf831|mf931) – these are used to determine
	which of the 5 access/egress modes (walk, bus, feeder bus, P&R, K&R)
	are simulated.  That information is used to pull the appropriate data
	from MCxx_DISTR: average zonal distance and standard deviation of
	the modes to simulate actual distance.

"""

from ..addict import Dict
import larch
import os
from os.path import join as pj
from ..skims import read_skims


class DictSkims(Dict):
	# defines special pickling
	def __getstate__(self):
		return self.get('filename', None)
	def __setstate__(self, state):
		self.update(_load_skims(state))
	def __reduce__(self):
		return (
			type(self),
			(),
			self.__getstate__(),
		)


def load_skims(filenames, dh=None):
	from ..util import search_path
	return _load_skims(search_path(filenames.emmemat), dh=dh)


def _load_skims(filename_emmemat, dh=None):
	try:
		skims = DictSkims()
		log.info(f"filename emmemat = {filename_emmemat}")
		skims.filename = filename_emmemat
		skims.raw = read_skims(filename_emmemat)

		skims.raw['mf45'] = skims.raw['mf45'].load()
		skims.raw['mf47'] = skims.raw['mf47'].load()

		skims.auto.col_mapping = dict(
			am_time='mf44',
			am_dist='mf45',
			am_toll_loinc='mf111',  # TODO
			am_toll_hiinc='mf114',  # TODO
			md_time='mf46',
			md_dist='mf47',
			md_toll='mf117',  # TODO
			am_time_hov='mf76',
			am_dist_hov='mf77',
			am_toll_hov_loinc='mf112',  # TODO
			am_toll_hov_hiinc='mf115',  # TODO
			am_opcost='am_opcost',
			am_opcost_hov='am_opcost_hov',
			md_opcost='md_opcost',
		)

		skims.transit_pk.col_mapping = Dict(
			ivtt=        'mf822',
			ovtt=        'mf823',
			headway=     'mf838',
			fare=        'mf828',
			firstmode=   'mf829',
			prioritymode='mf830',
			lastmode=    'mf831',
		)

		skims.transit_op.col_mapping = Dict(
			ivtt=        'mf922',
			ovtt=        'mf923',
			headway=     'mf938',
			fare=        'mf928',
			firstmode=   'mf929',
			prioritymode='mf930',
			lastmode=    'mf931',
		)

		if dh is not None:
			skims = _load_skims_step_2(skims, dh)
		return skims
	except:
		log.exception("exception in _load_skims")
		raise

def _load_skims_step_2(skims, dh):
	"""
	Some post-processing when `dh` is available

	Parameters
	----------
	skims
	dh

	Returns
	-------
	skimss
	"""
	try:
		try:
			skims.raw['mf829']
		except KeyError:
			if dh is None:
				raise ValueError("no data_handler defined, and transit skim convolution is needed")
			from ..transit_skim_convolution import skim_convol
			dh['skims'] = skims
			skim_convol(dh, peak=True)

		try:
			skims.raw['mf929']
		except KeyError:
			if dh is None:
				raise ValueError("no data_handler defined, and transit skim convolution is needed")
			from ..transit_skim_convolution import skim_convol
			dh['skims'] = skims
			skim_convol(dh, peak=False)

		# skims.first_mode_peak       = skims.transit_pk.raw[pick_in(skims.transit_pk.raw, 'mf829_$', 'mf829')][:].astype(np.int8)+1
		# skims.priority_mode_peak    = skims.transit_pk.raw[pick_in(skims.transit_pk.raw, 'mf830_$', 'mf830')][:].astype(np.int8)+1
		# skims.last_mode_peak        = skims.transit_pk.raw[pick_in(skims.transit_pk.raw, 'mf831_$', 'mf831')][:].astype(np.int8)+1
		# skims.first_mode_offpeak    = skims.transit_op.raw[pick_in(skims.transit_op.raw, 'mf929_$', 'mf929')][:].astype(np.int8)+1
		# skims.priority_mode_offpeak = skims.transit_op.raw[pick_in(skims.transit_op.raw, 'mf930_$', 'mf930')][:].astype(np.int8)+1
		# skims.last_mode_offpeak     = skims.transit_op.raw[pick_in(skims.transit_op.raw, 'mf931_$', 'mf931')][:].astype(np.int8)+1
		skims.first_mode_peak       = skims.raw['mf829'].load().values.astype(np.int8)+1
		skims.priority_mode_peak    = skims.raw['mf830'].load().values.astype(np.int8)+1
		skims.last_mode_peak        = skims.raw['mf831'].load().values.astype(np.int8)+1

		skims.first_mode_offpeak    = skims.raw['mf929'].load().values.astype(np.int8)+1
		skims.priority_mode_offpeak = skims.raw['mf930'].load().values.astype(np.int8)+1
		skims.last_mode_offpeak     = skims.raw['mf931'].load().values.astype(np.int8)+1

		def to_speed_bucket(_d, _t):
			return np.where(
				skims.raw[skims.auto.col_mapping[_t]].load(),
				np.round(np.ceil(
					skims.raw[skims.auto.col_mapping[_d]].load() /
					skims.raw[skims.auto.col_mapping[_t]].load() * 60. / 5.
				)),
				0
			).astype(np.uint8)
		def to_opcost(_d, _t):
			return (
				dh.m023.AUTO_OPERATING_COST_BY_SPEED[to_speed_bucket(_d, _t)] / 100.
				* skims.raw[skims.auto.col_mapping[_d]].load()
			)
		skims.raw['am_opcost'] = to_opcost('am_dist', 'am_time')
		skims.raw['am_opcost_hov'] = to_opcost('am_dist_hov', 'am_time_hov')
		skims.raw['md_opcost'] = to_opcost('md_dist', 'md_time')

		return skims
	except:
		log.exception("exception in _load_skims")
		raise
