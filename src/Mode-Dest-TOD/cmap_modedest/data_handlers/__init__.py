import cloudpickle
import numpy as np
import pandas as pd
import tempfile
import larch
from ..addict import Dict
from pathlib import Path
try:
	from sharrow import Flow
except ImportError:
	Flow = type(np)

try:
	from geopandas import GeoDataFrame
except ImportError:
	GeoDataFrame = ()

from .filepaths import FileNames, PathAttr
from .skims_handler import DictSkims
from logging import getLogger
log = getLogger("CMAP")

class DataHandler:

	serial_dir = PathAttr()

	def __init__(
			self,
			filenames=None,
			serial_dir=None,
			tg_detail=False,
			tripclass='typical',
			backfill_uncompressed_skims=False,
			**kwargs,
	):

		self._jedi_names = set()

		self.artifacts = {}
		# artifacts tell how to reload things if they are not cached
		# this makes it easier to pass this DataHandler to subprocesses,
		# as we don't need to serialize data that is already serialized.

		self._cache = {}
		# the _cache is where we store things that we have already loaded
		# in this process, so they are available in RAM already.

		log.info("loading filenames")
		if filenames is None:
			filenames = FileNames(**kwargs)

		if serial_dir is None:
			log.debug("create serial temp dir")
			self._temporary_dir = tempfile.TemporaryDirectory()
			serial_dir = self._temporary_dir.name

		log.debug("serial_dir init")
		self.serial_dir = serial_dir

		log.debug("filenames init")
		self.filenames = filenames

		log.info("loading hhv_types")
		from .household_types import load_household_types
		self['hhv_types'] = load_household_types()

		log.info("loading distr")
		from .distr_handler import load_distr
		self['distr'] = load_distr(filenames)
		self._jedi_names.add('distr')

		log.info("loading m01")
		from .m01_handler import load_m01
		self['m01'] = load_m01(filenames)
		self._jedi_names.add('m01')

		log.info("loading m023")
		from .m023_handler import load_m023
		self['m023'] = load_m023(filenames)
		self._jedi_names.add('m023')

		log.info("loading shapes")
		from .shp_handler import load_zone_shapes
		self['zone_shp'] = load_zone_shapes(filenames)
		self._jedi_names.add('zone_shp')

		log.info("loading skims")
		from .skims_handler import load_skims
		self['skims'] = load_skims(
			filenames, self, backfill_uncompressed_skims=backfill_uncompressed_skims,
		)
		self._jedi_names.add('skims')

		log.info("loading tg")
		from .tg_handler import load_tg, load_hh_enum
		tripgens = load_tg(filenames, with_detail=tg_detail)
		self['tripclass'] = tripclass
		for k, tg in tripgens.items():
			# self[f'trip_attractions8_{k}'] = tg.trip_attractions8
			# self[f'trip_productions8_{k}'] = tg.trip_productions8
			# self[f'zone_productions8_{k}'] = tg.zone_productions8
			self[f'trip_attractions5_{k}'] = tg.trip_attractions5 # needed
			# self[f'trip_productions5_{k}'] = tg.trip_productions5
			self[f'zone_productions5_{k}'] = tg.zone_productions5 # needed
			# if tg_detail:
			# 	self[f'trip_attractions_detail_{k}'] = tg.trip_attractions_detail
			# 	self[f'trip_productions_detail_{k}'] = tg.trip_productions_detail
		self._jedi_names.add('tripclass')
		self._jedi_names.add('trip_attractions5')
		self._jedi_names.add('zone_productions5')

		log.info("loading hh enumeration")
		hh_enum, hh_tabulation = load_hh_enum(filenames)
		self['hh_enum'] = hh_enum
		self['hh_tabulation'] = hh_tabulation
		self._jedi_names.add('hh_enum')
		self._jedi_names.add('hh_tabulation')

		log.info("loading parking")
		from .parking_handler import load_cbd_parking
		parking = load_cbd_parking(filenames)
		self['cbd_parking_prices'] = parking.cbd_parking_prices
		self['cbd_parking_price_prob'] = parking.cbd_parking_price_prob
		self['cbd_parking'] = parking.cbd_parking
		self['cbd_parking2'] = parking.cbd_parking2
		self['CBD_PARKING_ZONES'] = parking.CBD_PARKING_ZONES
		self._jedi_names.add('cbd_parking_prices')
		self._jedi_names.add('cbd_parking_price_prob')
		self._jedi_names.add('cbd_parking')
		self._jedi_names.add('cbd_parking2')
		self._jedi_names.add('CBD_PARKING_ZONES')

		log.info("loading visitor trip tables")
		from .visitors import load_visitor_trips
		self['visitor_trips'] = load_visitor_trips(filenames, scale_factor=1.0)
		self._jedi_names.add('visitor_trips')

		log.info("loading of data handlers complete")

	def __dir__(self):
		return self._jedi_names | self.__dict__.keys()

	@property
	def cfg(self):
		return self.filenames.cfg

	@property
	def choice_model_params(self):
		return self.filenames.choice_model_params

	def __getstate__(self):
		return {
			'artifacts': self.artifacts,
			'filenames': self.filenames,
			'_serial_dir': self._serial_dir,
		}

	def __setstate__(self, state):
		self._cache = {}
		self.artifacts = state['artifacts']
		self.filenames = state['filenames']
		self._serial_dir = state['_serial_dir']

	def __setitem__(self, key, value):
		self._cache[key] = value
		if not isinstance(key, str):
			raise ValueError("keys must be str")
		if isinstance(value, GeoDataFrame):
			filename = self.serial_dir / f"{key}.pkl"
			with open(filename, "wb") as f:
				cloudpickle.dump(value, f)
			self.artifacts[key] = ('pickle', filename)
		elif isinstance(value, pd.DataFrame):
			filename = self.serial_dir / f"{key}.pq"
			try:
				value.to_parquet(filename)
			except ValueError:
				filename = self.serial_dir / f"{key}.h5"
				value.to_hdf(filename, key)
				self.artifacts[key] = ('DataFrame.h5', filename)
			else:
				self.artifacts[key] = ('DataFrame', filename)
		elif isinstance(value, pd.Series):
			filename = self.serial_dir / f"{key}.spq"
			pd.DataFrame(value).to_parquet(filename)
			self.artifacts[key] = ('Series', filename)
		elif isinstance(value, np.ndarray):
			filename = self.serial_dir / f"{key}.nm"
			mm = np.memmap(
				filename,
				dtype=value.dtype,
				mode='w+',
				shape=value.shape,
			)
			mm[:] = value[:]
			self.artifacts[key] = ('array', filename, value.dtype, value.shape)
		elif isinstance(value, larch.OMX):
			filename = value.filename
			self.artifacts[key] = ('OMX', filename)
		elif isinstance(value, DictSkims):
			filename = self.serial_dir / f"{key}.pkl"
			with open(filename, "wb") as f:
				cloudpickle.dump(value, f)
			self.artifacts[key] = ('DictSkims', filename)
		elif isinstance(value, Dict):
			self.artifacts[key] = ('Dict', set(value.keys()))
			for k1, v1 in value.items():
				self[f"{key}.{k1}"] = v1
		elif isinstance(value, Flow):
			pass # Flow objects are thread-local
		else:
			filename = self.serial_dir / f"{key}.pkl"
			with open(filename, "wb") as f:
				cloudpickle.dump(value, f)
			self.artifacts[key] = ('pickle', filename)

	def __getitem__(self, key):
		if key == 'trip_attractions5':
			return self[f'trip_attractions5_{self.tripclass}']
		elif key == 'zone_productions5':
			return self[f'zone_productions5_{self.tripclass}']
		if key in self._cache:
			return self._cache[key]
		art = self.artifacts[key]
		if art[0] == 'DataFrame':
			result = pd.read_parquet(art[1])
		elif art[0] == 'DataFrame.h5':
			result = pd.read_hdf(art[1], key)
		elif art[0] == 'Series':
			result = pd.read_parquet(art[1]).iloc[:,0]
		elif art[0] == 'array':
			result = np.memmap(
				art[1],
				dtype=art[2],
				mode='r+',
				shape=art[3],
			)
		elif art[0] == 'OMX':
			result = larch.OMX(art[1], 'r')
		elif art[0] == 'Dict':
			result = Dict()
			for k1 in art[1]:
				result[k1] = self[f"{key}.{k1}"]
		elif art[0] == 'pickle':
			with open(art[1], "rb") as f:
				result = cloudpickle.load(f)
		elif art[0] == 'DictSkims':
			with open(art[1], "rb") as f:
				result = cloudpickle.load(f)
				from .skims_handler import _load_skims_step_2
				_load_skims_step_2(result, self)
		else:
			raise TypeError(art[0])
		self._cache[key] = result
		return result

	def __getattr__(self, item):
		try:
			return self[item]
		except:
			raise AttributeError(item)

	@property
	def n_internal_zones(self):
		return len(self.m01)

