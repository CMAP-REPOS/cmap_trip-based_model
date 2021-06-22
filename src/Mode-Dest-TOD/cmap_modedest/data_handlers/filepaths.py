from pathlib import Path
import os
import glob
import time
import pandas as pd
import numpy as np
import yaml
from ..addict import Dict

import logging
log = logging.getLogger('CMAP')

_here = os.path.abspath(os.path.dirname(__file__))
emme_database_dir = os.path.normpath(os.path.join(_here, "../tests/data"))


class PathAttr:

	def __set_name__(self, owner, name):
		self.public_name = name
		self.private_name = '_' + name

	def __init__(self, can_null=True, change_callback=None):
		self.can_null = can_null
		self.change_callback = change_callback

	def __get__(self, obj, objtype):
		return getattr(obj, self.private_name)

	def __set__(self, obj, value):
		if value is None:
			if not self.can_null:
				raise ValueError(f'cannot set {self.private_name[1:]} to None')
		else:
			value = Path(value)
		try:
			prior = getattr(obj, self.private_name)
		except AttributeError:
			self.on_change(obj, value)
		else:
			if prior != value:
				self.on_change(obj, value)
		log.debug(f"setattr {self.private_name} = {value}")
		setattr(obj, self.private_name, value)

	def on_change(self, obj, new_value):
		log.debug(f"on_change {self.public_name}")
		if self.change_callback:
			self.change_callback(obj, new_value)


def _changed_emme_database_dir(filenames, new_db_dir):
	cfgfile = new_db_dir / f"cmap_trip_config.yaml"
	with open(cfgfile, 'r') as f:
		filenames.cfg = Dict(yaml.load(f, Loader=yaml.SafeLoader))

def _make_dirs(filenames, new_dir):
	if new_dir and not os.path.exists(new_dir):
		os.makedirs(new_dir)


class FileNames:

	emme_database_dir = PathAttr(can_null=False, change_callback=_changed_emme_database_dir)
	emmemat_dir = PathAttr(can_null=True)
	#omx_skims_dir = PathAttr()
	cache_dir = PathAttr(change_callback=_make_dirs)
	zone_shapefile = PathAttr()

	def __init__(
			self,
			emme_database_dir,
			#omx_skims_dir=None,
			cache_dir=None,
			emmemat_dir=None,
			#emmemat_archive=None,
			zone_shapefile=None,
	):
		self._memory_mapped_skim = {}
		self.emme_database_dir = emme_database_dir
		#self.omx_skims_dir = omx_skims_dir
		# if emmemat_archive:
		# 	self.use_emmemat_archive(emmemat_archive)
		# else:
		self.emmemat_dir = emmemat_dir
		self.cache_dir = cache_dir
		self.zone_shapefile = zone_shapefile
		self._temporary_dir = None
		if self.choice_model_param_file and os.path.exists(self.choice_model_param_file):
			with open(self.choice_model_param_file, 'r') as f:
				self.choice_model_params = Dict(yaml.load(f, Loader=yaml.SafeLoader))
		else:
			self.choice_model_params = Dict()


	def __getattr__(self, item):
		if item == 'temp_dir':
			if self._temporary_dir is None:
				import tempfile
				self._temporary_dir = tempfile.TemporaryDirectory()
			return Path(self._temporary_dir.name)
		if item[-6:] == "_DISTR":
			return self.emme_database_dir / f"{item}.TXT"
		if item[-4:] == "_M01":
			return self.emme_database_dir / f"{item}.TXT"
		if item[-5:] == "_M023":
			return self.emme_database_dir / f"{item}.TXT"
		if item[-5:] == "_M023":
			return self.emme_database_dir / f"{item}.TXT"
		if item[-9:] == "_CBDPARK0":
			return self.emme_database_dir / f"{item[:-1]}.TXT"
		if item[-8:] == "_CBDPARK":
			return self.cache_dir / f"__{item}.TXT"
		if item[-9:] == "_CBDPARK2":
			return self.cache_dir / f"__{item}.TXT"
		if item[-6:] == "_skims":
			raise NotImplementedError
			# if self.omx_skims_dir:
			# 	return self.omx_skims_dir / f"{item}.omx"
			# else:
			# 	return self.emme_database_dir / f"{item}.omx"
		if item[:2] == "mf":
			if self.emmemat_dir:
				return self.emmemat_dir / f"{item}.emx"
			else:
				return self.emme_database_dir / "emmemat" / f"{item}.emx"
		if item != "_memory_mapped_skim" and item in self._memory_mapped_skim:
			return self._memory_mapped_skim[item]
		if item[:6] == "map_mf":
			if self.emmemat_dir:
				pth = self.emmemat_dir / f"{item[4:]}.emx"
			else:
				pth = self.emme_database_dir / "emmemat" / f"{item[4:]}.emx"
			mapped_skim = np.memmap(pth, dtype='f4', mode='r')
			nzones = int(np.sqrt(mapped_skim.shape[0]))
			mapped_skim = self._memory_mapped_skim[item] = mapped_skim.reshape([nzones,nzones])
			return mapped_skim
		if item == "config":
			return self._emme_database_dir / f"cmap_trip_config.yaml"
		if item == "choice_model_param_file":
			from ..util import search_path
			result = search_path(
				self.emme_database_dir / f"choice_model_params.yaml",
				self.cache_dir / f"choice_model_params.yaml",
			)
			if result is None:
				result = self.cache_dir / f"choice_model_params.yaml"
			return result
		if item == "tod_model_param_file":
			from ..util import search_path
			return search_path(
				self.emme_database_dir / f"tod_model_params.yaml",
				self.cache_dir / f"tod_model_params.yaml",
			)
		if item == "zone_districts":
			from ..util import search_path
			return search_path(
				self.emme_database_dir / f"CMAP_Zone_Districts.csv.gz",
				self.cache_dir / f"CMAP_Zone_Districts.csv.gz",
			)
		if item == "emmemat":
			if self.emmemat_dir:
				return self.emmemat_dir
			else:
				return self.emme_database_dir/"emmemat"
		raise AttributeError(item)

	@property
	def tripgen_sas(self):
		return latest_matching(
			self.emme_database_dir / "tg/sas/data/tg*.sas7bdat"
		) or latest_matching(
			self.emme_database_dir / "tg*.sas7bdat"
		) or latest_matching(
			self.cache_dir / "tg*.sas7bdat"
		)

	@property
	def hh_enum(self):
		return latest_matching(
			self.emme_database_dir / "TG_HHENUM_OUTPUT.TXT.gz"
		) or latest_matching(
			self.cache_dir / "TG_HHENUM_OUTPUT.TXT.gz"
		) or latest_matching(
			self.emme_database_dir / "TG_HHENUM_OUTPUT.TXT"
		) or latest_matching(
			self.cache_dir / "TG_HHENUM_OUTPUT.TXT"
		)

	def save(self, name, data):
		try:
			if isinstance(data, pd.DataFrame):
				if self.cache_dir:
					pth = self.cache_dir / f"{name}.pq"
				else:
					pth = f"{name}.pq"
				data.to_parquet(pth)
			else:
				raise TypeError(str(type(data)))
		except Exception as err:
			import warnings
			warnings.warn(f"failed to save {name}: {err}")

	def load(self, name):
		if self.cache_dir:
			pth = self.cache_dir / f"{name}.pq"
		else:
			pth = f"{name}.pq"
		if os.path.exists(pth):
			return pd.read_parquet(pth)
		# else file does not exist
		return None

	# def use_emmemat_archive(self, archive_file):
	# 	import zipfile
	# 	import tempfile
	# 	self.temp_emmemat_dir = tempfile.TemporaryDirectory()
	# 	with zipfile.ZipFile(archive_file, 'r') as zip_ref:
	# 		zip_ref.extractall(self.temp_emmemat_dir.name)
	# 	self.emmemat_dir = self.temp_emmemat_dir.name



def _insensitive_glob(pattern):
	def either(c):
		return '[%s%s]' % (c.lower(), c.upper()) if c.isalpha() else c
	return ''.join(map(either, pattern))


def latest_matching(pattern, echo=False, case_insensitive=False, expanduser=True):
	"""Get the most recently modified file matching the glob pattern

	Parameters
	----------
	pattern : str
		A glob pattern to match on
	echo : bool, default False
		If true, print the last modified time for each matching file
	case_insensitive : bool, default False
		Tf true, the glob pattern will be modified to be case insensitive.
	expanduser : bool, default true
		Should each pattern be passed through the `os.path.expanduser` function?

	Returns
	-------
	str
		The filename of the most recently modified file matching the glob pattern

	"""
	if expanduser:
		pattern = os.path.expanduser(pattern)
	if case_insensitive:
		pattern = _insensitive_glob(pattern)
	files = glob.glob(pattern)
	propose = None
	propose_mtime = 0
	for file in files:
		(mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = os.stat(file)
		if echo:
			print (file,"last modified: %s" % time.ctime(mtime))
		if mtime > propose_mtime:
			propose_mtime = mtime
			propose = file
	return propose


