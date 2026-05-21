import os.path
import numpy as np
import pandas as pd
import gzip
from ..addict import Dict
from ..util import search_path
import logging



def load_hh_enum(filenames):

	hhv = pd.read_csv(
		filenames.hh_enum,
		header=None,
		names=["subzone", 'zone', 'hhvtype']
	)
	tabulation = (
		hhv
		.groupby(["zone","hhvtype"])
		.size()
		.unstack()
		.reindex(pd.RangeIndex(1, 3633, name='TAZ'))
		.reindex(pd.RangeIndex(1, 625, name='hhvtype'), axis=1)
		.fillna(0)
	)
	return hhv, tabulation



def load_tg(filenames, with_detail=False):


	pa_trip_types = pd.read_csv(
		os.path.join(os.path.dirname(__file__), "pa_trip_types.csv"),
		index_col=0,
	)
	purpose_categories = pa_trip_types.purpose_category

	trip49_filenames = {}
	trip49_filenames['typical']	= search_path(
		filenames.cache_dir / "TRIP49_PA_OUT.TXT.gz",
		filenames.cache_dir / "TRIP49_PA_OUT.TXT",
		filenames.emme_database_dir / "TRIP49_PA_OUT.TXT.gz",
		filenames.emme_database_dir / "TRIP49_PA_OUT.TXT",
		filenames.emme_database_dir / "defaults_base_year/TRIP49_PA_OUT.TXT.gz",
		filenames.emme_database_dir / "defaults_base_year/TRIP49_PA_OUT.TXT",
	)
	# secondary WFH trip generation
	trip49_filenames['wfh']	= search_path(
		filenames.cache_dir / "TRIP49_PA_WFH_OUT.TXT.gz",
		filenames.cache_dir / "TRIP49_PA_WFH_OUT.TXT",
		filenames.emme_database_dir / "TRIP49_PA_WFH_OUT.TXT.gz",
		filenames.emme_database_dir / "TRIP49_PA_WFH_OUT.TXT",
		filenames.emme_database_dir / "defaults_base_year/TRIP49_PA_WFH_OUT.TXT.gz",
		filenames.emme_database_dir / "defaults_base_year/TRIP49_PA_WFH_OUT.TXT",
	)

	log = logging.getLogger('CMAP')
	result = {}
	for tripclass, trip49_filename in trip49_filenames.items():

		log.info(f"reading {tripclass} trip generation from: {trip49_filename}")

		if os.path.splitext(trip49_filename)[1] == '.gz':
			f = gzip.open(trip49_filename, 'rb')
		else:
			f = open(trip49_filename, 'rb')
		try:
			trip49 = pd.read_fwf(
				f,
				widths=[6, 6, 2, 9, 9],
				names=['subzone', 'zone', 'trip_type', 'productions', 'attractions'],
			)
		finally:
			f.close()

		trip49["purpose_category"] = trip49["trip_type"].map(purpose_categories)

		# Certain trip types are NHB but generated as P-A, convert them to O-D
		nhb_pa = (pa_trip_types['purpose_category'].isin(['NHB', 'NHBR', 'NHBS'])) & (pa_trip_types['pair_type'] == 'PA')
		trip49["mix_PA"] = trip49["trip_type"].map(nhb_pa)
		mixed_PA = (trip49['productions'] + trip49['attractions']) / 2
		trip49.loc[trip49["mix_PA"], 'productions'] = mixed_PA.loc[trip49["mix_PA"]]
		trip49.loc[trip49["mix_PA"], 'attractions'] = mixed_PA.loc[trip49["mix_PA"]]

		trip49z = trip49.groupby(["zone", "purpose_category"])[['productions', 'attractions']].sum()
		prodns = trip49z['productions'].unstack().fillna(0.0)
		attrns = trip49z['attractions'].unstack().fillna(0.0)

		if with_detail:
			trip49z_detail = trip49.groupby(["zone", "trip_type"])[['productions', 'attractions']].sum()
			prodns_detail = trip49z_detail['productions'].unstack().fillna(0.0)
			attrns_detail = trip49z_detail['attractions'].unstack().fillna(0.0)

		for xns in (prodns, attrns):
			# There are high and low income work purposes.  We push all the non-income bound
			# home-based mandatory purposes into income-related groups based on the relative
			# share in the income-bound values
			income_generic_mandatory = xns["HBW"]
			lo_income_share_mandatory = (xns["HBWL"] / (xns["HBWL"] + xns["HBWH"])).where((xns["HBWL"] + xns["HBWH"])>0, 0)
			hi_income_share_mandatory = 1 - lo_income_share_mandatory
			xns["HBWL"] += income_generic_mandatory * lo_income_share_mandatory
			xns["HBWH"] += income_generic_mandatory * hi_income_share_mandatory
			xns.drop(columns=['HBW', 'EXCLUDE'], inplace=True)

		pd.concat([prodns.stack().rename("productions"), attrns.stack().rename("attractions")], axis=1).to_csv(
			filenames.cache_dir / "trips_PA_by_purpose.csv"
		)

		tg = Dict()
		tg.trip_attractions8 = attrns.copy()
		tg.trip_productions8 = prodns.copy()
		tg.zone_productions8 = prodns.round().astype(int).copy()

		for xns in (prodns, attrns):
			xns['HBO'] = xns['HBO'] + xns['HBOR']
			xns['NHB'] = xns['NHB'] + xns['NHBR'] + xns['NHBS']
			xns.drop(columns=['HBOR', 'NHBR', 'NHBS'], inplace=True)

		tg.trip_attractions5 = attrns
		tg.trip_productions5 = prodns
		tg.zone_productions5 = prodns.round().astype(int)
		if with_detail:
			tg.trip_attractions_detail = attrns_detail
			tg.trip_productions_detail = prodns_detail
		result[tripclass] = tg

	return result
