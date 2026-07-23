import numpy as np
import pandas as pd
import os
from ..addict import Dict
import larch as larch
from larch import P,X
from larch.util.figures import distribution_figure, share_figure
from larch.util.data_expansion import piecewise_linear
from IPython.display import display, HTML

import logging
log = logging.getLogger('CMAP')

def L(*args):
	if len(args)==1 and isinstance(args[0], str) and args[0][0]=="#":
		log.info(args[0])
	else:
		s = "\n".join(str(i) for i in args)
		s = "\n"+s
		log.info(s.replace("\n","\n    "))


from ..transit_approach import transit_approach
from .est_data import dh
from ..choice_model import model_builder

from .est_config import mode_modeled, n_sampled_dests
from .est_survey import trips
from .est_sample_dest import sample_dest_zones_and_data
from ..util import resource_usage
from ..modecodes import mode9names


L(
	"###### data statistics ######",
	trips['tripPurpose'].value_counts(),
	trips.mode3.value_counts()
)

L("###### sample_dest_zones_and_data ######")

# Null out invalid data # now in survey_data
# transit_cols = ['ivtt','ovtt','headway','fare','firstmode','lastmode']
# for a in transit_cols:
#     to_nan = trips[f'transit_{a}'] > 999
#     trips.loc[to_nan,f'transit_{a}'] = np.nan


L("## ae_approach_los ##")
from .est_survey import ae_approach_los

trip_approach_distances, ae_los = ae_approach_los(trips)

L("## sample_dest_zones_and_data ##")
TRIP_ALTS_CACHE_FILE = f"trip_alts_n{n_sampled_dests}_vx14"

trip_alt_df = dh.filenames.load(TRIP_ALTS_CACHE_FILE)
if trip_alt_df is None:
	trip_alt_df = sample_dest_zones_and_data(
		trips,
		n_zones=dh.n_internal_zones,
		n_sampled_dests=n_sampled_dests,
		keep_trips_cols=[
			'd_zone',
			'mode5',
			'mode5code',
			'mode7',
			'mode7code',
			'mode9',
			'mode9code',
			'incomeLevel',
			'tripCat',
			'tripPurpose',
			'auto_dist',
			'auto_time',
			'auto_parking_cost',
			'transit_fare',
			'transit_ivtt',
			'transit_ovtt',
			'transit_headway',
			'transit_approach_cost',
			'transit_approach_drivetime',
			'transit_approach_walktime',
			'transit_approach_waittime',
			'taxi_wait_time',
			'taxi_fare',
			'tnc_solo_wait_time',
			'tnc_pool_wait_time',
			'tnc_solo_fare',
			'tnc_pool_fare',
			'hhinc_dollars',
			'timeperiod',
			'mode_and_time',
			'actualdest',
			'ozone_areatype',
			'ozone_autopropensity',
			'autopropensity',
			'areatype',
			'hhveh',
			'hhsize',
			'hhadults',
			'paFlip',
			'auto_time_hov',
			'auto_toll',
			'auto_toll_hiinc',
			'auto_toll_loinc',
			'auto_toll_hov_loinc',
			'auto_toll_hov_hiinc',
			'auto_opcost',
			'auto_opcost_hov',
		]
	)

	#
	# display(HTML(f"<h4>auto_dist statistics</h4>"))
	# display(trip_alt_df['auto_dist'].statistics())
	#
	# display(HTML(f"<h4>altdest0001_auto_dist statistics</h4>"))
	# display(trip_alt_df['altdest0001_auto_dist'].statistics())
	#


	L("## invalid_walktime ##")
	for peaky in ['PEAK', 'OFFPEAK']:
		invalid_walktime = trip_alt_df[f'actualdest_transit_approach_walktime_{peaky}'] > 180
		trip_alt_df.loc[invalid_walktime, f'actualdest_transit_approach_walktime_{peaky}'] = np.nan

	L("## invalid_drivetime ##")
	for peaky in ['PEAK', 'OFFPEAK']:
		invalid_drivetime = trip_alt_df[f'actualdest_transit_approach_drivetime_{peaky}'] > 180
		trip_alt_df.loc[invalid_drivetime, f'actualdest_transit_approach_drivetime_{peaky}'] = np.nan

	for i in range(n_sampled_dests):
		for peaky in ['PEAK', 'OFFPEAK']:
			L(f"# approach simulate for altdest{i + 1:04d} {peaky} ")
			# q = (trips.tripPurpose == purpose)
			# _trips_by_purpose = trip_alt_df[q]
			result_purpose = transit_approach(
				dh,
				trip_alt_df.o_zone,
				trip_alt_df[f'altdest{i + 1:04d}'],
				'HW' if peaky=='PEAK' else 'HO',
				replication=1,
				approach_distances=None,
				trace=False,
				random_state=123 + i,
			)
			for key in ['drivetime', 'walktime', 'cost', 'waittime']:
				v = result_purpose[key][:, 0].astype(float)
				if key in ['drivetime', 'walktime', 'waittime']:
					v[v > 180] = np.nan
				trip_alt_df.loc[:, f'altdest{i + 1:04d}_transit_approach_{key}_{peaky}'] = v

	dh.filenames.save(TRIP_ALTS_CACHE_FILE, trip_alt_df)

from ..choice_model import alt_codes_and_names

alt_codes, alt_names = alt_codes_and_names(
	n_sampled_dests=n_sampled_dests,
	include_actual_dest=True,
)


dats = Dict()
mods = Dict()
mods_preload = Dict()

from ..purposes import purposes5, purposes_to_peaky
purposes = purposes5


altdest_tags = lambda suffix: [
	f'altdest{i + 1:04d}_{suffix}'
	for i in range(n_sampled_dests)
]

altdest_tags_div = lambda suffix1, suffix2: [
	f'altdest{i + 1:04d}_{suffix1}/altdest{i + 1:04d}_{suffix2}'
	for i in range(n_sampled_dests)
]


# `ca_folds` defines how ca_folded (see below) is built
ca_folds = {
	"nAttractions_HBWH": ['actualdest_log_attractions_HBWH'] + altdest_tags("log_attractions_HBWH"),
	"nAttractions_HBWL": ['actualdest_log_attractions_HBWL'] + altdest_tags("log_attractions_HBWL"),
	"nAttractions_HBS": ['actualdest_log_attractions_HBS'] + altdest_tags("log_attractions_HBS"),
	"nAttractions_HBO": ['actualdest_log_attractions_HBO'] + altdest_tags("log_attractions_HBO"),
	"nAttractions_NHB": ['actualdest_log_attractions_NHB'] + altdest_tags("log_attractions_NHB"),
	"auto_dist_OFFPEAK": ['actualdest_auto_dist_OFFPEAK'] + altdest_tags("auto_dist_OFFPEAK"),
	"auto_dist_PEAK": ['actualdest_auto_dist_PEAK'] + altdest_tags("auto_dist_PEAK"),
	"auto_time_OFFPEAK": ['actualdest_auto_time_OFFPEAK'] + altdest_tags("auto_time_OFFPEAK"),
	"auto_time_PEAK": ['actualdest_auto_time_PEAK'] + altdest_tags("auto_time_PEAK"),
	"transit_ivtt_PEAK": ['actualdest_transit_ivtt_PEAK'] + altdest_tags("transit_ivtt_PEAK"),
	"transit_ivtt_OFFPEAK": ['actualdest_transit_ivtt_OFFPEAK'] + altdest_tags("transit_ivtt_OFFPEAK"),
	"transit_ovtt_OFFPEAK": ['actualdest_transit_ovtt_OFFPEAK'] + altdest_tags("transit_ovtt_OFFPEAK"),
	"transit_approach_waittime_OFFPEAK": ['actualdest_transit_approach_waittime_OFFPEAK'] + altdest_tags("transit_approach_waittime_OFFPEAK"),
	"transit_approach_walktime_OFFPEAK": ['actualdest_transit_approach_walktime_OFFPEAK'] + altdest_tags("transit_approach_walktime_OFFPEAK"),
}

# TODO check data on transit_approach_waittime_OFFPEAK

# `ca_folded` will contain, by purpose, destination-specific data for later analysis
ca_folded = Dict()

cached_model_filename = lambda purpose: dh.filenames.cache_dir / f"choicemodel_{purpose}_hc.xlsx"
cached_model_filereport = lambda purpose: dh.filenames.cache_dir / f"choicemodel_{purpose}_hc_report.xlsx"

for purpose in purposes:

	# define filter for this trip purpose
	q = (trip_alt_df['tripCat'] == purpose)

	# assemble quantitative (size) factors
	#   We use the attractions defined in the model's Trip Generation step.
	log.debug(f"size_of_altdests for {purpose}")
	size_of_altdests = [
		np.log(np.fmax(
			dh.trip_attractions5.loc[
				trip_alt_df[q][f"altdest{i + 1:04d}"],
				purpose,
			].reset_index(
				drop=True
			),
			1e-300,  # nonzero but tiny
		)).rename(
			f'altdest{i + 1:04d}_log_attractions_{purpose}'
		)
		for i in range(n_sampled_dests)
	]

	log.debug(f"actualdest_log_attractions for {purpose}")
	_df = pd.concat(
		[
			trip_alt_df[q].reset_index(),
			np.log(np.fmax(
				dh.trip_attractions5.loc[
					trips[q].d_zone,
					purpose,
				].reset_index(
					drop=True
				),
				1e-300,  # nonzero but tiny
			)).rename(
				f'actualdest_log_attractions_{purpose}'
			),
		] + size_of_altdests,
		axis=1,
	)

	peaky = 'PEAK' if 'W' in purpose else 'OFFPEAK'

	positive_attractions = (_df[f'actualdest_log_attractions_{purpose}'] > -666)
	_df[f'actualdest_transit_avail_{purpose}'] = (
			(_df[f'actualdest_transit_ivtt_{peaky}'] < 999)
			& (_df[f'actualdest_transit_approach_walktime_{peaky}'] < 999)
			& (_df[f'actualdest_transit_approach_drivetime_{peaky}'] < 999)
			& positive_attractions
	)
	_df[f'actualdest_auto_avail_{purpose}'] = positive_attractions
	_df[f'actualdest_walk_avail_{purpose}'] = positive_attractions & (_df[f'actualdest_auto_dist_OFFPEAK'] < 3)
	_df[f'actualdest_bike_avail_{purpose}'] = positive_attractions & (_df[f'actualdest_auto_dist_OFFPEAK'] < 12)
	for i in range(n_sampled_dests):
		positive_attractions = (_df[f'altdest{i + 1:04d}_log_attractions_{purpose}'] > -666)
		_df[f'altdest{i + 1:04d}_transit_avail_{purpose}'] = (
			(  _df[f'altdest{i + 1:04d}_transit_ivtt_{peaky}'] < 999)
			& (_df[f'altdest{i + 1:04d}_transit_ovtt_{peaky}'] < 999)
			& (_df[f'altdest{i + 1:04d}_transit_approach_walktime_{peaky}'] < 999)
			& (_df[f'altdest{i + 1:04d}_transit_approach_drivetime_{peaky}'] < 999)
			& positive_attractions
		)
		_df[f'altdest{i + 1:04d}_auto_avail_{purpose}'] = positive_attractions
		_df[f'altdest{i + 1:04d}_walk_avail_{purpose}'] = positive_attractions & (_df[f'altdest{i + 1:04d}_auto_dist_OFFPEAK'] < 3)
		_df[f'altdest{i + 1:04d}_bike_avail_{purpose}'] = positive_attractions & (_df[f'altdest{i + 1:04d}_auto_dist_OFFPEAK'] < 12)
	# Build IDCA folded data for later analysis
	_ca_folded = {}
	for k, v in ca_folds.items():
		if 'nAttractions' in k:
			if purpose not in k:
				continue
			folder = np.exp(_df[v])
		else:
			folder = _df[v]
		folder.columns = range(len(v))
		_ca_folded[k] = folder.stack().rename(k)
	ca_folded[purpose] = pd.DataFrame(_ca_folded)

	ca_folded[purpose]['walktime_dist'] = (
			ca_folded[purpose]['transit_approach_walktime_OFFPEAK']
			/ ca_folded[purpose]['auto_dist_OFFPEAK']
	)

	# Model's actual data
	dfs = dats[purpose] = larch.DataFrames(
		co=_df,
		alt_codes=alt_codes,
		alt_names=alt_names,
		ch='mode9code',
		# av=pd.DataFrame({
		# 	k: _df.eval(v)
		# 	for k, v in av.items()
		# }).astype(np.int8),
	)

	cached_model_file = cached_model_filename(purpose)
	if 0 and os.path.exists(cached_model_file) and purpose != "NHB":
		m = mods[purpose] = larch.Model.load(
			cached_model_file
		)
		mods_preload[purpose] = True
	else:
		m = mods[purpose] = model_builder(
			purpose,
			include_actual_dest=True,
			n_sampled_dests=n_sampled_dests,
			parameter_values=None,
			constraints=True,
		)
		mods_preload[purpose] = False
	m.dataservice = dfs
	m.load_data()
	m.diagnosis = m.doctor(repair_ch_av="-")




L("## model parameter estimation ##")

Pr = Dict()

nests_per_dest = 3 # CHANGE ME if/when the number of nests per destination is altered in `model_builder`



figures = Dict()

def dest_profiler(
	purpose,
):
	n_modes = len(mode9names)
	_offset = (n_sampled_dests + 1) * n_modes + nests_per_dest-1
	_ch = mods[purpose].dataframes.data_ch_cascade(mods[purpose].graph)\
		      .iloc[:, _offset:-1:nests_per_dest].stack().values
	_av = mods[purpose].dataframes.data_av_cascade(mods[purpose].graph)\
		      .iloc[:, _offset:-1:nests_per_dest].stack().values

	if purposes_to_peaky[purpose]:

		figdef = Dict()
		figdef[f'auto_dist_PEAK'].bins = 50
		figdef[f'auto_dist_PEAK'].range = (0, 50)
		# figdef[f'auto_dist_{peaky}'].xscale = {'value':'symlog', 'linthresh':2, 'linscale':0.3}
		# figdef[f'auto_dist_{peaky}'].xmajorticks = [0, 1, 2, 5, 10, 20, 50]
		# figdef[f'auto_dist_{peaky}'].xminorticks = np.concatenate([
		# 	np.arange(0, 10),
		# 	np.arange(10, 20, 2),
		# 	np.arange(20, 50, 3),
		# ])

		figdef[f'auto_time_PEAK'].bins = 90
		figdef[f'auto_time_PEAK'].range = (0, 90)
		# figdef[f'auto_time_{peaky}'].xscale = {'value':'symlog', 'linthresh':2, 'linscale':0.3}
		# figdef[f'auto_time_{peaky}'].xmajorticks = [0, 1, 2, 5, 10, 20, 50]
		# figdef[f'auto_time_{peaky}'].xminorticks = np.concatenate([
		# 	np.arange(0, 10),
		# 	np.arange(10, 20, 2),
		# 	np.arange(20, 50, 3),
		# 	np.arange(50, 90, 5),
		# ])
	else:
		figdef = Dict()
		figdef[f'auto_dist_OFFPEAK'].bins = 50
		figdef[f'auto_dist_OFFPEAK'].range = (0, 50)
		figdef[f'auto_time_OFFPEAK'].bins = 60
		figdef[f'auto_time_OFFPEAK'].range = (0, 60)

	for x in figdef:
		tag = x.replace("_OFFPEAK","").replace("_PEAK","")
		figures.distribution[purpose][tag] = distribution_figure(
			ca_folded[purpose][x],
			probability=Pr.ByDest[purpose].stack().values,
			choices=_ch,
			availability=_av,
			xlabel=None,
			ylabel='Relative Frequency',
			style='hist',
			bins=figdef[x].bins,
			pct_bins=20,
			range=figdef[x].range,
			prob_label="Modeled",
			obs_label="Observed",
			bw_method=None,
			discrete=None,
			ax=None,
			format='png',
			#header=f"{purpose} / {x} Distribution",
			accumulator=True,
			xscale=figdef[x].xscale or None,
			xmajorticks=figdef[x].xmajorticks or None,
			xminorticks=figdef[x].xminorticks or None,
			coincidence_ratio=True,
		)
		display(HTML(f"<h4>{mods[purpose].title} Destinations by {tag}</h4>"))
		display(figures.distribution[purpose][tag])




def transit_profiler_X(
	purpose,
):
	n_modes = len(mode9names)
	_offset = (n_sampled_dests + 1) * n_modes + nests_per_dest-1
	_ch = mods[purpose].dataframes.data_ch_cascade(mods[purpose].graph)\
		      .iloc[:, :(n_sampled_dests + 1) * n_modes]
	_ch.columns = pd.MultiIndex.from_product([
		np.arange(n_sampled_dests + 1),
		mode9names,
	], names=['dest', 'mode'])
	_ch = _ch.xs("TRANSIT", axis=1, level="mode").stack().values

	_av = mods[purpose].dataframes.data_av_cascade(mods[purpose].graph)\
		      .iloc[:, :(n_sampled_dests + 1) * n_modes]
	_av.columns = pd.MultiIndex.from_product([
		np.arange(n_sampled_dests + 1),
		mode9names,
	], names=['dest', 'mode'])
	_av = _av.xs("TRANSIT", axis=1, level="mode").stack().values

	if purposes_to_peaky[purpose]:

		figdef = Dict()
		figdef[f'auto_dist_PEAK'].bins = 50
		figdef[f'auto_dist_PEAK'].range = (0, 50)
		figdef[f'transit_ivtt_PEAK'].bins = int(120/5)
		figdef[f'transit_ivtt_PEAK'].range = (0, 120)
	else:
		figdef = Dict()
		figdef[f'auto_dist_OFFPEAK'].bins = 30
		figdef[f'auto_dist_OFFPEAK'].range = (0, 30)
		figdef[f'transit_ivtt_OFFPEAK'].bins = int(120/5)
		figdef[f'transit_ivtt_OFFPEAK'].range = (0, 120)

	for x in figdef:
		tag = x.replace("_OFFPEAK","").replace("_PEAK","")
		figures.transit_dist[purpose][tag] = distribution_figure(
			ca_folded[purpose][x],
			probability=Pr.ByMode[purpose].xs("TRANSIT", axis=1, level="mode").stack().values,
			choices=_ch,
			availability=_av,
			xlabel=None,
			ylabel='Relative Frequency',
			style='hist',
			bins=figdef[x].bins,
			pct_bins=20,
			range=figdef[x].range,
			prob_label="Modeled",
			obs_label="Observed",
			bw_method=None,
			discrete=None,
			ax=None,
			format='png',
			#header=f"{purpose} / {x} Distribution",
			accumulator=True,
			xscale=figdef[x].xscale or None,
			xmajorticks=figdef[x].xmajorticks or None,
			xminorticks=figdef[x].xminorticks or None,
			coincidence_ratio=True,
		)
		display(HTML(f"<h4>{mods[purpose].title} Transit Usage by {tag}</h4>"))
		display(figures.transit_dist[purpose][tag])




def mode_usage_profiler(
	purpose,
	modename,
	figdef,
):
	n_modes = len(mode9names)
	_offset = (n_sampled_dests + 1) * n_modes + nests_per_dest-1
	_ch = mods[purpose].dataframes.data_ch_cascade(mods[purpose].graph)\
		      .iloc[:, :(n_sampled_dests + 1) * n_modes]
	_ch.columns = pd.MultiIndex.from_product([
		np.arange(n_sampled_dests + 1),
		mode9names,
	], names=['dest', 'mode'])
	_ch = _ch.xs(modename, axis=1, level="mode").stack().values

	_av = mods[purpose].dataframes.data_av_cascade(mods[purpose].graph)\
		      .iloc[:, :(n_sampled_dests + 1) * n_modes]
	_av.columns = pd.MultiIndex.from_product([
		np.arange(n_sampled_dests + 1),
		mode9names,
	], names=['dest', 'mode'])
	_av = _av.xs(modename, axis=1, level="mode").stack().values

	for x in figdef:
		tag = x.replace("_OFFPEAK","").replace("_PEAK","")
		figures[f'{modename}_dist'][purpose][tag] = distribution_figure(
			ca_folded[purpose][x],
			probability=Pr.ByMode[purpose].xs(modename, axis=1, level="mode").stack().values,
			choices=_ch,
			availability=_av,
			xlabel=None,
			ylabel='Relative Frequency',
			style='hist',
			bins=figdef[x].bins,
			pct_bins=20,
			range=figdef[x].range,
			prob_label="Modeled",
			obs_label="Observed",
			bw_method=None,
			discrete=None,
			ax=None,
			format='png',
			#header=f"{purpose} / {x} Distribution",
			accumulator=True,
			xscale=figdef[x].xscale or None,
			xmajorticks=figdef[x].xmajorticks or None,
			xminorticks=figdef[x].xminorticks or None,
			coincidence_ratio=True,
		)
		display(HTML(f"<h4>{mods[purpose].title} {modename} Usage by {tag}</h4>"))
		display(figures[f'{modename}_dist'][purpose][tag])


def walk_profiler(purpose):
	figdef = Dict()
	figdef[f'auto_dist_OFFPEAK'].bins = 20
	figdef[f'auto_dist_OFFPEAK'].range = (0, 5)
	return mode_usage_profiler(purpose, "WALK", figdef)

def transit_profiler(purpose):
	figdef = Dict()
	if purposes_to_peaky[purpose]:
		figdef[f'auto_dist_PEAK'].bins = 50
		figdef[f'auto_dist_PEAK'].range = (0, 50)
		figdef[f'transit_ivtt_PEAK'].bins = int(120/5)
		figdef[f'transit_ivtt_PEAK'].range = (0, 120)
	else:
		figdef[f'auto_dist_OFFPEAK'].bins = 30
		figdef[f'auto_dist_OFFPEAK'].range = (0, 30)
		figdef[f'transit_ivtt_OFFPEAK'].bins = int(120/5)
		figdef[f'transit_ivtt_OFFPEAK'].range = (0, 120)
	return mode_usage_profiler(purpose, "TRANSIT", figdef)



def value_of_time_profiler(purpose):
	m = mods[purpose]
	time_params = [
		'ivtt',
		'totaltime',
		'auto_time',
		'tnc_time',
		'transit_ivtt',
		'transit_ovtt',
		'ovtt',
		'walk_time',
		'bike_time',
		'walk_time[1]: up to 0.5',
		'walk_time[2]: 0.5 to 1.0',
		'walk_time[3]: over 1.0',
	]
	result = {}
	for t in time_params:
		if t in m:
			result[t] = ((P(t) * 60) / (P.cost)).set_fmt("${:.2f}/hr").string(m)
	result = pd.DataFrame(pd.Series(result, name='Value of Time'))
	figures.vot[purpose] = result
	display(result)

def mode_share_profiler(
		purpose,
):
	d_codes = np.arange(n_sampled_dests+1)

	_pr = Pr.ByMode[purpose].stack(0)
	# _pr = _pr.stack([0,2]).sum(1).unstack()

	_ch = mods[purpose].dataframes.data_ch.copy()
	_ch.columns=pd.MultiIndex.from_product([
		d_codes,
		mode9names,
	])
	_ch = _ch.stack(0)
	# _ch = _ch.stack([0,2]).sum(1).unstack()

	peaky = 'PEAK' if purposes_to_peaky[purpose] else 'OFFPEAK'

	figdef = Dict()
	figdef[f'auto_dist_{peaky}'].bins = np.logspace(np.log10(1),np.log10(51),10)-1
	# np.concatenate([
	# 	np.arange(0, 10, 1),  # first 1 mile bins to 10 miles
	# 	np.arange(10, 20, 2), # then 2 mile bins to 20 miles
	# 	np.arange(20, 51, 5), # then 5 mile bins to 50 miles
	# ])

	for x in figdef:
		tag = x.replace("_OFFPEAK","").replace("_PEAK","")
		figures.share[purpose][tag] = share_figure(
			x=ca_folded[purpose][x],
			probability=_pr.fillna(0),
			choices=_ch,
			style='stacked',
			bins=figdef[x].bins,
			format='png',
			#header=f"{purpose} Mode Share by {x}"
			xscale={'value':'symlog', 'linthresh':2, 'linscale':0.5},
			xmajorticks=[0, 1, 2, 5, 10, 20, 50],
			xminorticks=np.concatenate([
				np.arange(0, 10),
				np.arange(10, 20, 2),
				np.arange(20, 50, 3),
			]),
		)
		display(HTML(f"<h4>{mods[purpose].title} Mode Shares by {tag}</h4>"))
		display(figures.share[purpose][tag])



def mode_choice_summary(m):
	ch_av_summary = m.dataframes.choice_avail_summary().iloc[:-1]
	for k in ['available', 'available weighted', 'available unweighted']:
		try:
			ch_av_summary[k] = ch_av_summary[k].astype(int)
		except KeyError:
			pass
	pr_summary = m.probability(return_dataframe=True).sum()
	ch_av_summary['model prob'] = pr_summary
	ch_av_summary.index = pd.MultiIndex.from_product(
		[
			np.arange(n_sampled_dests + 1),
			mode9names,
		],
		names=['dest', 'mode'],
	)
	result = ch_av_summary.groupby('mode').sum()
	display(HTML(f"<h4>{m.title} Mode Choices Summary</h4>"))
	display(result)
	return result

def estimation(n_mu_dest_search_grids=5, n_mu_car_search_grids=5, purposes=None, force_reestimate=False):

	if purposes is None:
		from ..purposes import purposes5
		purposes = purposes5

	for purpose, m in mods.items():

		if purposes is not None and purpose not in purposes: continue

		m.dataframes.autoscale_weights()
		display(HTML(f"<h3>{m.title}</h3>"))
		if not mods_preload[purpose] or force_reestimate:
			holdfast_cache = m.pf.holdfast.copy()
			minimums_cache = m.pf.minimum.copy()
			maximums_cache = m.pf.maximum.copy()
			# GRID SEARCH on MU
			r = None

			n_search_grids = n_mu_dest_search_grids*n_mu_car_search_grids
			search_grid_num = 1

			for mu_Dest in np.linspace(0.25, 1.0, n_mu_dest_search_grids):
				for mu_HiredCar in np.linspace(0.05, mu_Dest*0.75, n_mu_car_search_grids): # force some mu on hired car
					m.set_values("init")
					m.lock_value("Mu-Dest", mu_Dest)
					m.lock_value("Mu-HiredCar", mu_HiredCar)
					m.lock_value("Mu-PrivateCar", mu_HiredCar)
					r = m.maximize_loglike(
						prior_result=r,
						return_dashboard=True,
						method='SLSQP',
						options={'maxiter': 1000},
						bhhh_start=5,
						iteration_number_tail=f" Grid Search {search_grid_num}/{n_search_grids}"
					)
					search_grid_num += 1
			m.pf['holdfast'] = holdfast_cache
			m.pf['minimum'] = minimums_cache
			m.pf['maximum'] = maximums_cache
			m.set_values("best", respect_holdfast=False)
			# small perturbation to encourage a little movement
			log.info(f"loglike currently {r.get('loglike', 'missing')}")

			m.set_value('Const_WALK', m['Const_WALK'].value * 0.5)
			m.set_value('Const_Transit', m['Const_Transit'].value * 0.5)
			m.set_value('Const_HOV2', m['Const_HOV2'].value * 0.5)
			m.set_value('Const_HOV3', m['Const_HOV3'].value * 0.5)
			m.set_value('Const_TAXI', m['Const_TAXI'].value * 0.5)
			m.set_value('Const_TNC1', m['Const_TNC1'].value * 0.5)
			m.set_value('Const_TNC2', m['Const_TNC2'].value * 0.5)

			log.info(f"loglike perturbed {m.loglike()}")


			log.info(f"Unlocked Estimation from iteration number {r.get('iteration_number', 'NO ITER NUM')}")
			# final maximize from grid search best, probably minimal improvement from here
			r = m.maximize_loglike(
				prior_result=r,
				return_dashboard=True,
				method='SLSQP',
				options={'maxiter': 1000},
			)
			log.info(f"Final Estimation at iteration number {r.get('iteration_number', 'NO ITER NUM')}")

			# # holdfast_cache = mods.HBO.pf.holdfast.copy()
			# # mu_params = [i for i in mods.HBO.pf.index if 'Mu-' in i]
			# # m.pf.loc[mu_params, 'holdfast'] = 1
			# # m.constraint_intensity = 100.0
			# # r = m.maximize_loglike(method='SLSQP', options={'maxiter': 20}, bhhh_start=5, return_dashboard=True)
			# # m.pf['holdfast'] = holdfast_cache
			# # m.constraint_intensity = 0.0
			# m.maximize_loglike(
			# 	method='SLSQP',
			# 	options={'maxiter':1000},
			# 	#prior_result=r,
			# 	bhhh_start=5,
			# )
			m.calculate_parameter_covariance()
			m.to_xlsx(
				cached_model_filename(purpose)
			)
			summary = m.most_recent_estimation_result.copy()
			summary.pop('x', None)
			summary.pop('d_loglike', None)
			summary.pop('nit', None)
			display(larch.util.dictx(summary).__xml__())
			display(m.estimation_statistics())
		_pr = m.probability(return_dataframe='names', include_nests=True)
		n_elemental_alts = (n_sampled_dests+1)*len(mode9names)
		Pr.ByDest[purpose] = _pr.iloc[:,n_elemental_alts+nests_per_dest-1:-1:nests_per_dest]
		Pr.ByMode[purpose] = _pr.iloc[:,:n_elemental_alts]
		Pr.ByMode[purpose].columns = pd.MultiIndex.from_product([
			np.arange(n_sampled_dests+1),
			mode9names,
		], names=['dest','mode'])

		display(HTML(f"<h2>{m.title}</h2>"))
		display(m.parameter_summary())

		try:
			mode_choice_summary_table = mode_choice_summary(m)
		except:
			log.exception("exception in mode_choice_summary")
			mode_choice_summary_success = False
		else:
			mode_choice_summary_success = True

		try:
			dest_profiler(purpose)
		except:
			log.exception("exception in dest_profiler")
			dest_profiler_success = False
		else:
			dest_profiler_success = True

		try:
			mode_share_profiler(purpose)
		except:
			log.exception("exception in mode_share_profiler")
			mode_share_profiler_success = False
		else:
			mode_share_profiler_success = True

		try:
			transit_profiler(purpose)
		except:
			log.exception("exception in transit_profiler")
			transit_profiler_success = False
		else:
			transit_profiler_success = True

		try:
			walk_profiler(purpose)
		except:
			log.exception("exception in walk_profiler")
			walk_profiler_success = False
		else:
			walk_profiler_success = True

		try:
			value_of_time_profiler(purpose)
		except:
			log.exception("exception in value_of_time_profiler")
			value_of_time_profiler_success = False
		else:
			value_of_time_profiler_success = True

		xl = m.to_xlsx(
			cached_model_filereport(purpose),
			save_now=False
		)
		try:
			if dest_profiler_success:
				xl.add_content_tab(
					figures.distribution[purpose]['auto_dist'],
					sheetname="Figures",
					heading="Destination Probabilities by Distance",
				)
				xl.add_content_tab(
					figures.distribution[purpose]['auto_time'],
					sheetname="Figures",
					heading="Destination Probabilities by Auto Travel Time",
				)
			if mode_share_profiler_success:
				xl.add_content_tab(
					figures.share[purpose]['auto_dist'],
					sheetname="Figures",
					heading="Mode Choice by Distance",
				)
			if mode_choice_summary_success:
				xl.add_content_tab(
					mode_choice_summary_table,
					sheetname="Applied",
					heading="Mode Choice Summary",
				)
			if transit_profiler_success:
				xl.add_content_tab(
					figures.TRANSIT_dist[purpose]['auto_dist'],
					sheetname="Figures",
					heading="Transit Usage by Distance",
				)
				xl.add_content_tab(
					figures.TRANSIT_dist[purpose]['transit_ivtt'],
					sheetname="Figures",
					heading="Transit Usage by Transit IVTT",
				)
			else:
				log.warning("transit_profiler_success is False")
			if walk_profiler_success:
				xl.add_content_tab(
					figures.WALK_dist[purpose]['auto_dist'],
					sheetname="Figures",
					heading="Total Walking by Distance",
				)
			if value_of_time_profiler_success:
				xl.add_content_tab(
					figures.vot[purpose],
					sheetname="Figures",
					heading="Value of Time",
				)
		finally:
			xl.save()

	with open(dh.filenames.choice_model_param_file, 'w', encoding="utf-8") as cmp_yaml:
		print("---", file=cmp_yaml)
		for purpose in purposes:
			print(f"{purpose}:", file=cmp_yaml)
			for k, v in mods[purpose].pf.value.items():
				if ':' in k:
					k = f'"{k}"'
				print(f'  {k:24s}: {v}', file=cmp_yaml)
			print("", file=cmp_yaml)
		print("...", file=cmp_yaml)

	return mods

resource_usage.check()
L("## est_choice complete ##")
