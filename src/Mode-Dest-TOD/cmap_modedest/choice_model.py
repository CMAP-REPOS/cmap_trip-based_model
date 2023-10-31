import numpy as np
import pandas as pd
import larch
from larch import P,X
from larch.util.data_expansion import piecewise_linear
from .addict import Dict
from .cmap_logging import getLogger
from .modecodes import mode5codes, mode9codes, mode9names

log = getLogger()



def alt_codes_and_names(
		n_sampled_dests=5,
		include_actual_dest=True,
):
	"""
	Generate alternative names and codes for mode-destination model.

	Parameters
	----------
	n_sampled_dests : int
		The number of destinations that will be used. In estimation, this
		is the number of destinations to be sampled. In application, this
		is the total number of destinations, as sampling is not used.
	include_actual_dest : bool
		Whether to include the "actual" destination.  This destination is
		included in estimation, where it is used to populate the actually
		chosen alternative.  In appplication, the "actual" destination is
		not applicable and should be turned off.

	Returns
	-------
	alt_codes : ndarray
	alt_names : list
	"""
	n_modes = len(mode9names)
	alt_codes = np.arange(n_modes * (n_sampled_dests + 1)) + 1
	alt_names = [i for i in mode9names]
	for i in range(n_sampled_dests):
		alt_names.extend([(j + f"d{i + 1:04d}") for j in mode9names])
	if not include_actual_dest:
		alt_codes = alt_codes[n_modes:]
		alt_names = alt_names[n_modes:]
	return alt_codes, alt_names


def model_utility_for_dest(
		m,
		dest_number,
		purpose,
		n_modes,
):
	"""
	Construct some utility functions for the mode and destination model.

	This method constructs the utility functions for all of the modes, for
	a single destination.  It is called multiple times to build the complete
	set of utility functions across all destinations.

	Parameters
	----------
	m : larch.Model
	dest_number : int
		The number of the destination.  In application, this is
		the TAZ index (TAZ ID minus 1).  In estimation, this is
		the sampling slot, or for actual destination, give -1.
	purpose : str
	n_modes : int

	Returns
	-------

	"""
	if dest_number == -1:
		dest_label = "actualdest"
	else:
		dest_label = f'altdest{dest_number + 1:04d}'

	alts_per_dest = n_modes
	utility_destination = (
			+ P("samp_af") * X(f"log(1/{dest_label}_samp_wgt)")
			+ P("log_attraction") * X(f"{dest_label}_log_attractions_{purpose}")
			+ P("intrazonal") * X(f"o_zone == {dest_label}")
			+ piecewise_linear(f"{dest_label}_auto_dist_OFFPEAK", "distance", breaks=[5, 10])
	)
	shift = (dest_number+1) * alts_per_dest
	jAUTO = mode9codes.AUTO + shift
	jHOV2 = mode9codes.HOV2 + shift
	jHOV3 = mode9codes.HOV3 + shift
	jTNC1 = mode9codes.TNC1 + shift
	jTNC2 = mode9codes.TNC2 + shift
	jTAXI = mode9codes.TAXI + shift
	jTRANSIT = mode9codes.TRANSIT + shift
	jWALK = mode9codes.WALK + shift
	jBIKE = mode9codes.BIKE + shift
	peaky = 'PEAK' if 'W' in purpose else 'OFFPEAK'
	peaky_hov = 'hov_PEAK' if 'W' in purpose else 'OFFPEAK'


	m.utility_co[jAUTO] = (
			+ P("cost") * X(f"{dest_label}_auto_opcost_{peaky}") / 100 # cost in dollars
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("cost") * X(f"{dest_label}_auto_parking_cost_{purpose}") / 100 # cost in dollars
			# TODO add walk terminal time cost
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
		) + utility_destination
	if purpose.upper()[-1] == 'H':
		m.utility_co[jAUTO] += P("cost") * X(f"{dest_label}_auto_toll_hiinc_PEAK") / 100 # cost in dollars
	elif purpose.upper()[-1] == 'L':
		m.utility_co[jAUTO] += P("cost") * X(f"{dest_label}_auto_toll_loinc_PEAK") / 100 # cost in dollars
	else:
		m.utility_co[jAUTO] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 100 # cost in dollars
	if purpose.upper() in {'NH', 'NHB'}:
		m.utility_co[jAUTO] += (
			+ P("AUTO_ozone_autopropensity") * X(f"ozone_autopropensity")
			+ P("AUTO_dzone_autopropensity") * X(f"{dest_label}_autopropensity")
		)
	else:
		m.utility_co[jAUTO] += (
			+ P("AUTO_no_veh") * X("hhveh==0")
			+ P("AUTO_sufficient_veh") * X("hhveh>=hhadults")
		)


	m.utility_co[jHOV2] = (
			P.Const_HOV2
			+ P("cost") * X(f"{dest_label}_auto_opcost_{peaky_hov}") * 0.5 / 100 # cost in dollars
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("cost") * 0.5 * X(f"{dest_label}_auto_parking_cost_{purpose}") / 100 # cost in dollars
			# TODO add walk terminal time cost
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
			+ piecewise_linear(f"{dest_label}_auto_dist_OFFPEAK", "HOV2_distance", breaks=[5, 10])
		) + utility_destination
	if purpose.upper()[-1] == 'H':
		m.utility_co[jHOV2] += P("cost") * X(f"{dest_label}_auto_toll_hov_hiinc_PEAK") / 2 / 100 # cost in dollars
	elif purpose.upper()[-1] == 'L':
		m.utility_co[jHOV2] += P("cost") * X(f"{dest_label}_auto_toll_hov_loinc_PEAK") / 2 / 100 # cost in dollars
	else:
		m.utility_co[jHOV2] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 2 / 100 # cost in dollars
	if purpose.upper() in {'NH', 'NHB'}:
		m.utility_co[jHOV2] += (
			+ P("HOV_ozone_autopropensity") * X(f"ozone_autopropensity")
			+ P("HOV_dzone_autopropensity") * X(f"{dest_label}_autopropensity")
		)
	else:
		m.utility_co[jHOV2] += (
			+ P("HOV_no_veh") * X("hhveh==0")
			+ P("HOV_sufficient_veh") * X("hhveh>=hhadults")
		)




	m.utility_co[jHOV3] = (
			P.Const_HOV3
			+ P("cost") * X(f"{dest_label}_auto_opcost_{peaky_hov}") * 0.33 / 100 # cost in dollars
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("cost") * 0.33 * X(f"{dest_label}_auto_parking_cost_{purpose}") / 100 # cost in dollars
			# TODO add walk terminal time cost
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
			+ piecewise_linear(f"{dest_label}_auto_dist_OFFPEAK", "HOV3_distance", breaks=[5, 10])
		) + utility_destination
	if purpose.upper()[-1] == 'H':
		m.utility_co[jHOV3] += P("cost") * X(f"{dest_label}_auto_toll_hov_hiinc_PEAK") / 3 / 100 # cost in dollars
	elif purpose.upper()[-1] == 'L':
		m.utility_co[jHOV3] += P("cost") * X(f"{dest_label}_auto_toll_hov_loinc_PEAK") / 3 / 100 # cost in dollars
	else:
		m.utility_co[jHOV3] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 3 / 100 # cost in dollars
	if purpose.upper() in {'NH', 'NHB'}:
		m.utility_co[jHOV3] += (
			+ P("HOV_ozone_autopropensity") * X(f"ozone_autopropensity")
			+ P("HOV_dzone_autopropensity") * X(f"{dest_label}_autopropensity")
		)
	else:
		m.utility_co[jHOV3] += (
			+ P("HOV_no_veh") * X("hhveh==0")
			+ P("HOV_sufficient_veh") * X("hhveh>=hhadults")
		)
	m.utility_co[jTNC1] = (
			P.Const_TNC1
			+ P("cost") * X(f"{dest_label}_tnc_solo_fare_{peaky}") / 100 # cost in dollars
			+ P("ovtt_dist") * X(f"{dest_label}_tnc_solo_wait_time_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_tnc_solo_wait_time_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
		) + utility_destination
	m.utility_co[jTNC2] = (
			P.Const_TNC2
			+ P("cost") * X(f"{dest_label}_tnc_pool_fare_{peaky}") / 100 # cost in dollars
			+ P("ovtt_dist") * X(f"{dest_label}_tnc_pool_wait_time_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_tnc_pool_wait_time_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
		) + utility_destination
	m.utility_co[jTAXI] = (
			P.Const_TAXI
			+ P("cost") * X(f"{dest_label}_taxi_fare_{peaky}") / 100 # cost in dollars
			+ P("ovtt_dist") * X(f"{dest_label}_taxi_wait_time_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_taxi_wait_time_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_auto_time_{peaky}")
			+ P("unavail") * X(f"1-{dest_label}_auto_avail_{purpose}")
		) + utility_destination

	if purpose.upper()[-1] == 'H':
		m.utility_co[jTNC1] += P("cost") * X(f"{dest_label}_auto_toll_hiinc_PEAK") / 100 # cost in dollars
		m.utility_co[jTNC2] += P("cost") * X(f"{dest_label}_auto_toll_hiinc_PEAK") / 2 / 100 # cost in dollars
		m.utility_co[jTAXI] += P("cost") * X(f"{dest_label}_auto_toll_hiinc_PEAK") / 100 # cost in dollars
	elif purpose.upper()[-1] == 'L':
		m.utility_co[jTNC1] += P("cost") * X(f"{dest_label}_auto_toll_loinc_PEAK") / 100 # cost in dollars
		m.utility_co[jTNC2] += P("cost") * X(f"{dest_label}_auto_toll_loinc_PEAK") / 2 / 100 # cost in dollars
		m.utility_co[jTAXI] += P("cost") * X(f"{dest_label}_auto_toll_loinc_PEAK") / 100 # cost in dollars
	else:
		m.utility_co[jTNC1] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 100 # cost in dollars
		m.utility_co[jTNC2] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 2 / 100 # cost in dollars
		m.utility_co[jTAXI] += P("cost") * X(f"{dest_label}_auto_toll_OFFPEAK") / 100 # cost in dollars


	m.utility_co[jWALK] = (
			P.Const_WALK
			#+ P("walk_time") * X(f"{dest_label}_auto_dist_OFFPEAK") * 20 # minutes per mile
			+ piecewise_linear(f"{dest_label}_auto_dist_OFFPEAK", "walk_time", breaks = [0.5,1.0]) * 20 # minutes per mile
			+ P("walk_intrazonal") * X(f"o_zone == {dest_label}")
			+ P("walk_areatype2") * X(f"fmax(ozone_areatype, {dest_label}_areatype)==2")
			+ P("walk_areatype3") * X(f"fmax(ozone_areatype, {dest_label}_areatype)==3")
			+ P("walk_areatype4") * X(f"fmax(ozone_areatype, {dest_label}_areatype)==4")
		) + utility_destination
	m.utility_co[jBIKE] = (
			P.Const_BIKE
			+ P("bike_time") * X(f"{dest_label}_auto_dist_OFFPEAK") * 5 # minutes per mile
			+ P("bike_intrazonal") * X(f"o_zone == {dest_label}")
		) + utility_destination
	m.utility_co[jTRANSIT] = (
			P.Const_Transit
			+ P("cost") * X(f"{dest_label}_transit_fare_{peaky}") / 100 # cost in dollars
			+ P("totaltime") * X(f"{dest_label}_transit_ovtt_{peaky}")
			+ P("cost") * X(f"{dest_label}_transit_approach_cost_{peaky}") / 100 # cost in dollars
			+ P("totaltime") * X(f"{dest_label}_transit_approach_drivetime_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_transit_approach_walktime_{peaky}")
			+ P("totaltime") * X(f"{dest_label}_transit_approach_waittime_{peaky}")
			+ P("unavail") * X(f"1-{dest_label}_transit_avail_{purpose}")
			+ P("ovtt_dist") * X(f"{dest_label}_transit_ovtt_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("ovtt_dist") * X(f"{dest_label}_transit_approach_walktime_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("ovtt_dist") * X(f"{dest_label}_transit_approach_waittime_{peaky}")/X(f"{dest_label}_auto_dist_{peaky}")
			+ P("transit_intrazonal") * X(f"o_zone == {dest_label}")

		) + utility_destination
	if 'W' not in purpose.upper():
		m.utility_co[jTRANSIT] += (
			+ P("transit_walk_is_short") * X(f"hard_sigmoid({dest_label}_transit_approach_walktime_{peaky}, 4.0, 2.0)")
			+ P("transit_areatype3") * X(f"fmax(ozone_areatype, {dest_label}_areatype)==3")
		)
	else:
		m.utility_co[jTRANSIT] += (
			+ P("transit_areatype2") * X(f"fmin(ozone_areatype, {dest_label}_areatype)==2")
			+ P("transit_areatype3") * X(f"fmin(ozone_areatype, {dest_label}_areatype)==3")
			+ P("transit_areatype4") * X(f"fmin(ozone_areatype, {dest_label}_areatype)==4")
			+ piecewise_linear(f"{dest_label}_auto_dist_PEAK", "trlong_distance", breaks=[5, 10])
		)

	if purpose.upper() in {'NH', 'NHB'}:
		m.utility_co[jTRANSIT] += (
			+ P("totaltime") * X(f"piece({dest_label}_transit_ivtt_{peaky}, None, 20)")
			+ P("ivtt_longtransit") * X(f"piece({dest_label}_transit_ivtt_{peaky}, 20, None)")
		)
	else:
		m.utility_co[jTRANSIT] += (
			+ P("totaltime") * X(f"{dest_label}_transit_ivtt_{peaky}")
		)

	## IMPORTANT be sure to change `nests_per_dest` elsewhere (i.e. estimation code)
	#            if/when the number of nests per destination is altered here

	private_car = m.graph.new_node(
		parameter="Mu-PrivateCar",
		children=[jAUTO, jHOV2, jHOV3],
		name=f"privatecar-{dest_label}",
	)
	hired_car = m.graph.new_node(
		parameter="Mu-HiredCar",
		children=[jTAXI, jTNC1, jTNC2],
		name=f"hiredcar-{dest_label}",
	)
	m.graph.new_node(
		parameter="Mu-Dest",
		children=[private_car, hired_car, jTRANSIT, jWALK, jBIKE],
		name=f"{dest_label}",
	)


def _lock_value(self, name, value, note=None, change_check=True):
	"""
	Set a fixed value for a model parameter.

	Parameters with a fixed value (i.e., with "holdfast" set to 1)
	will not be changed during estimation by the likelihood
	maximization algorithm.

	Parameters
	----------
	name : str
		The name of the parameter to set to a fixed value.
	value : float
		The numerical value to set for the parameter.
	note : str, optional
		A note as to why this parameter is set to a fixed value.
		This will not affect the mathematical treatment of the
		parameter in any way, but may be useful for reporting.
	change_check : bool, default True
		Whether to trigger a check to see if any parameter frame
		values have changed.  Can be set to false to skip this
		check if you know that the values have not changed or want
		to delay this check for later, but this may result in
		problems if the check is needed but not triggered before
		certain other modeling tasks are performed.

	"""
	name = str(name)
	if value == 'null':
		value = self.pf.loc[name, 'nullvalue']
	self.set_value(name, value, holdfast=1, initvalue=value, nullvalue=value, minimum=value, maximum=value)
	if note is not None:
		self._frame.loc[name, 'note'] = note
	if change_check:
		self._check_if_frame_values_changed()


def purpose_peakiness(purpose):
	from .purposes import purposes_to_peaky
	if purposes_to_peaky[purpose]:
		return 'PEAK'
	else:
		return 'OFFPEAK'


def model_choice_availability(purpose, n_sampled_dests, include_actual_dest=False):
	"""
	Build a dictionary that has expressions for availability of each mode to each destination.

	Parameters
	----------
	purpose : str
		The trip purpose, which in turn determines the available skims used.
	n_sampled_dests : int
		The number of destinations that will be used. In estimation, this
		is the number of destinations to be sampled. In application, this
		is the total number of destinations, as sampling is not used.
	include_actual_dest : bool
		Whether to include the "actual" destination.  This destination is
		included in estimation, where it is used to populate the actually
		chosen alternative.  In appplication, the "actual" destination is
		not applicable and should be turned off.

	Returns
	-------
	Dict[int, str]
	"""

	peaky = purpose_peakiness(purpose)
	n_modes = len(mode9codes)

	# Define the alternative availability for each alternative in this model.
	av = {}
	dzone_has_nonzero_attractions = f"actualdest_log_attractions_{purpose} > -666"
	if include_actual_dest:
		av[mode9codes.AUTO] = dzone_has_nonzero_attractions
		av[mode9codes.HOV2] = dzone_has_nonzero_attractions
		av[mode9codes.HOV3] = dzone_has_nonzero_attractions
		av[mode9codes.TNC1] = dzone_has_nonzero_attractions
		av[mode9codes.TNC2] = dzone_has_nonzero_attractions
		av[mode9codes.TAXI] = dzone_has_nonzero_attractions
		av[mode9codes.TRANSIT] = (
			f"(actualdest_transit_ivtt_{peaky} < 999) "
			f"& (actualdest_transit_approach_walktime_{peaky} < 999) "
			f"& (actualdest_transit_approach_drivetime_{peaky} < 999) "
			f"& ({dzone_has_nonzero_attractions})"
		)
		av[mode9codes.WALK] = f"(actualdest_log_attractions_{purpose} > -666)&(actualdest_auto_dist_OFFPEAK < 3)"
		av[mode9codes.BIKE] = f"(actualdest_log_attractions_{purpose} > -666)&(actualdest_auto_dist_OFFPEAK < 12)"
	num = n_modes
	for i in range(n_sampled_dests):
		altdest_has_nonzero_attractions = f"altdest{i + 1:04d}_auto_avail_{purpose}"
		av[num + mode9codes.AUTO] = altdest_has_nonzero_attractions
		av[num + mode9codes.HOV2] = altdest_has_nonzero_attractions
		av[num + mode9codes.HOV3] = altdest_has_nonzero_attractions
		av[num + mode9codes.TNC1] = altdest_has_nonzero_attractions
		av[num + mode9codes.TNC2] = altdest_has_nonzero_attractions
		av[num + mode9codes.TAXI] = altdest_has_nonzero_attractions
		av[num + mode9codes.TRANSIT] = f"altdest{i + 1:04d}_transit_avail_{purpose}"
		av[num + mode9codes.WALK] = f"altdest{i + 1:04d}_walk_avail_{purpose}"
		av[num + mode9codes.BIKE] = f"altdest{i + 1:04d}_bike_avail_{purpose}"
		num += n_modes

	return av


def model_builder(
		purpose,
		include_actual_dest=True,
		n_sampled_dests=5,
		parameter_values=None,
		constraints=True,
		n_threads=-1,
		application_mode=False,
		explicit_av=True,
):
	"""
	Construct a larch Model for mode and destination choice.

	This function creates the structure of the model and sets parameter
	values within that structure if given.  To ensure consistency, the
	same code is used to contruct models for estimation and for application,
	although slight changes in structure are applied based on the arguments
	as documented below.

	Parameters
	----------
	purpose : str
	include_actual_dest : bool
		The "actual" observed destination is included for estimation, but
		not in application.
	n_sampled_dests : int
		For estimation, only a subset of destinations are sampled using a
		weighted importance sampling.  In application, this is set to the
		full number of zones and no sampling weights are applied.
	parameter_values : Mapping
		The values to use for model parameters.  Typically not provided here
		for estimation, but should be provided for application.
	constraints : bool
		Whether to include estimation constraints in the model specification.
		In application, parameters are not changed so constraints are
		unnecessary.
	n_threads : int, default -1
		Number of threads to use for computation.  Set to -1 to use threads for
		all processessor cores.
	application_mode
	explicit_av

	Returns
	-------

	"""


	log.debug(f"model_builder({purpose}, n_sampled_dests={n_sampled_dests})")

	n_modes = len(mode9names)

	alt_codes, alt_names = alt_codes_and_names(
		n_sampled_dests=n_sampled_dests,
		include_actual_dest=include_actual_dest,
	)
	from larch.numba import DataFrames
	dummy_dfs = DataFrames(
		alt_codes=alt_codes,
		alt_names=alt_names,
	)

	if explicit_av:
		av = model_choice_availability(purpose, n_sampled_dests, include_actual_dest)

	import larch.numba
	m = larch.numba.Model(
		dataservice=dummy_dfs,
		n_threads=n_threads,
	)

	m.title = f"{purpose} Mode & Destination"

	if explicit_av:
		m.availability_co_vars = av

	if include_actual_dest:
		model_utility_for_dest(
			m,
			dest_number=-1,
			purpose=purpose,
			n_modes=n_modes,
		)

	for i in range(n_sampled_dests):
		model_utility_for_dest(
			m,
			dest_number=i,
			purpose=purpose,
			n_modes=n_modes,
		)

	m.unmangle()
	_lock_value(m, "samp_af", value=1.0)
	_lock_value(m, "log_attraction", value=1.0)
	_lock_value(m, "unavail", value=-999)

	# initial setting of nesting parameters
	m.set_value("Mu-HiredCar", 0.4)
	m.set_value("Mu-Dest", 0.7)

	m.set_value("cost", maximum=-0.00001)
	# m.set_value("auto_time", maximum=-0.01, minimum=-0.03)
	# m.set_value("tnc_time", maximum=-0.01, minimum=-0.03)
	# m.set_value("transit_ivtt", maximum=-0.01, minimum=-0.03)
	m.set_value("totaltime", maximum=-0.01, minimum=-0.03)
	if "ivtt_longtransit" in m:
		m.set_value("ivtt_longtransit", maximum=-0.0001, minimum=-0.03)
	m.set_value("ovtt_dist", maximum=-0.001)
	if parameter_values is None:
		m.set_values(
			cost=-0.0001,
			# auto_time=-0.01,
			tnc_time=-0.02,
			totaltime=-0.015,
			ovtt_dist=-0.03,
			Const_TNC1=-1.0,
			Const_TNC2=-1.0,
			Const_Transit=-1.0,
			intrazonal=-0.1,
		)
	else:
		m.set_values(**parameter_values)

	if constraints:
		from larch.model.constraints import RatioBound
		if purpose=='HBWL':
			vot_constraint = RatioBound(P("totaltime"), P("cost"), min_ratio=0.0001, max_ratio=0.5, scale=1)
		elif purpose=='HBWH':
			vot_constraint = RatioBound(P("totaltime"), P("cost"), min_ratio=0.5, max_ratio=1.5, scale=1)
		else:
			vot_constraint = RatioBound(P("totaltime"), P("cost"), min_ratio=0.0001, max_ratio=1.0, scale=1)

		m.constraints = [
			# RatioBound(P("ovtt"), P("totaltime"), min_ratio=1.5, max_ratio=3.0, scale=1),
			RatioBound(P("Mu-HiredCar"), P("Mu-Dest"), min_ratio=1e-5, max_ratio=0.75, scale=1),
			RatioBound(P("Mu-PrivateCar"), P("Mu-Dest"), min_ratio=1e-5, max_ratio=0.75, scale=1),
			vot_constraint,
		]

	if application_mode:
		m._preload_tree_structure()

	m.set_cap()
	return m


