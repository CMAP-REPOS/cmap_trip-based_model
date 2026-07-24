import numpy as np
import logging
from .addict import Dict
log = logging.getLogger('CMAP')

from .ae_distance_sim import simulate_ae_dist
from .random_states import check_random_generator

# trip types
HW = 'HW'  # HOME PRODUCTIONS TO WORK ATTRACTIONS
HO = 'HO'  # HOME PRODUCTIONS TO NON-WORK/NON-HOME ATTRACTIONS
NH = 'NH'  # NON-HOME PRODUCTIONS TO NON-HOME ATTRACTIONS

ITER = 50  # NUMBER OF TRIPS USED TO COMPUTE AVERAGE IMPEDANCES

from .modecodes import (
	APPROACH_WALK,
	APPROACH_BUS,
	APPROACH_PARK_N_RIDE,
	APPROACH_KISS_N_RIDE,
	APPROACH_FEEDER_BUS,
	N_APPROACH_MODES,
	APPROACH_MODE_NAMES,
	DIST_TO_BUS,
	DIST_TO_CTA_RAIL,
	DIST_TO_METRA,
	DIST_TO_FEEDER_BUS,
	DIST_TO_PARK_N_RIDE_STATION,
	N_DIST_TO_TYPES,
	TransitModeCode_CTA_RAIL,
	TransitModeCode_METRA_RAIL,
	TransitModeCode_CTA_EXPRESS_BUS,
	TransitModeCode_CTA_REGULAR_BUS,
	TransitModeCode_PACE_BUS,
)



FRONT_END = 0
BACK_END = 1
N_TRIP_ENDS = 2

SPDWLK = 30
# SPDWLK = SYSTEM-WIDE SPEED OF WALKING,
#          DEFAULT IS 30 TENTHS OF A MILE PER HOUR

SPEEDS = np.array([7, 15, 20, 30, 5, 10, 12, 17])
# SPEEDS = SPEEDS OF APPROACH AUTO AND BUS BY ZONE AREA TYPE
#          AUTO APPROACH SPEEDS:
#            ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 7 MPH
#            ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 15 MPH
#            ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 20 MPH
#            ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 30 MPH
#          BUS APPROACH SPEEDS:
#            ZONE TYPE 1 (CHICAGO CBD) DEFAULT IS 5 MPH
#            ZONE TYPE 2 (CHICAGO REMAINDER) DEFAULT IS 10 MPH
#            ZONE TYPE 3 (DENSE SUBURB) DEFAULT IS 12 MPH
#            ZONE TYPE 4 (SPARSE SUBURB) DEFAULT IS 17 MPH

DRVOT = 14
# DRVOT  = DRIVER'S VALUE OF TIME, DEFAULT IS 14 CENTS/MIN

OVT_IVT_RATIO = 2.0



AFC1 = 35  # AUTO FIXED COSTS FOR AUTO DRIVER IN CENTS
AFC2 = 20  # AUTO FIXED COSTS FOR AUTO PASSENGER IN CENTS

#  W2PNR  = WALK TIME TO STATION FROM PARK AND RIDE LOT,
#           DEFAULT IS 2 MINUTES
W2PNR = 2


def _simulate_approach_distances(
		dh,
		zone,
		attached_mode,
		trip_purpose,
		trip_end,
		out,
		random_state=None,
):
	"""

	Parameters
	----------
	dh : DataHandler
	zone : int
		Zone id (1-based)
	attached_mode : int
		Number for first or last mode (as matches this approach)
	trip_purpose : {'HW','HO','NH'}
		Trip purpose, used to select DISTR table and possibly filter
		approach modes
	trip_end : {0,1}
		Zero if approach to first mode, one if approach from last mode
	out : array-like
		Output array must already exist, as a float dtype,
		with shape [replications, N_APPROACH_MODES, ]

	"""
	if not isinstance(zone, int):
		return _simulate_approach_distances_arr(
			dh,
			zone,
			attached_mode,
			trip_purpose,
			trip_end,
			out,
			random_state=random_state,
		)
	random_state = check_random_generator(random_state)
	replication = list(out.shape[:-1])
	distr = dh.distr
	if replication[0] == 1:
		replication = replication[1:]
	for J in range(N_DIST_TO_TYPES):
		# OBTAIN APPROACH DISTANCES TO FIVE MODES
		if (J == DIST_TO_BUS):
			distr_params = distr[trip_purpose].loc[(zone, 'bus')]
		elif (J == DIST_TO_CTA_RAIL):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS CTA RAIL
			if attached_mode == TransitModeCode_CTA_RAIL:
				distr_params = distr[trip_purpose].loc[(zone, 'ctarail')]
			else:
				distr_params = (999,999,999)
		elif (J == DIST_TO_METRA):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
			if attached_mode == TransitModeCode_METRA_RAIL:
				distr_params = distr[trip_purpose].loc[(zone, 'metra')]
			else:
				distr_params = (999, 999, 999)
		elif (J == DIST_TO_FEEDER_BUS):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
			if attached_mode == TransitModeCode_METRA_RAIL:
				distr_params = distr[trip_purpose].loc[(zone, 'feederbus')]
			else:
				distr_params = (999, 999, 999)
		elif (J == DIST_TO_PARK_N_RIDE_STATION):
			# PARK AND RIDE STATION DISTANCE OBTAINED WHEN TRIP END IS FRONT
			if trip_end == FRONT_END:
				distr_params = distr[trip_purpose].loc[(zone, 'pnr')]
			else:
				distr_params = (999, 999, 999)
		else:
			raise ValueError(J)
		if distr_params[2] != 999:
			out[...,J] = simulate_ae_dist(*distr_params, replication=replication, random_state=random_state)
		else:
			out[...,J] = 255.0


def _simulate_approach_distances_arr(
		dh,
		zone,
		attached_mode,
		trip_purpose,
		trip_end,
		out,
		random_state=None,
):
	"""

	Parameters
	----------
	zone : array-like
		Zone id (1-based)
	attached_mode : int
		Number for first or last mode (as matches this approach)
	trip_purpose : {'HW','HO','NH'}
		Trip purpose, used to select DISTR table and possibly filter
		approach modes
	trip_end : {0,1}
		Zero if approach to first mode, one if approach from last mode
	out : array-like
		Output array must already exist, as a float dtype,
		with shape [replications, N_APPROACH_MODES, ]

	"""
	random_state = check_random_generator(random_state)
	replication = list(out.shape[1:-1])
	distr_df = dh.distr[trip_purpose].unstack().loc[zone]
	for J in range(N_DIST_TO_TYPES):
		# OBTAIN APPROACH DISTANCES TO FIVE MODES
		if (J == DIST_TO_BUS):
			distr_params = distr_df.xs('bus', level='submode', axis=1).copy()
		elif (J == DIST_TO_CTA_RAIL):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS CTA RAIL
			distr_params = distr_df.xs('ctarail', level='submode', axis=1).copy()
			distr_params.loc[attached_mode != TransitModeCode_CTA_RAIL,:] = 999
		elif (J == DIST_TO_METRA):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
			distr_params = distr_df.xs('metra', level='submode', axis=1).copy()
			distr_params.loc[attached_mode != TransitModeCode_METRA_RAIL,:] = 999
		elif (J == DIST_TO_FEEDER_BUS):
			# DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
			distr_params = distr_df.xs('feederbus', level='submode', axis=1).copy()
			distr_params.loc[attached_mode != TransitModeCode_METRA_RAIL,:] = 999
		elif (J == DIST_TO_PARK_N_RIDE_STATION):
			# PARK AND RIDE STATION DISTANCE OBTAINED WHEN TRIP END IS FRONT
			if trip_end == FRONT_END:
				distr_params = distr_df.xs('pnr', level='submode', axis=1).copy()
			else:
				distr_params.loc[:,:] = 999
		else:
			raise ValueError(J)
		_temp = simulate_ae_dist(
			distr_params.p1,
			distr_params.p2,
			distr_params.p3,
			replication=replication,
			random_state=random_state,
		)
		out[..., J] = _temp


# def simulate_approach_distances_222(ozone, dzone, firstmode, lastmode, trip_purpose):
# 	DOND = np.full([ITER, N_APPROACH_MODES, N_TRIP_ENDS], 255.)
# 	for I in range(N_TRIP_ENDS):
#
# 		# C     I=1 OBTAIN DISTANCES FOR ORIGIN
# 		# C     I=2 OBTAIN DISTANCES FOR DESTINATION
# 		if I == FRONT_END:
# 			Z=ozone
# 			M=firstmode
# 		else:
# 			Z=dzone
# 			M=lastmode
#
#
# 		for J in range(N_APPROACH_MODES):
# 			# C
# 			# C     OBTAIN APPROACH DISTANCES TO FIVE MODES
# 			DOND[:,J,I]=255.
#
# 			if (J == APPROACH_WALK):
# 				distr_params = distr[trip_purpose].loc[(ozone, 3)]
# 			elif (J == APPROACH_BUS):
# 				distr_params = distr[trip_purpose].loc[(ozone, 2)]
# 			elif (J == APPROACH_PARK_N_RIDE):
# 				distr_params = distr[trip_purpose].loc[(ozone, 1)]
# 			elif (J == APPROACH_KISS_N_RIDE):
# 				distr_params = distr[trip_purpose].loc[(ozone, 4)]
# 			else: # (J == FEEDER_BUS):
# 				distr_params = distr[trip_purpose].loc[(ozone, 5)]
#
# 			DOND[:,J,I] = simulate_ae_dist(*distr_params, replication=ITER)
#
# 	return DOND


def _IS_CTA(m):
	return (
		m == TransitModeCode_CTA_RAIL
		or m == TransitModeCode_CTA_REGULAR_BUS
		or m == TransitModeCode_CTA_EXPRESS_BUS
	)


from numba import jit, njit, prange

def transit_approach(
		dh,
		ozone,
		dzone,
		TPTYPE,
		replication=None,
		approach_distances=None,
		trace=False,
		random_state=None,
):
	"""
	Replaces TRAPP fortran.

	Parameters
	----------
	dh : DataHandler
	ozone, dzone : int or array-like
		Zone ID numbers.  If array-like, should be arrays of the same shape.
	TPTYPE : {'HW', 'HO', 'NH'}
		Trip type
	replication : int
		Number of simulation replications

	Returns
	-------
	Dict
		Containing:
		- drivetime : array of int32, shape [replication]
			simulated in vehicle (drive) approach times, in minutes
		- walktime : array of int32, shape [replication]
			simulated out of vehicle (walk) approach times, in minutes
		- cost : array of int32, shape [replication]
			simulated approach costs, in cents
		- waittime : array of int32, shape [replication]
			simulated approach waiting times
		- approach_mode : array of int8, shape [replication, 2]
			simulated best approach modes
		= approach_distances : array of float32, shape [replication, N_DIST_TO_TYPES, N_TRIP_ENDS]

	Notes
	-----
	When ozone, dzone are given as arrays, all returned arrays have one extra front
	dimension matching these arrays.

	"""
	random_state = check_random_generator(random_state or [ozone, dzone])

	PACE_BUS_BOARDING_FARE = dh.m023.PACE_BUS_BOARDING_FARE
	PACE_BUS_FIRST_XFER_FARE = dh.m023.PACE_BUS_FIRST_XFER_FARE
	FEEDER_BUS_BOARDING_FARE = dh.m023.FEEDER_BUS_BOARDING_FARE
	FEEDER_BUS_CBD_FARE = dh.m023.FEEDER_BUS_CBD_FARE

	CTA_FIRST_XFER_FARE = dh.m023.CTA_FIRST_XFER_FARE
	CTA_CBD_LINK_UP_FARE = dh.m023.CTA_CBD_LINK_UP_FARE

	AUTO_OPERATING_COST_BY_ZONETYPE = dh.m023.AUTO_OPERATING_COST_BY_ZONETYPE  # AVERAGE OPERATING COST PER MILE FOR AUTO, BY ZONE TYPE

	if replication is None:
		replication = ITER

	if trace:
		log.log(trace, f"transit_approach({ozone},{dzone},{TPTYPE},{replication})")

	# convert inputs to length-1 vectors if not already vectors
	if isinstance(ozone, int):
		ozone_ = ozone
		ozone = np.array([ozone])
	else:
		ozone_ = ozone = np.asanyarray(ozone)
	if isinstance(dzone, int):
		dzone_ = dzone
		dzone = np.array([dzone])
	else:
		dzone_ = dzone = np.asanyarray(dzone)
	vector_len = ozone.shape[0]
	assert vector_len == dzone.shape[0]

	ozone_idx = ozone-1
	dzone_idx = dzone-1

	m01_df = dh.m01
	ZTYPE = m01_df['zone_type'] # integers 1-4
	if TPTYPE == HW:
		fwbus = m01_df['first_wait_bus_peak'] # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
		fwfdr = m01_df['first_wait_feeder_peak'] # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
	else:
		fwbus = m01_df['first_wait_bus_offpeak'] # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
		fwfdr = m01_df['first_wait_feeder_offpeak'] # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
	PNRAVL = m01_df['pnr_flag'].astype(bool) # park-n-ride available, by zone
	PRCOST = m01_df['pnr_parking_cost'] # park-n-ride cost, by zone

	# -- INITIALIZE VALUES --
	approach_cost = np.zeros([vector_len, replication, N_APPROACH_MODES], dtype=np.float32)
	approach_waittime = np.zeros([vector_len, replication, N_APPROACH_MODES], dtype=np.float32)
	approach_drivetime = np.zeros([vector_len, replication, N_APPROACH_MODES], dtype=np.float32)
	approach_walktime = np.zeros([vector_len, replication, N_APPROACH_MODES], dtype=np.float32)
	TVAR4 = np.zeros([vector_len, replication, 5], dtype=np.float32)

	best_approach_mode = np.zeros([vector_len*replication, N_TRIP_ENDS], dtype=np.int8)
	best_cost = np.zeros([vector_len*replication, N_TRIP_ENDS], dtype=np.int32)
	best_waittime = np.zeros([vector_len*replication, N_TRIP_ENDS], dtype=np.int32)
	best_walktime = np.zeros([vector_len* replication, N_TRIP_ENDS], dtype=np.int32)
	best_drivetime = np.zeros([vector_len* replication, N_TRIP_ENDS], dtype=np.int32)
	#
	#     GET ZONE TYPES
	#
	ozone_type = ZTYPE.iloc[ozone_idx]
	dzone_type = ZTYPE.iloc[dzone_idx]
	#
	#     GET INTERCHANGE ATTRIBUTES
	#     FM=FIRST MODE,LM=LAST MODE,PM=PRIORITY MODE
	#
	if TPTYPE == 'HW':
		FM = dh.skims.first_mode_peak[ozone_idx, dzone_idx]
		LM = dh.skims.last_mode_peak[ozone_idx, dzone_idx]
	else:
		FM = dh.skims.first_mode_offpeak[ozone_idx, dzone_idx]
		LM = dh.skims.last_mode_offpeak[ozone_idx, dzone_idx]
	#
	#     INET TRANSIT NETWORK STORES SOME SUBURBAN BUS LINES (MODE=6)
	#     AS MODE=5 DUE TO ARRAY SIZE LIMITS.  IF MODE=5 AND
	#     ZONE TYPE NO. 1 IS OUTSIDE OF CHICAGO, THEN CHANGE MODE TO 6.
	#

	FM[(FM == 5) & (ozone_type > 2)] = 6
	LM[(LM == 5) & (ozone_type > 2)] = 6

	#
	#     GET APPROACH DISTANCES FOR FIRST AND LAST MODES
	#

	####      CALL ADIST(ozone,dzone,FM,LM)
	if approach_distances is not None:
		if replication == 1 and len(approach_distances.shape) == 3:
			approach_distances = np.expand_dims(approach_distances, axis=1)
		assert approach_distances.shape == (vector_len, replication, N_DIST_TO_TYPES, N_TRIP_ENDS)
	else:
		approach_distances = np.empty([vector_len, replication, N_DIST_TO_TYPES, N_TRIP_ENDS])
		_simulate_approach_distances(
			dh,
			ozone_,
			attached_mode=FM,
			trip_purpose=TPTYPE,
			trip_end=0,
			out=approach_distances[:,:,:,0],
			random_state=random_state,
		)
		_simulate_approach_distances(
			dh,
			dzone_,
			attached_mode=LM,
			trip_purpose=TPTYPE,
			trip_end=1,
			out=approach_distances[:,:,:,1],
			random_state=random_state,
		)
	if trace:
		log.log(trace, f" PRODUCTION APPROACH DISTANCES")
		log.log(trace, f"  to Bus    {approach_distances[:5,:5,DIST_TO_BUS,0]}")
		log.log(trace, f"  to El     {approach_distances[:5,:5,DIST_TO_CTA_RAIL,0]}")
		log.log(trace, f"  to Metra  {approach_distances[:5,:5,DIST_TO_METRA,0]}")
		log.log(trace, f"  to feeder {approach_distances[:5,:5,DIST_TO_FEEDER_BUS,0]}")
		log.log(trace, f"  to PnR    {approach_distances[:5,:5,DIST_TO_PARK_N_RIDE_STATION,0]}")
		log.log(trace, f" ATTRACTION APPROACH DISTANCES")
		log.log(trace, f"  to Bus    {approach_distances[:5,:5,DIST_TO_BUS,1]}")
		log.log(trace, f"  to El     {approach_distances[:5,:5,DIST_TO_CTA_RAIL,1]}")
		log.log(trace, f"  to Metra  {approach_distances[:5,:5,DIST_TO_METRA,1]}")
		log.log(trace, f"  to feeder {approach_distances[:5,:5,DIST_TO_FEEDER_BUS,1]}")
		log.log(trace, f"  to PnR    {approach_distances[:5,:5,DIST_TO_PARK_N_RIDE_STATION,1]}")


	#     CHECK FIRST/LAST MODES AND COMPUTE APPROACH TIME AND COST
	#
	#     ARRAYS approach_walktime,APCOST,approach_drivetime CONTAIN TIME TO WALK,APPROACH COST,
	#     AND IN-VEHICLE APPROACH TIME RESPECTIVELY. THESE ARRAYS HAVE FIVE
	#     ELEMENTS FOR FIVE POSSIBLE APPROACH MODES.( 1-WALK,2-BUS,
	#     3-PARK & RIDE,4-KISS & RIDE,AND 5-FEEDER BUS)
	#

	for I in range(N_TRIP_ENDS):
		#     I=1 GET VALUES FOR ORIGIN
		#     I=2 GET VALUES FOR DESTINATION
		if (I == FRONT_END):
			Z = ozone
			M = FM
		else: # I == BACK_END
			Z = dzone
			M = LM

		ZTYPE_Z = ZTYPE[Z].values

		#
		#  IN THIS CASE WE ARE MAKING THE STATION PARKING COST FOR HOME BASED OTHER AND
		#  NON-HOME BASED TRIPS EQUAL TO 60 PERCENT OF HOME BASED WORK
		#  CHANGE MADE 12/8/93 BY GWS NEXT LINE
		#      IF(TPTYPE.NE.1) PRCOST(Z) = PRCOST(Z) * 0.6
		# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
		#     NONWORK PARK AND RIDE PARKING COST IS NOW READ FROM M01.  IN MANY
		#     CASES THE NONWORK PNR COSTS ARE HIGHER THAN WORK DUE TO
		#     DISCOUNTING OF MONTHLY PARKING FEES.
		#
		# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
		#     SET HIGH STARTING VALUE OF TVAR5
		TVAR5 = np.full([vector_len*replication], 1.E10)
		# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
		#     IN CALCULATING TVAR4 AND TVAR5
		#       IN-VEHICLE TIME = DRVOT = 20 CENTS/MIN
		#       OUT-OF-VEHICLE TIME = 40 CENTS/MIN
		#       PASSENGER TIME = 0.5 DRIVER TIME
		# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
		#
		#     FOLLOWING SECTION ADDED BY EASH TO SIMPLIFY LOGIC
		#     IF M IS BUS (MODE<7) THEN ONLY POSSIBLE APPROACH COST IS TIME TO
		#     WALK TO BUS.  OTHER APPROACH COSTS AND TIMES ARE LINE=HAUL.
		t = (M < TransitModeCode_CTA_RAIL)
		if np.any(t):
			J = APPROACH_WALK
			approach_walktime[t,:,J] = approach_distances[t,:, DIST_TO_BUS, I] / SPDWLK * 600.
			# INCREASE WALK TIME IN CHICAGO CBD FOR WORK TRIPS
			cbd_work = t & (ZTYPE_Z == 1) & (TPTYPE == HW)
			approach_walktime[cbd_work,:,J] *= 1.20

			# if (ZTYPE_Z == 1 and TPTYPE == HW):
			# 	approach_walktime[:, 0] = approach_walktime[:, 0] * 1.20
			TVAR4[t,:, J] = approach_walktime[t,:, 0] * DRVOT * 2.0
			TVAR5.reshape([vector_len,replication])[t,:] = TVAR4[t,:, J]
			best_approach_mode.reshape([vector_len,replication,N_TRIP_ENDS])[t,:, I] = 0
			best_drivetime.reshape([vector_len,replication,N_TRIP_ENDS])[t,:, I] = 0
			best_walktime.reshape([vector_len,replication,N_TRIP_ENDS])[t,:, I] = approach_walktime[t,:, 0] + .5
			best_cost.reshape([vector_len,replication,N_TRIP_ENDS])[t,:, I] = 0
			best_waittime.reshape([vector_len,replication,N_TRIP_ENDS])[t,:, I] = 0
			for J in [APPROACH_BUS, APPROACH_PARK_N_RIDE, APPROACH_KISS_N_RIDE, APPROACH_FEEDER_BUS]:
				TVAR4[t,:, J] = 0.
				approach_cost[t, :, J] = 0.
				approach_waittime[t, :, J] = 0.
				approach_drivetime[t,:, J] = 0.
				approach_walktime[t,:, J] = 0.

		t = (M >= TransitModeCode_CTA_RAIL)
		if np.any(t):
			#     REMAINDER OF SUBROUTINE FOR RAIL TRANSIT/COMMUTER RAIL ONLY
			#     GET VALUES FOR FIVE ALTERNATIVES
			for J in range(5):
				TVAR4[t,:, J] = 0.
				approach_cost[t, :, J] = 0.
				approach_waittime[t,:,J] = 0.
				approach_drivetime[t,:, J] = 0.
				approach_walktime[t,:, J] = 0.

				K = np.fmax(0, M - 6).astype(int) # 0 for BUS, 1 for CTA RAIL(7-1), 2 for METRA(8-1)

				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				#     J=0(WALK).COMPUTE WALKING TIME TO FIRST MODE.NO COST OR IN-VEHICLE TIME
				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				if J == APPROACH_WALK:
					approach_walktime[t,:, J] = approach_distances[t,:, K[t], I] / SPDWLK * 600.
					# INCREASE WALK TIME IN CHICAGO CBD FOR WORK TRIPS
					cbd_work = t & (ZTYPE_Z == 1) & (TPTYPE == HW)
					approach_walktime[cbd_work,:, J] *= 1.20
					# if (ZTYPE_Z == 1 and TPTYPE == HW):
					# 	approach_walktime[:, J] = approach_walktime[:, J] * 1.20
					TVAR4[t,:, J] = approach_walktime[t,:, J] * DRVOT * 2.0
					# ADD APPROACH TIMES AND COSTS - EVERYTHING SHOULD NOW BE IN CENTS
					TVAR4[t,:, J] += approach_cost[t, :, J]

				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				#    J=1(BUS) FIRST MODE. COMPUTE WALKING TIME, COST, AND IN-VEHICLE TIME
				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				elif J == APPROACH_BUS or (J == APPROACH_FEEDER_BUS and I == FRONT_END):

					if J == APPROACH_FEEDER_BUS:
						approach_walktime[t,:, J] = approach_distances[t,:, DIST_TO_FEEDER_BUS, I] / SPDWLK * 600.
						approach_waittime[t,:,J] = fwfdr[Z][t].values[:,np.newaxis]
					else:
						approach_walktime[t,:, J] = approach_distances[t,:, DIST_TO_BUS, I] / SPDWLK * 600.
						# INCREASE WALK TIME IN CHICAGO CBD
						approach_walktime[t & (ZTYPE_Z == 1), :, J] *= 1.20
						approach_waittime[t,:,J] = fwbus[Z][t].values[:,np.newaxis]
					approach_drivetime[t,:, J] = approach_distances[t,:, K[t], I] / SPEEDS[ZTYPE_Z[t]-1 + 4, np.newaxis] * 60.
					TVAR4[t,:, J] = (
							approach_walktime[t,:, J] * OVT_IVT_RATIO
							+ approach_drivetime[t,:, J]
							+ approach_waittime[t,:,J] * OVT_IVT_RATIO
					) * DRVOT

					# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
					#
					#     COST COMPUTATIONS FOR APPROACH BUS
					#     REVISED BY EASH 12/4/93 TO REFLECT CURRENT FARES
					#   ====  ORIGIN  ====
					if (I == FRONT_END):
						# FIRST MODE SUBURBAN RAIL - CHECK ZONE TYPE AT ORIGIN						
						# --- SUBURBAN ORIGIN, PACE BUS ---
						s = t & (FM == TransitModeCode_METRA_RAIL) & (ozone_type > 2)
						if np.any(s):
							# PACE BUS - METRA RAIL, ADDED FARE IS PACE FEEDER BUS FARE
							approach_cost[s & (LM == TransitModeCode_METRA_RAIL), :, J] = FEEDER_BUS_BOARDING_FARE
							#     PACE BUS - METRA RAIL - CTA, NO ADDED FARE, LINKUP > FEEDER BUS
							# already zero # approach_cost[s & (LM == TransitModeCode_CTA_RAIL), :, J] = 0
							# PACE BUS - METRA RAIL - PACE, ADDED FARE IS LINKUP LESS FEEDER BUS
							approach_cost[s & (LM == TransitModeCode_PACE_BUS), :, J] = FEEDER_BUS_CBD_FARE

						#   --- CHICAGO ORIGIN, CTA BUS ---
						s = t & (FM == TransitModeCode_METRA_RAIL) & (ozone_type <= 2)
						if np.any(s):
							# CTA BUS - METRA RAIL, ADDED FARE IS LINKUP FARE (SINGLE RIDE)
							approach_cost[s & (LM == TransitModeCode_METRA_RAIL), :, J] = CTA_CBD_LINK_UP_FARE
							# CTA BUS - METRA RAIL - CTA, ADDED FARE IS CTA TRANSFER
							approach_cost[s & (LM == TransitModeCode_CTA_RAIL), :, J] = CTA_FIRST_XFER_FARE
							# CTA BUS - METRA RAIL - PACE, ADDED FARE IS LINKUP LESS FEEDER BUS
							approach_cost[s & (LM == TransitModeCode_PACE_BUS), :, J] = FEEDER_BUS_CBD_FARE

						# FIRST MODE CTA RAIL
						#     WHEN THIS IS TRUE A FULL FARE AND TRANSFER HAVE
						#     BEEN PAID, SO NO ADDED FARE IS NEEDED FOR BUS


						# ORIGIN OTHER THAN CHICAGO, ADDED FARE IS NOW AN RTA TRANSFER
						s = t & (FM < TransitModeCode_CTA_RAIL) & (ozone_type > 2)
						if np.any(s):
							approach_cost[s, :, J] = PACE_BUS_FIRST_XFER_FARE

						# CHICAGO ORIGIN, ADDED FARE IS CTA TRANSFER
						s = t & (FM < TransitModeCode_CTA_RAIL) & (ozone_type <= 2)
						if np.any(s):
							approach_cost[s, :, J] = CTA_FIRST_XFER_FARE

						TVAR4[t, :, J] += approach_cost[t, :, J]


					#   ====  DESTINATION  ====
					else:
						# LAST MODE SUBURBAN RAIL
						s = t & (LM == TransitModeCode_METRA_RAIL) & (dzone_type > 2)
						if np.any(s):
							#     SUBURBAN DESTINATION, PACE BUS
							#     METRA RAIL - PACE BUS, ADDED FARE IS PACE FEEDER BUS FARE
							approach_cost[s & (FM == TransitModeCode_METRA_RAIL), :, J] = FEEDER_BUS_BOARDING_FARE
							#     CTA - METRA RAIL - PACE BUS,  NO ADDED FARE, LINKUP > FEEDER BUS
							#     PACE - METRA RAIL - PACE BUS, ADDED FARE IS LINKUP LESS FEEDER BUS
							approach_cost[s & (FM == TransitModeCode_PACE_BUS), :, J] = FEEDER_BUS_CBD_FARE
						s = t & (LM == TransitModeCode_METRA_RAIL) & (dzone_type <= 2)
						if np.any(s):
							#     CHICAGO DESTINATION, CTA BUS
							#     METRA - CTA BUS, ADDED COST IS LINKUP FARE (SINGLE RIDE)
							approach_cost[s & (FM == TransitModeCode_METRA_RAIL), :, J] = CTA_CBD_LINK_UP_FARE
							#     CTA - METRA - CTA BUS, ADDED COST IS CTA TRANSFER
							approach_cost[s & (FM == TransitModeCode_CTA_RAIL), :, J] = CTA_FIRST_XFER_FARE
							#     PACE - METRA - CTA BUS, ADDED COST IS LINKUP MINUS FEEDER BUS
							approach_cost[s & (FM == TransitModeCode_PACE_BUS), :, J] = FEEDER_BUS_CBD_FARE
						# ADD CTA TRANSFER IF NOT ALREADY PAID BUT IT WAS USED
						s = t & (LM < TransitModeCode_CTA_RAIL) & (dzone_type > 2)
						if np.any(s):
							approach_cost[s, J] = CTA_FIRST_XFER_FARE * (
									(best_approach_mode[:, FRONT_END]==APPROACH_BUS)
									|(best_approach_mode[:, FRONT_END]==APPROACH_FEEDER_BUS)
							)

						TVAR4[t, :, J] += approach_cost[t, :, J]


				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				#    J=2(PARK & RIDE) FIRST MODE. PARK & RIDE FOR APPROACH TO RAPID TRANSIT AND SUBURBAN RAIL ROAD
				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				elif (J == APPROACH_PARK_N_RIDE and I == FRONT_END):
					approach_drivetime[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] / SPEEDS[ZTYPE_Z[t] - 1, np.newaxis] * 60.
					approach_walktime[t,:, J] = W2PNR
					#     APPROACH COST=PER MILE COST + FIXED COST
					approach_cost[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] * AUTO_OPERATING_COST_BY_ZONETYPE[ZTYPE_Z[t] - 1,np.newaxis]
					#     OPERATING COST MAY NOT BE LESS THAN 5 CENTS
					approach_cost[t,:, J] = np.fmax(approach_cost[t,:, J], 5.0)

					approach_cost[t,:, J] = approach_cost[t,:, J] + AFC1
					#     ADD HALF OF THE PARKING COST IF PARK-&-RIDE AVAILABLE
					_tz = t&PNRAVL[Z]
					approach_cost[_tz,:, J] += PRCOST[Z].values[_tz,np.newaxis] / 2
					#     IF NO PARK-&-RIDE FACILITY AVAILABLE INCREASE WALK TIME
					approach_walktime[~_tz,:, J] = 3 * W2PNR

					TVAR4[t,:, J] = (
						approach_walktime[t,:, J] * DRVOT * 2.0
						+ approach_drivetime[t,:, J] * DRVOT
						+ approach_cost[t,:, J]
					)

				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				#    J=3(KISS & RIDE) FIRST MODE. KISS & RIDE FOR APPROACH TO RAPID TRANSIT AND SUBURBAN RAIL ROAD
				# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
				elif (J == APPROACH_KISS_N_RIDE and I == FRONT_END):
					approach_drivetime[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] / SPEEDS[ZTYPE_Z[t] - 1, np.newaxis] * 60.
					approach_walktime[t,:, J] = W2PNR
					approach_cost[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] * AUTO_OPERATING_COST_BY_ZONETYPE[ZTYPE_Z[t] - 1, np.newaxis]
					approach_cost[t,:, J] = np.fmin(approach_cost[t,:, J], 5.0)
					# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
					#     ASSUMPTION IS THAT KISS AND RIDE REQUIRES A SPECIAL
					#     TRIP FROM HOME.  DRIVER AND PASSENGER TIME VALUES NOW EQUAL.
					#      APCOST[J]=APCOST[J]*2.+AFC2+(DRVOT*approach_drivetime[J]*2.)/10
					approach_cost[t,:, J] = approach_cost[t,:, J] * 2. + AFC2
					TVAR4[t,:, J] = (
							approach_walktime[t,:, J] * OVT_IVT_RATIO
							+ approach_drivetime[t,:, J] * 2 # KISSING DRIVER
							+ approach_drivetime[t,:, J]
					) * DRVOT
					# *******************  RWE CHANGE FOR I290 AUGUST-SEPT 2009  ************
					TVAR4[t,:, J] += approach_cost[t,:, J]

		# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
		#     EVALUATE APPROACH MODES AND SELECT THE BEST
		# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
		#     FOLLOWING CODE CHANGED BY EASH 12/6/93 TO REFLECT
		#     NEW TRIP TYPES
		#     TPTYPE = 1 HOME PRODUCTIONS TO WORK ATTRACTIONS
		#     TPTYPE = 2 HOME PRODUCTIONS TO NON-WORK/NON-HOME ATTRACTIONS
		#     TPTYPE = 3 NON-HOME PRODUCTIONS TO NON-HOME ATTRACTIONS
		for J in range(N_APPROACH_MODES):
			#     NO KISS-&-RIDE FOR NON-WORK TRIPS
			if (TPTYPE != HW and J == APPROACH_KISS_N_RIDE):
				continue
			#     NO PARK-AND RIDE OR KISS-&-RIDE AT THE WORK/OTHER
			#     ATTRACTION END FOR HOME BASED TRIPS
			if (TPTYPE != NH and I == BACK_END and (J == APPROACH_PARK_N_RIDE or J == APPROACH_KISS_N_RIDE)):
				continue
			#     NO PARK-&-RIDE OR KISS AND RIDE FOR NON-HOME TO NON-HOME TRIPS
			if (TPTYPE == NH and (J == APPROACH_PARK_N_RIDE or J == APPROACH_KISS_N_RIDE)):
				continue
			# --  FIND LOWEST COST APPROACH
			TVAR4_J = TVAR4[:,:, J].reshape(-1)
			low_cost = (TVAR4_J < TVAR5) & (TVAR4_J > 0)
			TVAR5[low_cost] = TVAR4_J[low_cost]
			best_approach_mode[low_cost, I] = J
			best_drivetime[low_cost, I] = approach_drivetime.reshape([-1,N_APPROACH_MODES])[low_cost, J] + .5
			best_walktime[low_cost, I] = approach_walktime.reshape([-1,N_APPROACH_MODES])[low_cost, J] + .5
			best_cost[low_cost, I] = approach_cost.reshape([-1,N_APPROACH_MODES])[low_cost, J] + .5
			best_waittime[low_cost, I] = approach_waittime.reshape([-1,N_APPROACH_MODES])[low_cost,J] + .5
			if trace:
				log.log(trace, f" DIRECTION {I} APPROACH TYPE {J} {APPROACH_MODE_NAMES.get(J)}")
				log.log(trace, f"  drivetime {approach_drivetime[:5,:5, J]}")
				log.log(trace, f"  walktime  {approach_walktime[:5,:5, J]}")
				log.log(trace, f"  cost      {approach_cost[:5,:5, J]}")
				log.log(trace, f"  waittime  {approach_waittime[:5,:5,J]}")
				log.log(trace, f"  gen cost  {TVAR4[:5,:5, J]}")
		if trace:
			log.log(trace, f" DIRECTION {I} BEST APPROACH TYPE {best_approach_mode[:5,I]}")


	#     ADD ORIGIN AND DESTINATION QUANTITIES AND PASS BACK TO TRIPS

	ae_drivetime = best_drivetime[:, 0] + best_drivetime[:, 1]
	ae_walktime = best_walktime[:, 0] + best_walktime[:, 1]
	ae_cost = best_cost[:, 0] + best_cost[:, 1]
	ae_waittime = best_waittime[:, 0] + best_waittime[:, 1]

	if not isinstance(ozone_, int):
		ae_drivetime = ae_drivetime.reshape([vector_len,replication])
		ae_walktime = ae_walktime.reshape([vector_len,replication])
		ae_cost = ae_cost.reshape([vector_len,replication])
		ae_waittime = ae_waittime.reshape([vector_len,replication])
		best_approach_mode = best_approach_mode.reshape([vector_len,replication,N_TRIP_ENDS])
		approach_distances = approach_distances.reshape([vector_len, replication, N_DIST_TO_TYPES, N_TRIP_ENDS])

	out = Dict()
	out.drivetime = ae_drivetime
	out.walktime = ae_walktime
	out.cost = ae_cost
	out.waittime = ae_waittime
	out.approach_mode = best_approach_mode
	out.approach_distances = approach_distances
	return out

