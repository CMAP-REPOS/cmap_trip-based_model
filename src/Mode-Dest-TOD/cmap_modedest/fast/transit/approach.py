
import numpy as np
import numba as nb
import logging
from ...addict import Dict
from typing import Generator
from collections import namedtuple

log = logging.getLogger('CMAP')

from .access_egress_distance import compile_simulate_approach_distances
from ...ae_distance_sim import simulate_ae_dist
from ...random_states import check_random_state
from ...purposes import purposes_to_3

# trip types
HW = 'HW'  # HOME PRODUCTIONS TO WORK ATTRACTIONS
HO = 'HO'  # HOME PRODUCTIONS TO NON-WORK/NON-HOME ATTRACTIONS
NH = 'NH'  # NON-HOME PRODUCTIONS TO NON-HOME ATTRACTIONS

ITER = 50  # NUMBER OF TRIPS USED TO COMPUTE AVERAGE IMPEDANCES

from ...modecodes import (
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

SPDWLK = np.float32(30.)
# SPDWLK = SYSTEM-WIDE SPEED OF WALKING,
#          DEFAULT IS 30 TENTHS OF A MILE PER HOUR

SPEEDS = np.array([7., 15., 20., 30., 5., 10., 12., 17.])
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

TransitApproachStruct = namedtuple('TransitApproachStruct', [
    'fwbus_pk',
    'fwfdr_pk',
    'fwbus_op',
    'fwfdr_op',
    'ZTYPE',
    'FM_pk',
    'LM_pk',
    'FM_op',
    'LM_op',
    'FEEDER_BUS_BOARDING_FARE',
    'FEEDER_BUS_CBD_FARE',
    'CTA_CBD_LINK_UP_FARE',
    'CTA_FIRST_XFER_FARE',
    'PACE_BUS_FIRST_XFER_FARE',
    'PNRAVL',
    'PRCOST',
    'AUTO_OPERATING_COST_BY_ZONETYPE',
    'distr_array',
    'distr_BUS',
    'distr_CTARAIL',
    'distr_FEEDERBUS',
    'distr_METRA',
    'distr_PNR',
])

# class TransitApproachStruct(_TransitApproachStruct):
#     def __new__(cls, *args, **kwargs):
#         if len(args) == 1 and len(kwargs) == 0:
#             if isinstance(args[0], cls):
#                 return args[0]
#             elif isinstance(args[0], Generator):
#                 return super().__new__(cls, *args[0])
#         return super().__new__(cls, *args, **kwargs)


def compile_transit_approach(
        dh,
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

    PACE_BUS_BOARDING_FARE = dh.m023.PACE_BUS_BOARDING_FARE
    PACE_BUS_FIRST_XFER_FARE = dh.m023.PACE_BUS_FIRST_XFER_FARE
    FEEDER_BUS_BOARDING_FARE = dh.m023.FEEDER_BUS_BOARDING_FARE
    FEEDER_BUS_CBD_FARE = dh.m023.FEEDER_BUS_CBD_FARE

    CTA_FIRST_XFER_FARE = dh.m023.CTA_FIRST_XFER_FARE
    CTA_CBD_LINK_UP_FARE = dh.m023.CTA_CBD_LINK_UP_FARE

    AUTO_OPERATING_COST_BY_ZONETYPE = dh.m023.AUTO_OPERATING_COST_BY_ZONETYPE  # AVERAGE OPERATING COST PER MILE FOR AUTO, BY ZONE TYPE

    m01_df = dh.m01

    ZTYPE = m01_df['zone_type'].to_numpy()
    fwbus_pk = m01_df['first_wait_bus_peak'].to_numpy().astype(np.float32)  # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    fwfdr_pk = m01_df['first_wait_feeder_peak'].to_numpy().astype(np.float32)   # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    fwbus_op = m01_df['first_wait_bus_offpeak'].to_numpy().astype(np.float32)   # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    fwfdr_op = m01_df['first_wait_feeder_offpeak'].to_numpy().astype(np.float32)   # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    PNRAVL = m01_df['pnr_flag'].astype(bool).to_numpy()  # park-n-ride available, by zone
    PRCOST = m01_df['pnr_parking_cost'].to_numpy().astype(np.float32)   # park-n-ride cost, by zone

    FM_pk = dh.skims.first_mode_peak.astype(np.int32)
    LM_pk = dh.skims.last_mode_peak.astype(np.int32)
    FM_op = dh.skims.first_mode_offpeak.astype(np.int32)
    LM_op = dh.skims.last_mode_offpeak.astype(np.int32)

    # simulate_approach_distances, simulate_approach_distances2 = compile_simulate_approach_distances(dh)
    distr_array = np.stack([
        dh.distr['HW'],
        dh.distr['HO'],
        dh.distr['NH'],
    ]).reshape((3,-1,5,3)).astype(np.float32)

    distr_BUS = dh.distr['HW'].loc[1].index.get_loc('bus')
    distr_CTARAIL = dh.distr['HW'].loc[1].index.get_loc('ctarail')
    distr_FEEDERBUS = dh.distr['HW'].loc[1].index.get_loc('feederbus')
    distr_METRA = dh.distr['HW'].loc[1].index.get_loc('metra')
    distr_PNR = dh.distr['HW'].loc[1].index.get_loc('pnr')



    trapp_struct = TransitApproachStruct(
        fwbus_pk= fwbus_pk,
        fwfdr_pk= fwfdr_pk,
        fwbus_op= fwbus_op,
        fwfdr_op= fwfdr_op,
        ZTYPE= ZTYPE,
        FM_pk= FM_pk,
        LM_pk= LM_pk,
        FM_op= FM_op,
        LM_op= LM_op,
        FEEDER_BUS_BOARDING_FARE= FEEDER_BUS_BOARDING_FARE,
        FEEDER_BUS_CBD_FARE= FEEDER_BUS_CBD_FARE,
        CTA_CBD_LINK_UP_FARE= CTA_CBD_LINK_UP_FARE,
        CTA_FIRST_XFER_FARE= CTA_FIRST_XFER_FARE,
        PACE_BUS_FIRST_XFER_FARE= PACE_BUS_FIRST_XFER_FARE,
        PNRAVL= PNRAVL,
        PRCOST= PRCOST,
        AUTO_OPERATING_COST_BY_ZONETYPE= AUTO_OPERATING_COST_BY_ZONETYPE,
        distr_array=distr_array,
        distr_BUS = distr_BUS,
        distr_CTARAIL = distr_CTARAIL,
        distr_FEEDERBUS = distr_FEEDERBUS,
        distr_METRA = distr_METRA,
        distr_PNR = distr_PNR,
    )


    return trapp_struct


def transit_approach_distances(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        n_reps=50,
        random_seed=None,
):
    if random_seed is not None:
        seed_base = np.int32(random_seed)
    else:
        seed_base = np.int32(0)
    TPTYPE = purposes_to_3.get(TPTYPE, 'NH')
    approach_distances = np.zeros((ozone.shape[0], n_reps, 5, 2), dtype=np.float32)
    seeds = np.zeros(ozone.shape[0], dtype=np.int32)
    _transit_approach_distances(trapp_struct, ozone, dzone, TPTYPE, seed_base, approach_distances, seeds)
    return approach_distances, seeds


@nb.njit(parallel=True, cache=True)
def _transit_approach_distances(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        seed_base,
        approach_distances,
        seeded,
):
    # struct data
    ZTYPE = trapp_struct.ZTYPE
    FM_pk = trapp_struct.FM_pk
    LM_pk = trapp_struct.LM_pk
    FM_op = trapp_struct.FM_op
    LM_op = trapp_struct.LM_op
    distr_array = trapp_struct.distr_array
    distr_BUS = trapp_struct.distr_BUS
    distr_CTARAIL = trapp_struct.distr_CTARAIL
    distr_FEEDERBUS = trapp_struct.distr_FEEDERBUS
    distr_METRA = trapp_struct.distr_METRA
    distr_PNR = trapp_struct.distr_PNR

    assert approach_distances.shape[-2:] == (5,2), \
        "last dimensions approach_distances must be (5,2)"

    for z in nb.prange(ozone.shape[0]):

        ozone_idx = ozone[z]-1
        dzone_idx = dzone[z]-1

        seed = np.int32(seed_base) + (np.int32(ozone_idx) << 14) + np.int32(dzone_idx)
        seeded[z] = seed
        np.random.seed(seed)

        #
        #     GET ZONE TYPES
        #
        ozone_type = ZTYPE[ozone_idx]
        dzone_type = ZTYPE[dzone_idx]
        #
        #     GET INTERCHANGE ATTRIBUTES
        #     FM=FIRST MODE,LM=LAST MODE,PM=PRIORITY MODE
        #
        if TPTYPE == 0:
            FM = FM_pk[ozone_idx, dzone_idx]
            LM = LM_pk[ozone_idx, dzone_idx]
        else:
            FM = FM_op[ozone_idx, dzone_idx]
            LM = LM_op[ozone_idx, dzone_idx]
        #
        #     INET TRANSIT NETWORK STORES SOME SUBURBAN BUS LINES (MODE=6)
        #     AS MODE=5 DUE TO ARRAY SIZE LIMITS.  IF MODE=5 AND
        #     ZONE TYPE NO. 1 IS OUTSIDE OF CHICAGO, THEN CHANGE MODE TO 6.
        #

        if (FM == 5) and (ozone_type > 2):
            FM = 6
        if (LM == 5) and (dzone_type > 2):
            LM = 6

        #
        #     GET APPROACH DISTANCES FOR FIRST AND LAST MODES
        #
        for trip_end in [0,1]:
            if trip_end == 0:
                zone = ozone[z]
                attached_mode = FM
            else:
                zone = dzone[z]
                attached_mode = LM
            if TPTYPE == 'HW':
                distr_ = distr_array[0, zone - 1]
            elif TPTYPE == 'HO':
                distr_ = distr_array[1, zone - 1]
            else:  # if TPTYPE == 'NH':
                distr_ = distr_array[2, zone - 1]
            for J in range(N_DIST_TO_TYPES):
                # OBTAIN APPROACH DISTANCES TO FIVE MODES
                if (J == DIST_TO_BUS):
                    distr_params = distr_[distr_BUS]
                elif (J == DIST_TO_CTA_RAIL) and (attached_mode == TransitModeCode_CTA_RAIL):
                    # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS CTA RAIL
                    distr_params = distr_[distr_CTARAIL]
                elif (J == DIST_TO_METRA) and (attached_mode == TransitModeCode_METRA_RAIL):
                    # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
                    distr_params = distr_[distr_METRA]
                elif (J == DIST_TO_FEEDER_BUS) and (attached_mode == TransitModeCode_METRA_RAIL):
                    # DISTANCE OBTAINED ONLY IF FIRST/LAST MODE IS METRA
                    distr_params = distr_[distr_FEEDERBUS]
                elif (J == DIST_TO_PARK_N_RIDE_STATION) and (trip_end == FRONT_END):
                    # PARK AND RIDE STATION DISTANCE OBTAINED WHEN TRIP END IS FRONT
                    distr_params = distr_[distr_PNR]
                else:
                    distr_params = np.full(3, 999, dtype=np.float32)
                for k in range(approach_distances.shape[1]):
                    if distr_params[2] == 101:        # use_normal = (p3 == 101)
                        rv = np.random.normal(distr_params[0], distr_params[1])
                        approach_distances[z, k, J, trip_end] = np.float32(min(max(rv, 0.06), 200.0))
                    elif distr_params[2] < 101:       # use_slopey = (p3 < 101)
                        x_min = distr_params[0]
                        x_max = distr_params[1]
                        ratio = distr_params[2]
                        if ratio == 1.0:
                            approach_distances[z, k, J, trip_end] = np.float32(min(np.random.uniform(x_min, x_max), 200.0))
                        else:
                            span = x_max - x_min
                            if span != 0:
                                slope = (1.0 - ratio) / span
                            else:
                                slope = (1.0 - ratio)
                            area = .5 * (1 + ratio) * span
                            y = np.float32(np.random.uniform(0.0, 1.0)) * area
                            zp = np.sqrt(ratio * ratio + 2 * slope * y)
                            if slope:
                                rv = ((zp - ratio) / slope) + x_min
                            else:
                                rv = (zp - ratio) + x_min
                            approach_distances[z, k, J, trip_end] = min(max(rv, 0.06), 200.0)
                    else:                             # use_nan = (p3 == 999)
                        approach_distances[z, k, J, trip_end] = 255.0


@nb.njit(cache=True)
def transit_approach(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        approach_distances,
        out,
):
    # struct data
    fwbus_pk = trapp_struct.fwbus_pk   # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    fwfdr_pk = trapp_struct.fwfdr_pk   # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    fwbus_op = trapp_struct.fwbus_op   # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    fwfdr_op = trapp_struct.fwfdr_op   # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    ZTYPE = trapp_struct.ZTYPE
    FM_pk = trapp_struct.FM_pk
    LM_pk = trapp_struct.LM_pk
    FM_op = trapp_struct.FM_op
    LM_op = trapp_struct.LM_op
    FEEDER_BUS_BOARDING_FARE = trapp_struct.FEEDER_BUS_BOARDING_FARE
    FEEDER_BUS_CBD_FARE = trapp_struct.FEEDER_BUS_CBD_FARE
    CTA_CBD_LINK_UP_FARE = trapp_struct.CTA_CBD_LINK_UP_FARE
    CTA_FIRST_XFER_FARE = trapp_struct.CTA_FIRST_XFER_FARE
    PACE_BUS_FIRST_XFER_FARE = trapp_struct.PACE_BUS_FIRST_XFER_FARE
    PNRAVL = trapp_struct.PNRAVL
    PRCOST = trapp_struct.PRCOST
    AUTO_OPERATING_COST_BY_ZONETYPE = trapp_struct.AUTO_OPERATING_COST_BY_ZONETYPE

    # random_state = check_random_state(random_state or ozone+dzone)
    replication = approach_distances.shape[1]

    # if trace:
    #     log.log(trace, f"transit_approach({ozone},{dzone},{TPTYPE},{replication})")

    # convert inputs to length-1 vectors if not already vectors
    # ozone = np.asanyarray(ozone)
    # dzone = np.asanyarray(dzone)
    vector_len = ozone.shape[0]
    assert vector_len == dzone.shape[0]

    ozone_idx = ozone-1
    dzone_idx = dzone-1

    # ZTYPE = m01_df['zone_type'].to_numpy()
    # fwbus_pk = m01_df['first_wait_bus_peak'].to_numpy()  # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    # fwfdr_pk = m01_df['first_wait_feeder_peak'].to_numpy()  # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    # fwbus_op = m01_df['first_wait_bus_offpeak'].to_numpy()  # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
    # fwfdr_op = m01_df['first_wait_feeder_offpeak'].to_numpy()  # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    # PNRAVL = m01_df['pnr_flag'].astype(bool).to_numpy()  # park-n-ride available, by zone
    # PRCOST = m01_df['pnr_parking_cost'].to_numpy()  # park-n-ride cost, by zone

    if TPTYPE == HW:
        fwbus = fwbus_pk # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
        fwfdr = fwfdr_pk # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL
    else:
        fwbus = fwbus_op # FIRST WAIT FOR BUS IN APPROACH SUBMODEL
        fwfdr = fwfdr_op # FIRST WAIT FOR FEEDER BUS IN APPROACH SUBMODEL

    # -- INITIALIZE VALUES --
    approach_cost = np.zeros((vector_len, replication, N_APPROACH_MODES), dtype=np.float32)
    approach_waittime = np.zeros((vector_len, replication, N_APPROACH_MODES), dtype=np.float32)
    approach_drivetime = np.zeros((vector_len, replication, N_APPROACH_MODES), dtype=np.float32)
    approach_walktime = np.zeros((vector_len, replication, N_APPROACH_MODES), dtype=np.float32)
    TVAR4 = np.zeros((vector_len, replication, 5), dtype=np.float32)

    best_approach_mode = np.zeros((vector_len,replication, N_TRIP_ENDS), dtype=np.int8)
    best_cost = np.zeros((vector_len, replication, N_TRIP_ENDS), dtype=np.int32)
    best_waittime = np.zeros((vector_len, replication, N_TRIP_ENDS), dtype=np.int32)
    best_walktime = np.zeros((vector_len, replication, N_TRIP_ENDS), dtype=np.int32)
    best_drivetime = np.zeros((vector_len, replication, N_TRIP_ENDS), dtype=np.int32)
    #
    #     GET ZONE TYPES
    #
    ozone_type = ZTYPE[ozone_idx]
    dzone_type = ZTYPE[dzone_idx]
    #
    #     GET INTERCHANGE ATTRIBUTES
    #     FM=FIRST MODE,LM=LAST MODE,PM=PRIORITY MODE
    #
    FM = np.empty(ozone_idx.size, dtype=np.int8)
    LM = np.empty(ozone_idx.size, dtype=np.int8)
    if TPTYPE == 'HW':
        for i in range(ozone_idx.size):
            FM[i] = FM_pk[ozone_idx[i], dzone_idx[i]]
            LM[i] = LM_pk[ozone_idx[i], dzone_idx[i]]
    else:
        for i in range(ozone_idx.size):
            FM[i] = FM_op[ozone_idx[i], dzone_idx[i]]
            LM[i] = LM_op[ozone_idx[i], dzone_idx[i]]
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

    assert approach_distances.shape == (vector_len, replication, N_DIST_TO_TYPES, N_TRIP_ENDS)

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

        ZTYPE_Z = ZTYPE[Z-1]

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
        TVAR5 = np.full((vector_len, replication), 1.E10)
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
        t1 = (M < TransitModeCode_CTA_RAIL)
        if np.any(t1):
            J = APPROACH_WALK
            approach_walktime[t1,:,J] = approach_distances[t1,:, DIST_TO_BUS, I] / SPDWLK * 600.
            # INCREASE WALK TIME IN CHICAGO CBD FOR WORK TRIPS
            cbd_work = t1 & (ZTYPE_Z == 1) & (TPTYPE == HW)
            approach_walktime[cbd_work,:,J] *= 1.20

            # if (ZTYPE_Z == 1 and TPTYPE == HW):
            # 	approach_walktime[:, 0] = approach_walktime[:, 0] * 1.20
            TVAR4[t1,:, J] = approach_walktime[t1,:, 0] * DRVOT * 2.0
            TVAR5[t1,:] = TVAR4[t1,:, J]
            best_approach_mode.reshape((vector_len,replication,N_TRIP_ENDS))[t1,:, I] = 0
            best_drivetime[t1,:, I] = 0
            best_walktime[t1,:, I] = approach_walktime[t1,:, 0] + .5
            best_cost[t1, :, I] = 0
            best_waittime[t1,:, I] = 0
            for J in [APPROACH_BUS, APPROACH_PARK_N_RIDE, APPROACH_KISS_N_RIDE, APPROACH_FEEDER_BUS]:
                TVAR4[t1,:, J] = 0.0
                approach_cost[t1, :, J] = 0.0
                approach_waittime[t1, :, J] = 0.0
                approach_drivetime[t1,:, J] = 0.0
                approach_walktime[t1,:, J] = 0.0

        t = (M >= TransitModeCode_CTA_RAIL)
        if np.any(t):
            #     REMAINDER OF SUBROUTINE FOR RAIL TRANSIT/COMMUTER RAIL ONLY
            #     GET VALUES FOR FIVE ALTERNATIVES
            for J in range(5):
                TVAR4[t,:, J] = 0.0
                approach_cost[t, :, J] = 0.0
                approach_waittime[t,:,J] = 0.0
                approach_drivetime[t,:, J] = 0.0
                approach_walktime[t,:, J] = 0.0

                K = np.maximum(0, M - 6).astype(np.int8) # 0 for BUS, 1 for CTA RAIL(7-1), 2 for METRA(8-1)

                # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
                #     J=0(WALK).COMPUTE WALKING TIME TO FIRST MODE.NO COST OR IN-VEHICLE TIME
                # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
                if J == APPROACH_WALK:

                    for _kt in range(approach_distances.shape[2]):
                        kt = t & (K == _kt)
                        approach_walktime[kt,:, J] = approach_distances[kt, :, _kt, I] / SPDWLK * 600.
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
                        approach_walktime[t, :, J] = approach_distances[t,:, DIST_TO_FEEDER_BUS, I] * 600.0 / SPDWLK
                        approach_waittime[t, :, J] = np.expand_dims(fwfdr[Z-1][t], 1) # broadcast
                    else:
                        approach_walktime[t,:, J] = approach_distances[t,:, DIST_TO_BUS, I] * 600.0 / SPDWLK
                        # INCREASE WALK TIME IN CHICAGO CBD
                        approach_walktime[t & (ZTYPE_Z == 1), :, J] *= 1.20
                        approach_waittime[t,:,J] = np.expand_dims(fwbus[Z-1][t],1) # broadcast
                    for _kt in range(approach_distances.shape[2]):
                        kt = t & (K == _kt)
                        approach_drivetime[kt,:, J] = approach_distances[kt,:, _kt, I] / np.expand_dims(SPEEDS[ZTYPE_Z[kt]-1 + 4],1) * 60. # broadcast
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
                            approach_cost[s, :, J] = CTA_FIRST_XFER_FARE * (
                                    (best_approach_mode[s,:, FRONT_END]==APPROACH_BUS)
                                    |(best_approach_mode[s,:, FRONT_END]==APPROACH_FEEDER_BUS)
                            )

                        TVAR4[t, :, J] += approach_cost[t, :, J]


                # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
                #    J=2(PARK & RIDE) FIRST MODE. PARK & RIDE FOR APPROACH TO RAPID TRANSIT AND SUBURBAN RAIL ROAD
                # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
                elif (J == APPROACH_PARK_N_RIDE and I == FRONT_END):
                    approach_drivetime[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] / np.expand_dims(SPEEDS[ZTYPE_Z[t] - 1],1) * 60. # broadcast
                    approach_walktime[t,:, J] = W2PNR
                    #     APPROACH COST=PER MILE COST + FIXED COST
                    approach_cost[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] * np.expand_dims(AUTO_OPERATING_COST_BY_ZONETYPE[ZTYPE_Z[t] - 1],1) # broadcast
                    #     OPERATING COST MAY NOT BE LESS THAN 5 CENTS
                    approach_cost[t,:, J] = np.fmax(approach_cost[t,:, J], 5.0)

                    approach_cost[t,:, J] = approach_cost[t,:, J] + AFC1
                    #     ADD HALF OF THE PARKING COST IF PARK-&-RIDE AVAILABLE
                    _tz = t&PNRAVL[Z-1]
                    approach_cost[_tz,:, J] += np.expand_dims(PRCOST[Z-1][_tz],1) / 2  # broadcast
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
                    approach_drivetime[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] / np.expand_dims(SPEEDS[ZTYPE_Z[t] - 1],1) * 60. # broadcast
                    approach_walktime[t,:, J] = W2PNR
                    approach_cost[t,:, J] = approach_distances[t,:, DIST_TO_PARK_N_RIDE_STATION, I] * np.expand_dims(AUTO_OPERATING_COST_BY_ZONETYPE[ZTYPE_Z[t] - 1],1) # broadcast
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
            for _i1 in range(TVAR4.shape[0]):
                for _i2 in range(TVAR4.shape[1]):
                    if (TVAR4[_i1, _i2, J] > 0) and (TVAR4[_i1, _i2, J] < TVAR5[_i1, _i2]):
                        TVAR5[_i1, _i2] = TVAR4[_i1, _i2, J]
                        best_approach_mode[_i1, _i2, I] = J
                        best_drivetime[_i1, _i2, I] = approach_drivetime[_i1, _i2, J] + .5
                        best_walktime[_i1, _i2, I] = approach_walktime[_i1, _i2, J] + .5
                        best_cost[_i1, _i2, I] = approach_cost[_i1, _i2, J] + .5
                        best_waittime[_i1, _i2, I] = approach_waittime[_i1, _i2, J] + .5

            # if trace:
            #     log.log(trace, f" DIRECTION {I} APPROACH TYPE {J} {APPROACH_MODE_NAMES.get(J)}")
            #     log.log(trace, f"  drivetime {approach_drivetime[:5,:5, J]}")
            #     log.log(trace, f"  walktime  {approach_walktime[:5,:5, J]}")
            #     log.log(trace, f"  cost      {approach_cost[:5,:5, J]}")
            #     log.log(trace, f"  waittime  {approach_waittime[:5,:5,J]}")
            #     log.log(trace, f"  gen cost  {TVAR4[:5,:5, J]}")
        # if trace:
        #     log.log(trace, f" DIRECTION {I} BEST APPROACH TYPE {best_approach_mode[:5,I]}")


    #     ADD ORIGIN AND DESTINATION QUANTITIES AND PASS BACK TO TRIPS

    ae_drivetime = best_drivetime[:,:, 0] + best_drivetime[:,:, 1]
    ae_walktime = best_walktime[:,:, 0] + best_walktime[:,:, 1]
    ae_cost = best_cost[:,:, 0] + best_cost[:,:, 1]
    ae_waittime = best_waittime[:,:, 0] + best_waittime[:,:, 1]

    ae_drivetime = ae_drivetime.reshape((vector_len,replication))
    ae_walktime = ae_walktime.reshape((vector_len,replication))
    ae_cost = ae_cost.reshape((vector_len,replication))
    ae_waittime = ae_waittime.reshape((vector_len,replication))
    best_approach_mode = best_approach_mode.reshape((vector_len,replication,N_TRIP_ENDS))

    out[:,:,0] = ae_drivetime
    out[:,:,1] = ae_walktime
    out[:,:,2] = ae_cost
    out[:,:,3] = ae_waittime
    out[:,:,4] = best_approach_mode[:,:,0]
    out[:,:,5] = best_approach_mode[:,:,1]

    # return (
    #     ae_drivetime,
    #     ae_walktime,
    #     ae_cost,
    #     ae_waittime,
    #     best_approach_mode,
    # )

@nb.njit(parallel=True, cache=True)
def transit_approach_parallel(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        approach_distances,
):
    out = np.zeros((ozone.size, approach_distances.shape[1], 6), dtype=np.int32)
    slices = ozone.size // 100 + (1 if ozone.size % 100 else 0)
    for i in nb.prange(slices):
        s = slice(i*100, (i+1)*100)
        transit_approach(
            trapp_struct,
            ozone[s],
            dzone[s],
            TPTYPE,
            approach_distances[s],
            out[s],
        )
    return (
        out[:, :, 0],
        out[:, :, 1],
        out[:, :, 2],
        out[:, :, 3],
        out[:, :, 4:],
    )

def transit_approach_wrap(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        approach_distances,
):
    out_tuple = transit_approach_parallel(
        trapp_struct,
        ozone,
        dzone,
        TPTYPE,
        approach_distances,
    )
    out = Dict()
    out.drivetime = out_tuple[0]
    out.walktime = out_tuple[1]
    out.cost = out_tuple[2]
    out.waittime = out_tuple[3]
    out.approach_mode = out_tuple[4]
    return out

#return transit_approach_wrap, transit_approach_distances