import numpy as np
import pandas as pd
from .random_states import check_random_state

# cbd_parking = pd.read_csv(
# 	filenames.HW_CBDPARK,
# 	header=None,
# 	names=['ZoneID', 'CumProb', 'ThresholdPrice', 'SavePrice', 'WalkSeconds'],
# )
# cbd_parking.CumProb /= 10000.
# # cbd_parking['SumPrice'] = cbd_parking.ThresholdPrice + cbd_parking.SavePrice
# cbd_parking['rownum'] = cbd_parking.groupby(['ZoneID']).cumcount()
# _z = cbd_parking.ZoneID.value_counts().sort_index().index
# CBD_PARKING_ZONES = dict(zip(_z, np.arange(len(_z))))
#
#
# def decumulate(x):
# 	x_ = np.array(x)
# 	x_[1:] -= x[:-1]
# 	return x_
# cbd_parking['Prob'] = cbd_parking.groupby("ZoneID")['CumProb'].transform(decumulate)
# cbd_parking['WeightedPrice'] = cbd_parking['Prob'] * cbd_parking['ThresholdPrice']
# cbd_parking_prices = cbd_parking.set_index(["ZoneID",'rownum']).ThresholdPrice.unstack()
# cbd_parking_price_prob = cbd_parking.set_index(["ZoneID",'rownum']).Prob.unstack()
#
#
#
#
# cbd_parking2 = pd.read_csv(
# 	filenames.HW_CBDPARK2,
# 	header=None,
#     names=[
#         'IncomeCeiling',
#         'FreeParkingPct',
#         'TransitPct',
#         'AutoOcc1Pct',
#         'AutoOcc2Pct',
#         'AutoOcc3Pct',
#         'AutoOcc4Pct',
#     ],
# )


# def parking_cost_cbd(
# 		ORIG,
# 		DEST,
# 		INCOME,
# 		HOURS,
# 		random_state=None,
# ):
# 	"""
# 	This function tries to exactly replicate the FORTRAN PRKCBD, right or wrong
#
# 	Parameters
# 	----------
# 	ORIG, DEST : int
# 		TAZ ID for origin, destination
# 	INCOME : array-like, shape[ITER]
# 	HOURS : numeric
# 		Number of hours of parking to pay for
# 	CBD_PARKING_ZONES : Mapping
# 		Maps destination zone numbers to CBD parking zone numbers
# 	random_state
#
# 	Returns
# 	-------
# 	CAPK, WALK3, INTOCC, BLK, SI : array, shape[ITER]
# 		parking cost, walktime, vehicle occupancy, blocks walked, savings rate
# 	"""
# 	global CBD_PARKING_ZONES
# 	random_state = check_random_state(random_state)
#
# 	ITER = INCOME.shape[0]
#
# 	J6 = np.zeros(ITER)
# 	J7 = np.ones(ITER)
# 	J3 = np.zeros(ITER, dtype=int)
# 	L = np.zeros(ITER, dtype=int)
#
# 	RAN2 = random_state.random(size=ITER)
# 	RAN3 = random_state.random(size=ITER) * 100. # Free Parking randomizer
# 	RAN4 = random_state.random(size=ITER)
# 	RAN5 = random_state.random(size=ITER) * 100. # Auto Occupancy randomizer
#
# 	# ORIG = pd.Series(ORIG)
# 	# DEST = pd.Series(DEST)
#
# 	PZONE = CBD_PARKING_ZONES[DEST]
# 	FREEPRK = np.empty(ITER)
# 	for j2 in reversed(range(5)):
# 		k = INCOME<cbd_parking2.IncomeCeiling.iloc[j2]
# 		L[k] = j2
# 		FREEPRK[k] = cbd_parking2.FreeParkingPct.iloc[j2]
#
# 	CBDPRK = cbd_parking.query(f"ZoneID=={DEST}").iloc[:,1:].reset_index(drop=True)
#
# 	for J in reversed(range(5)):
# 		J3[RAN4 <= CBDPRK.CumProb.iloc[J]] = J
#
# 	HCPT1 = CBDPRK.ThresholdPrice[J3].values
# 	HCPT2 = CBDPRK.ThresholdPrice[np.clip(J3-1,0,None)].values
# 	HCPT3 = CBDPRK.CumProb[J3].values
# 	HCPT4 = CBDPRK.CumProb[np.clip(J3-1,0,None)].values
# 	HCPT3_4 = HCPT3 - HCPT4
# 	HCPT3_4[HCPT3_4==0] = 1  # silence div by zero warning
#
# 	J7[J3 > 1] = 0
# 	J6[J7 == 0] = 1
#
# 	VT = INCOME/2400.
#
# 	# Baseline free parking
# 	CAPK = np.zeros(ITER, dtype=int)
# 	WALK3 = np.full(ITER, 3.0)
# 	HC = np.zeros(ITER)
# 	SI = np.zeros(ITER)
# 	WK = np.zeros(ITER)
# 	BLK = np.zeros(ITER)
#
# 	paid_parking = (RAN3 > FREEPRK)
# 	HC[paid_parking] = (
# 		J7 * CBDPRK.ThresholdPrice.iloc[0] +                     # when using first row
# 		J6 * (HCPT1 + (HCPT2-HCPT1)*(HCPT3-RAN2)/(HCPT3_4))      # when using other rows
# 	)[paid_parking]
# 	SI[paid_parking] = CBDPRK.SavePrice[J3].values[paid_parking]
# 	WK[paid_parking] = np.fmax(CBDPRK.WalkSeconds[J3].values[paid_parking], 180)
# 	BLK[paid_parking] = SI[paid_parking] / (VT[paid_parking] * WK[paid_parking] / 60.) # BLK IS BLOCKS WALKED
# 	BLK[paid_parking] = np.clip(BLK[paid_parking], 0.25, 6.0)
# 	WALK3[paid_parking] = (BLK[paid_parking] * WK[paid_parking]) / 60.
# 	CAPK[paid_parking] = np.clip(HC[paid_parking] * HOURS - BLK[paid_parking] * SI[paid_parking], 0.0, None)
#
# 	INTOCC = np.zeros(ITER, dtype=np.int8)
# 	RAN5 -= cbd_parking2.loc[L, 'AutoOcc1Pct'].values
# 	INTOCC[(INTOCC==0) & (RAN5 < 0)] = 1
# 	RAN5 -= cbd_parking2.loc[L, 'AutoOcc2Pct'].values
# 	INTOCC[(INTOCC==0) & (RAN5 < 0)] = 2
# 	RAN5 -= cbd_parking2.loc[L, 'AutoOcc3Pct'].values
# 	INTOCC[(INTOCC==0) & (RAN5 < 0)] = 3
# 	INTOCC[(INTOCC==0)] = 4
#
# 	return CAPK, WALK3, INTOCC, BLK, SI
#

def parking_cost_v2(
		dh,
		DEST,
		INCOME,
		HOURS,
		purpose,
		random_state=None,
):

	"""
	Draw a parking cost from the random distribution in the destination zone.

	Parameters
	----------
	DEST : array-like, shape[ITER]
		TAZ ID for destination
	INCOME : array-like, shape[ITER]
	HOURS : numeric
		Number of hours of parking to pay for
	random_state

	Returns
	-------
	CAPK, WALK3, INTOCC, BLK, SI : array, shape[ITER]
		parking cost, walktime, vehicle occupancy, blocks walked, savings rate
	"""
	global CBD_PARKING_ZONES, cbd_parking_price_prob, cbd_parking_prices
	random_state = check_random_state(random_state)

	DEST = pd.Series(DEST)
	ITER = DEST.size

	rand_free_parking = random_state.random(size=ITER) * 100.  # Free Parking randomizer
	rand_parking_price = random_state.random(size=ITER)        # Parking Rate randomizer
	RAN5 = random_state.random(size=ITER) * 100.         # Auto Occupancy randomizer

	price_probs = dh.cbd_parking_price_prob.reindex(DEST.values)
	prices = dh.cbd_parking_prices.reindex(DEST.values)
	parking_price_row = np.full(ITER, -1, dtype=np.int8)
	for i in range(5):
		rand_parking_price -= price_probs.iloc[:,0]
		parking_price_row[(parking_price_row < 0) & (rand_parking_price < 0)] = i

	# Assemble the randomly selected parking-price columns into one Series.
	# It will have NaN values for zones outside the CBD parking model
	parking_price_1 = pd.Series(
		prices.values[range(len(parking_price_row)), parking_price_row],
		index=DEST.index,
	)

	# Assemble default hourly pricing to use for trips based on zone type
	# This Series is len(Zones) not len(Trips)
	zonetype_price = dh.m01.zone_type.map(dh.cfg.parking_costs.defaults[purpose])

	# Fill in NaNs with default values.
	parking_price_2 = DEST.map(zonetype_price)
	parking_price = parking_price_1.fillna(parking_price_2)

	# Find free parking rate based on income group
	income_group = np.zeros(ITER, dtype=int)
	FREEPRK = np.zeros(ITER)
	for j2 in reversed(range(5)):
		k = INCOME<dh.cbd_parking2.IncomeCeiling.iloc[j2]
		income_group[k] = j2
		FREEPRK[k] = dh.cbd_parking2.FreeParkingPct.iloc[j2]

	# Zero out parking price if the traveler gets free parking
	free_parking = (rand_free_parking <= FREEPRK)
	parking_price[free_parking] = 0

	# Scale up based on number of hours of parking
	parking_price *= HOURS

	return parking_price, free_parking

	# PZONE = CBD_PARKING_ZONES[DEST]
	# FREEPRK = np.empty(ITER)
	# for j2 in reversed(range(5)):
	# 	k = INCOME<cbd_parking2.IncomeCeiling.iloc[j2]
	# 	L[k] = j2
	# 	FREEPRK[k] = cbd_parking2.FreeParkingPct.iloc[j2]
	#
	# CBDPRK = cbd_parking.query(f"ZoneID=={DEST}").iloc[:,1:].reset_index(drop=True)
	#
	# for J in reversed(range(5)):
	# 	J3[RAN4 <= CBDPRK.CumProb.iloc[J]] = J
	#
	# HCPT1 = CBDPRK.ThresholdPrice[J3].values
	# HCPT2 = CBDPRK.ThresholdPrice[np.clip(J3-1,0,None)].values
	# HCPT3 = CBDPRK.CumProb[J3].values
	# HCPT4 = CBDPRK.CumProb[np.clip(J3-1,0,None)].values
	# HCPT3_4 = HCPT3 - HCPT4
	# HCPT3_4[HCPT3_4==0] = 1  # silence div by zero warning
	#
	# J7[J3 > 1] = 0
	# J6[J7 == 0] = 1
	#
	# VT = INCOME/2400.
	#
	# # Baseline free parking
	# CAPK = np.zeros(ITER, dtype=int)
	# WALK3 = np.full(ITER, 3.0)
	# HC = np.zeros(ITER)
	# SI = np.zeros(ITER)
	# WK = np.zeros(ITER)
	# BLK = np.zeros(ITER)
	#
	# paid_parking = (RAN3 > FREEPRK)
	# HC[paid_parking] = (
	# 	J7 * CBDPRK.ThresholdPrice.iloc[0] +                     # when using first row
	# 	J6 * (HCPT1 + (HCPT2-HCPT1)*(HCPT3-RAN2)/(HCPT3_4))      # when using other rows
	# )[paid_parking]
	# SI[paid_parking] = CBDPRK.SavePrice[J3].values[paid_parking]
	# WK[paid_parking] = np.fmax(CBDPRK.WalkSeconds[J3].values[paid_parking], 180)
	# BLK[paid_parking] = SI[paid_parking] / (VT[paid_parking] * WK[paid_parking] / 60.) # BLK IS BLOCKS WALKED
	# BLK[paid_parking] = np.clip(BLK[paid_parking], 0.25, 6.0)
	# WALK3[paid_parking] = (BLK[paid_parking] * WK[paid_parking]) / 60.
	# CAPK[paid_parking] = np.clip(HC[paid_parking] * HOURS - BLK[paid_parking] * SI[paid_parking], 0.0, None)
	#
	# INTOCC = np.zeros(ITER, dtype=np.int8)
	# RAN5 -= cbd_parking2.loc[L, 'AutoOcc1Pct'].values
	# INTOCC[(INTOCC==0) & (RAN5 < 0)] = 1
	# RAN5 -= cbd_parking2.loc[L, 'AutoOcc2Pct'].values
	# INTOCC[(INTOCC==0) & (RAN5 < 0)] = 2
	# RAN5 -= cbd_parking2.loc[L, 'AutoOcc3Pct'].values
	# INTOCC[(INTOCC==0) & (RAN5 < 0)] = 3
	# INTOCC[(INTOCC==0)] = 4
	#
	# return CAPK, WALK3, INTOCC, BLK, SI


def parking_cost_v3(
		dh,
		DEST,
		HOURS,
		purpose,
		random_state=None,
):

	"""
	Draw a parking cost from the random distribution in the destination zone.

	Parameters
	----------
	dh : DataHandler
	DEST : array-like, shape[ITER]
		TAZ ID for destination
	HOURS : numeric
		Number of hours of parking to pay for
	purpose : str
		Used to determine the default price per hour by zonetype.

	random_state

	Returns
	-------
	CAPK, WALK3, INTOCC, BLK, SI : array, shape[ITER]
		parking cost, walktime, vehicle occupancy, blocks walked, savings rate
	"""
	global CBD_PARKING_ZONES, cbd_parking_price_prob, cbd_parking_prices
	random_state = check_random_state(random_state)

	DEST = pd.Series(DEST.reshape(-1))
	ITER = DEST.size

	rand_free_parking = random_state.random(size=ITER) * 100.  # Free Parking randomizer
	rand_parking_price = random_state.random(size=ITER)        # Parking Rate randomizer

	price_probs = dh.cbd_parking_price_prob.reindex(DEST.values)
	prices = dh.cbd_parking_prices.reindex(DEST.values)
	parking_price_row = np.full(ITER, -1, dtype=np.int8)
	for i in range(5):
		rand_parking_price -= price_probs.iloc[:,0]
		parking_price_row[(parking_price_row < 0) & (rand_parking_price < 0)] = i

	# Assemble the randomly selected parking-price columns into one Series.
	# It will have NaN values for zones outside the CBD parking model
	parking_price_1 = pd.Series(
		prices.values[range(len(parking_price_row)), parking_price_row],
		index=DEST.index,
	)

	# Assemble default hourly pricing to use for trips based on zone type
	# This Series is len(Zones) not len(Trips)
	zonetype_price = dh.m01.zone_type.map(dh.cfg.parking_costs.defaults[purpose])

	# Fill in NaNs with default values.
	parking_price_2 = DEST.map(zonetype_price)
	parking_price = parking_price_1.fillna(parking_price_2)

	# Scale up based on number of hours of parking
	parking_price *= HOURS

	return parking_price


def parking_is_free(
		dh,
		INCOME,
		random_state=None,
):

	"""
	Draw a parking cost from the random distribution in the destination zone.

	Parameters
	----------
	dh : DataHandler
	INCOME : array-like, shape[ITER]
		Income groups for each draw, used to find if parking is actually free
		Values are categories 1 to 5
	random_state

	Returns
	-------
	paid_parking : array, shape[ITER]
		1 if parking is paid, 0 if free
	"""
	random_state = check_random_state(random_state)
	rand_free_parking = random_state.random(size=INCOME.size) * 100.  # Free Parking randomizer

	# Find free parking rate based on income group
	FREEPRK = dh.cbd_parking2.FreeParkingPct.iloc[INCOME-1]

	# random draw if the traveler gets free parking
	paid_parking = (rand_free_parking > FREEPRK)

	return paid_parking
