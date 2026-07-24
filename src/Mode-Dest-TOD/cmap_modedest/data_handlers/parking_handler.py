import os

import numpy as np
import pandas as pd
from ..addict import Dict

def load_cbd_parking(filenames):

	if os.path.exists(filenames.HW_CBDPARK0):
		# split the HW_CBDPARK file into its two primary parts
		with open(filenames.HW_CBDPARK0, 'rt') as f:
			lines = f.readlines()
		def secondpart(line):
			beginning = line.split(",", 1)[0]
			try:
				beginning = int(beginning)
			except:
				return 0
			if beginning < 10_000:
				return 0
			return 1
		with open(filenames.HW_CBDPARK, 'wt') as f:
			f.write("".join(i for i in lines if not secondpart(i)))
		with open(filenames.HW_CBDPARK2, 'wt') as f:
			f.write("".join(i for i in lines if secondpart(i)))

	cbd_parking = pd.read_csv(
		filenames.HW_CBDPARK,
		header=None,
		names=['ZoneID', 'CumProb', 'ThresholdPrice', 'SavePrice', 'WalkSeconds'],
	)
	cbd_parking.CumProb /= 10000.
	# cbd_parking['SumPrice'] = cbd_parking.ThresholdPrice + cbd_parking.SavePrice
	cbd_parking['rownum'] = cbd_parking.groupby(['ZoneID']).cumcount()

	def decumulate(x):
		x_ = np.array(x)
		x_[1:] -= x[:-1]
		return x_
	cbd_parking['Prob'] = cbd_parking.groupby("ZoneID")['CumProb'].transform(decumulate)
	cbd_parking['WeightedPrice'] = cbd_parking['Prob'] * cbd_parking['ThresholdPrice']
	cbd_parking_prices = cbd_parking.set_index(["ZoneID",'rownum']).ThresholdPrice.unstack()
	cbd_parking_price_prob = cbd_parking.set_index(["ZoneID",'rownum']).Prob.unstack()

	for zone_to, zone_source in filenames.cfg.parking_costs.cbd_nearby.items():
		cbd_parking_prices.loc[zone_to] = cbd_parking_prices.loc[zone_source]
		cbd_parking_price_prob.loc[zone_to] = cbd_parking_price_prob.loc[zone_source]

	cbd_parking_prices.sort_index(inplace=True)
	cbd_parking_price_prob.sort_index(inplace=True)

	CBD_PARKING_ZONES = dict(zip(cbd_parking_prices.index, np.arange(len(cbd_parking_prices.index))))

	cbd_parking2 = pd.read_csv(
		filenames.HW_CBDPARK2,
		header=None,
		names=[
			'IncomeCeiling',
			'FreeParkingPct',
			'TransitPct',
			'AutoOcc1Pct',
			'AutoOcc2Pct',
			'AutoOcc3Pct',
			'AutoOcc4Pct',
		],
	)

	parking = Dict()
	parking.cbd_parking = cbd_parking
	parking.cbd_parking_prices = cbd_parking_prices
	parking.cbd_parking_price_prob = cbd_parking_price_prob
	parking.cbd_parking2 = cbd_parking2
	parking.CBD_PARKING_ZONES = CBD_PARKING_ZONES
	return parking
