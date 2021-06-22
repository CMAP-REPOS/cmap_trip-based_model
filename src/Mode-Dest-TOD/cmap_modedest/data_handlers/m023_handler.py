import pandas as pd
import numpy as np
from ..addict import Dict

def read_m023(filename):
	m023 = Dict()
	with open(filename, 'rt') as f:
		line = f.readline().split()
		m023.CTA_BUS_BOARDING_FARE = int(line[0])
		m023.CTA_RAIL_BOARDING_FARE = int(line[1])
		m023.CTA_FIRST_XFER_FARE = int(line[2])
		m023.CTA_CBD_LINK_UP_FARE = int(line[3])

		line = f.readline().split()
		m023.FEEDER_BUS_BOARDING_FARE = int(line[0])
		m023.FEEDER_BUS_CBD_FARE = int(line[1])

		line = f.readline().split()
		m023.PACE_BUS_BOARDING_FARE = int(line[0])
		m023.PACE_BUS_FIRST_XFER_FARE = int(line[1])

		line = f.readline().split()
		m023.AUTO_OPERATING_COST_BY_SPEED = [int(j) for j in line[:8]]
		line = f.readline().split()
		m023.AUTO_OPERATING_COST_BY_SPEED.extend([int(j) for j in line[:8]])
		m023.AUTO_OPERATING_COST_BY_SPEED = np.asarray(m023.AUTO_OPERATING_COST_BY_SPEED)

		line = f.readline().split()
		m023.AUTO_OPERATING_COST_BY_ZONETYPE = [int(j) for j in line[:4]]
		m023.AUTO_OPERATING_COST_BY_ZONETYPE = np.asarray(m023.AUTO_OPERATING_COST_BY_ZONETYPE)

	return m023


def load_m023(filenames):

	m023 = read_m023(filenames.MCHW_M023)
	return m023

