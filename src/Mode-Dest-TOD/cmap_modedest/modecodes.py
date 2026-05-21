import numpy as np
from .addict import Dict

mode5names = ['AUTO', 'TAXI', 'TNC1', 'TNC2', 'TRANSIT']
mode5codes = Dict(zip(
	mode5names,
	np.arange(len(mode5names)) + 1,
))

mode7names = ['AUTO', 'TAXI', 'TNC1', 'TNC2', 'TRANSIT', 'WALK', 'BIKE']
mode7codes = Dict(zip(
	mode7names,
	np.arange(len(mode7names)) + 1,
))

mode9names = ['AUTO', 'HOV2', 'HOV3', 'TAXI', 'TNC1', 'TNC2', 'TRANSIT', 'WALK', 'BIKE']
mode9codes = Dict(zip(
	mode9names,
	np.arange(len(mode9names)) + 1,
))


TransitModeCode_CTA_REGULAR_BUS = 4
TransitModeCode_CTA_EXPRESS_BUS = 5
TransitModeCode_PACE_BUS = 6
TransitModeCode_CTA_RAIL = 7
TransitModeCode_METRA_RAIL = 8

N_TRANSIT_MODES = 8


APPROACH_WALK = 0
APPROACH_BUS = 1
APPROACH_PARK_N_RIDE = 2
APPROACH_KISS_N_RIDE = 3
APPROACH_FEEDER_BUS = 4

N_APPROACH_MODES = 5
APPROACH_MODE_NAMES = {
	APPROACH_WALK: 'Walk',
	APPROACH_BUS: 'Bus',
	APPROACH_PARK_N_RIDE: 'PnR',
	APPROACH_KISS_N_RIDE: 'KnR',
	APPROACH_FEEDER_BUS: 'Feeder',
}

DIST_TO_BUS = 0
DIST_TO_CTA_RAIL = 1
DIST_TO_METRA = 2
DIST_TO_FEEDER_BUS = 3
DIST_TO_PARK_N_RIDE_STATION = 4

N_DIST_TO_TYPES = 5
DIST_TO_NAMES = {
	DIST_TO_BUS: 'to Bus',
	DIST_TO_CTA_RAIL: 'to El',
	DIST_TO_METRA: 'to Metra',
	DIST_TO_FEEDER_BUS: 'to Feeder',
	DIST_TO_PARK_N_RIDE_STATION: 'to PnR',
}