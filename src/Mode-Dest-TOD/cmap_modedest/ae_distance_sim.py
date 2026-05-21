import numpy as np
import pandas as pd
from scipy import stats
import logging
log = logging.getLogger('CMAP')

def sloped_linear(x_min, x_max, ratio=1.0, random_state=None, replication=1):
	if isinstance(replication, int):
		replication = [replication]
	x_max = np.asarray(x_max)
	x_min = np.asarray(x_min)
	reps = list(x_min.shape)+replication
	if isinstance(ratio, float) and ratio == 1.0:
		return stats.uniform(x_min, x_max).rvs(reps, random_state=random_state)
	else:
		span = x_max - x_min
		slope = (1.0-ratio)/np.where(span!=0, span, 1.0)
		area =.5*(1+ratio)*span
		if len(replication) == 1:
			y = stats.uniform().rvs(reps, random_state=random_state)*area[:,None]
			zp = np.sqrt(ratio[:,None] ** 2 + 2 * slope[:,None] * y)
			return (zp - ratio[:,None]) / np.where(slope != 0, slope, 1.0)[:,None] + x_min[:,None]
		elif len(replication) == 2:
			y = stats.uniform().rvs(reps, random_state=random_state)*area[:,None,None]
			zp = np.sqrt(ratio[:,None,None] ** 2 + 2 * slope[:,None,None] * y)
			return (zp - ratio[:,None,None]) / np.where(slope != 0, slope, 1.0)[:,None,None] + x_min[:,None,None]
		else:
			raise ValueError('too many dims')


def trunc_normal(*arg, random_state=None, replication=1):
	if isinstance(replication, int):
		replication = [replication]
	rv = stats.norm(*(np.asarray(j) for j in arg))
	reps = replication+list(rv.args[0].shape)
	return np.fmax(rv.rvs(reps, random_state=random_state), 0.06)


def simulate_ae_dist(p1,p2,p3, random_state=None, replication=1):
	p1 = np.asarray(p1).reshape(-1)
	p2 = np.asarray(p2).reshape(-1)
	p3 = np.asarray(p3).reshape(-1)
	if isinstance(replication, int):
		replication = [replication]
	reps = list(p1.shape)+replication
	result = np.full(reps, np.nan, dtype=np.float32)

	use_normal = (p3 == 101)
	if use_normal.any():
		result[use_normal,...] = trunc_normal(
			p1[use_normal],
			p2[use_normal],
			random_state=random_state,
			replication=replication,
		).T

	use_slopey = (p3 < 101)
	n = use_slopey.sum()
	if n:
		result[use_slopey,...] = sloped_linear(
			p1[use_slopey],
			p2[use_slopey],
			p3[use_slopey],
			random_state=random_state,
			replication=replication,
		)

	use_nan = (p3 == 999)
	result[use_nan,...] = 255.0
	return result


ae_mode_speeds = { # minutes per mile
	1: 20, # walk
	2: 20, # bus
	3: 2, # pnr
	4: 2, # knr
	5: 20, # feeder
	999: 1, # not available
}