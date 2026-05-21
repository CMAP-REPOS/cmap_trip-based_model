"""
Filename: vehicle_availability.py
Author: Craig Heither
Description: These functions are the household vehicle availability model 
             for trip generation.
"""

import numpy as np

def vehOwnOneAdult(rows, rng=None):
    """
    Vehicle ownership model for 1-adult households.

    Parameters
    ----------
    rows : array_like
        Six inputs: [sidewalk_density, row_column, age_category, workers,
                     income_category, auto_commute_share]
    rng : numpy.random.Generator, optional
        A Numpy Generator instance for reproducible results. If None, uses
        np.random.default_rng().

    Returns
    -------
    ndarray
        Integer array of vehicle counts (0..2).
    """
    
    # Read household parameters
    rows = np.asarray(rows)
    if rows.ndim == 1:
        rows = rows.reshape(1, -1)
    sidewalk = rows[:, 0].astype(float)
    rowcol = rows[:, 1]
    age = rows[:, 2]
    workers = rows[:, 3]
    income = rows[:, 4]
    auto_comm = rows[:, 5].astype(float)
    if rng is None:
        rng = np.random.default_rng()

    # Lookup tables
    bias1_lookup = np.array([-2.600, -2.676, -2.869, -3.082])
    bias2_lookup = np.array([-5.077, -4.823, -4.914, -4.984])
    hh_lookup = np.array([0.392, 0.401, 0.249])
    hh2_lookup = np.array([0.394, 0.465, 0.218])

    # Safe indexing (rowcol and age are 1-based)
    rowcol_idx = np.clip(rowcol.astype(int) - 1, 0, 3)
    age_idx = np.clip(age.astype(int) - 1, 0, 2)

    # Utility for zero vehicles
    COEF_PEF0 = 0.06165
    UTIL0 = COEF_PEF0 * sidewalk

    # Utility for one vehicle
    COEF_WORKER = 0.4731
    COEF_HINC2 = 1.182
    COEF_HINC3 = 0.9910
    COEF_AUTOMS = 4.677
    COEF_PEF1 = 0.03188
    
    bias1 = bias1_lookup[rowcol_idx]
    hh_bias = hh_lookup[age_idx]
    temp1 = (workers.astype(int) == 1).astype(int)
    temp2 = (income.astype(int) != 1).astype(int)
    temp3 = (income.astype(int) >= 3).astype(int)

    UTIL1 = (COEF_WORKER * temp1 + COEF_HINC2 * temp2 + COEF_HINC3 * temp3
             + COEF_AUTOMS * auto_comm + COEF_PEF1 * sidewalk + bias1 + hh_bias)
    
    # Utility for two or more vehicles
    COEF_WORKER = 0.4731
    COEF_HINC2 = 1.766
    COEF_HINC3 = 1.690
    COEF_HINC4 = 0.4668
    COEF_AUTOMS = 4.677

    bias2 = bias2_lookup[rowcol_idx]
    hh_bias2 = hh2_lookup[age_idx]
    temp1 = (workers.astype(int) == 1).astype(int)
    temp2 = (income.astype(int) != 1).astype(int)
    temp3 = (income.astype(int) >= 3).astype(int)
    temp4 = (income.astype(int) == 4).astype(int)

    UTIL2 = (COEF_WORKER * temp1 + COEF_HINC2 * temp2 + COEF_HINC3 * temp3
             + COEF_HINC4 * temp4 + COEF_AUTOMS * auto_comm + bias2 + hh_bias2)
    
    # Vehicle ownership probabilities
    expU = np.exp(np.vstack([UTIL0, UTIL1, UTIL2]).T)
    denom = expU.sum(axis=1, keepdims=True)
    probs = expU / denom
    cumul_probs = probs.cumsum(axis=1)

    # Determine vehicle ownership level
    u = rng.random(size=cumul_probs.shape[0])
    # Vectorized row-wise mapping: count how many cumulative probs are < u
    idx = np.sum(u[:, None] > cumul_probs, axis=1)
    # idx maps to 0->0 vehicles, 1->1, 2->2+
    return idx.astype(int)

def vehOwnTwoAdult(rows, rng=None):
    """
    Vehicle ownership model for 2-adult households.

    Parameters
    ----------
    rows : array_like
        Seven inputs: [sidewalk_density, row_column, age_category, workers,
                       income_category, auto_commute_share, children]
    rng : numpy.random.Generator, optional
        A Numpy Generator instance for reproducible results. If None, uses
        np.random.default_rng().

    Returns
    -------
    ndarray
        Integer array of vehicle counts (0..3).
    """
    rows = np.asarray(rows)
    if rows.ndim == 1:
        rows = rows.reshape(1, -1)
    sidewalk = rows[:, 0].astype(float)
    rowcol = rows[:, 1]
    age = rows[:, 2]
    workers = rows[:, 3].astype(int)
    income = rows[:, 4].astype(int)
    auto_comm = rows[:, 5].astype(float)
    children = rows[:, 6].astype(float)
    if rng is None:
        rng = np.random.default_rng()
    
    # Lookup tables
    bias1_lookup = np.array([2.018, 2.259, 2.151, 1.925])
    bias2_lookup = np.array([-2.827, -2.637, -2.728, -3.144])
    bias3_lookup = np.array([-4.393, -3.944, -4.126, -4.302])
    hh_lookup = np.array([0.392, 0.401, 0.249])
    hh2_lookup = np.array([0.394, 0.465, 0.218])
    hh3_lookup = np.array([0.403, 0.574, 0.007])

    # Safe indexing (rowcol and age are 1-based)
    rowcol_idx = np.clip(rowcol.astype(int) - 1, 0, 3)
    age_idx = np.clip(age.astype(int) - 1, 0, 2)

    # Utility for zero vehicles
    COEF_PEF = 0.1280
    UTIL0 = COEF_PEF * sidewalk

    # Utility for one vehicle
    COEF_HINC2 = 1.702
    COEF_PEF = 0.06309
    bias1 = bias1_lookup[rowcol_idx]
    hh_bias = hh_lookup[age_idx]
    temp1 = (income != 1).astype(int)

    UTIL1 = COEF_HINC2 * temp1 + COEF_PEF * sidewalk + bias1 + hh_bias

    # Utility for two vehicles
    COEF_WORK1 = 0.6940
    COEF_WORK2 = 0.5198
    COEF_HINC2 = 2.466
    COEF_HINC3 = 0.8650
    COEF_HINC4 = 0.4517
    COEF_AUTOMS = 5.284
    COEF_CHILD = 0.2218
    COEF_PEF = 0.03359

    bias2 = bias2_lookup[rowcol_idx]
    hh_bias2 = hh2_lookup[age_idx]
    temp1 = (workers > 0).astype(int)
    temp2 = (workers > 1).astype(int)
    temp3 = (income > 1).astype(int)
    temp4 = (income > 2).astype(int)
    temp5 = (income == 4).astype(int)

    UTIL2 = (COEF_WORK1 * temp1 + COEF_WORK2 * temp2 + COEF_HINC2 * temp3
             + COEF_HINC3 * temp4 + COEF_HINC4 * temp5 + COEF_AUTOMS * auto_comm
             + COEF_CHILD * children + COEF_PEF * sidewalk + bias2 + hh_bias2)
    
    # Utility for three plus vehicles
    COEF_WORK1 = 0.6940
    COEF_WORK2 = 0.5198
    COEF_HINC2 = 2.466
    COEF_HINC3 = 0.8650
    COEF_HINC4 = 0.8827
    COEF_AUTOMS = 5.284

    bias3 = bias3_lookup[rowcol_idx]
    hh_bias3 = hh3_lookup[age_idx]
    temp1 = (workers > 0).astype(int)
    temp2 = (workers > 1).astype(int)
    temp3 = (income > 1).astype(int)
    temp4 = (income > 2).astype(int)
    temp5 = (income == 4).astype(int)

    UTIL3 = (COEF_WORK1 * temp1 + COEF_WORK2 * temp2 + COEF_HINC2 * temp3
             + COEF_HINC3 * temp4 + COEF_HINC4 * temp5 + COEF_AUTOMS * auto_comm
             + bias3 + hh_bias3)
    
    # Vehicle ownership probabilities
    expU = np.exp(np.vstack([UTIL0, UTIL1, UTIL2, UTIL3]).T)
    denom = expU.sum(axis=1, keepdims=True)
    probs = expU / denom
    cumul_probs = probs.cumsum(axis=1)

    # Determine vehicle ownership level
    u = rng.random(size=cumul_probs.shape[0])
    # Vectorized row-wise mapping: count how many cumulative probs are < u
    idx = np.sum(u[:, None] > cumul_probs, axis=1)
    return idx.astype(int)

def vehOwnThreeAdult(rows, rng=None):
    """
    Vehicle ownership model for 3-or-more-adult households.

    Parameters
    ----------
    rows : array_like
        Seven inputs: [sidewalk_density, row_column, age_category, workers,
                       income_category, auto_commute_share, nonworkers]
    rng : numpy.random.Generator, optional
        A Numpy Generator instance for reproducible results. If None, uses
        np.random.default_rng().

    Returns
    -------
    ndarray
        Integer array of vehicle counts (0..3).
    """
    rows = np.asarray(rows)
    if rows.ndim == 1:
        rows = rows.reshape(1, -1)
    sidewalk = rows[:, 0].astype(float)
    rowcol = rows[:, 1]
    age = rows[:, 2]
    workers = rows[:, 3].astype(int)
    income = rows[:, 4].astype(int)
    auto_comm = rows[:, 5].astype(float)
    nonworkers = rows[:, 6].astype(float)
    if rng is None:
        rng = np.random.default_rng()

    # Lookup tables
    bias1_lookup = np.array([2.806, 2.552, 1.547, 2.272])
    bias2_lookup = np.array([-1.836, -2.139, -2.783, -2.430])
    bias3_lookup = np.array([-1.631, -1.789, -2.668, -2.278])
    hh_lookup = np.array([0.392, 0.401, 0.249])
    hh2_lookup = np.array([0.394, 0.465, 0.218])
    hh3_lookup = np.array([0.403, 0.574, 0.007])

    # Safe indexing (rowcol and age are 1-based)
    rowcol_idx = np.clip(rowcol.astype(int) - 1, 0, 3)
    age_idx = np.clip(age.astype(int) - 1, 0, 2)

    # Utility for zero vehicles
    COEF_PEF = 0.1703
    UTIL0 = COEF_PEF * sidewalk

    # Utility for one vehicle
    COEF_WORK1 = 1.114
    COEF_HINC2 = 0.9492
    COEF_PEF = 0.06586
    
    bias1 = bias1_lookup[rowcol_idx]
    hh_bias = hh_lookup[age_idx]
    temp1 = (workers > 0).astype(int)
    temp2 = (income > 1).astype(int)

    UTIL1 = (COEF_WORK1 * temp1 + COEF_HINC2 * temp2 + COEF_PEF * sidewalk
             + bias1 + hh_bias)
    
    # Utility for two vehicles
    COEF_WORK1 = 1.114
    COEF_WORK2 = 0.7934
    COEF_HINC2 = 1.487
    COEF_HINC3 = 0.8723
    COEF_HINC4 = 1.390
    COEF_AUTOMS = 4.959
    COEF_PEF2 = 0.06586

    bias2 = bias2_lookup[rowcol_idx]
    hh_bias2 = hh2_lookup[age_idx]
    temp1 = (workers > 0).astype(int)
    temp2 = (workers > 1).astype(int)
    temp3 = (income > 1).astype(int)
    temp4 = (income > 2).astype(int)
    temp5 = (income == 4).astype(int)

    UTIL2 = (COEF_WORK1 * temp1 + COEF_WORK2 * temp2 + COEF_HINC2 * temp3
             + COEF_HINC3 * temp4 + COEF_HINC4 * temp5 + COEF_AUTOMS * auto_comm
             + COEF_PEF2 * sidewalk + bias2 + hh_bias2)

    # Utility for three plus vehicles
    COEF_WORK1 = 1.114
    COEF_WORK2 = 0.7934
    COEF_WORK3 = 1.389
    COEF_HINC2 = 1.487
    COEF_HINC3 = 1.571
    COEF_HINC4 = 1.834
    COEF_ADULT = 0.1491
    COEF_AUTOMS = 4.959

    bias3 = bias3_lookup[rowcol_idx]
    hh_bias3 = hh3_lookup[age_idx]
    temp1 = (workers > 0).astype(int)
    temp2 = (workers > 1).astype(int)
    temp3 = (workers > 2).astype(int)
    temp4 = (income > 1).astype(int)
    temp5 = (income > 2).astype(int)
    temp6 = (income == 4).astype(int)

    UTIL3 = (COEF_WORK1 * temp1 + COEF_WORK2 * temp2 + COEF_WORK3 * temp3 
             + COEF_HINC2 * temp4 + COEF_HINC3 * temp5 + COEF_HINC4 * temp6
             + COEF_ADULT * nonworkers + COEF_AUTOMS * auto_comm + bias3 + hh_bias3)
    
   # Vehicle ownership probabilities
    expU = np.exp(np.vstack([UTIL0, UTIL1, UTIL2, UTIL3]).T)
    denom = expU.sum(axis=1, keepdims=True)
    probs = expU / denom
    cumul_probs = probs.cumsum(axis=1)

    # Determine vehicle ownership level
    u = rng.random(size=cumul_probs.shape[0])
    # Vectorized row-wise mapping: count how many cumulative probs are < u
    idx = np.sum(u[:, None] > cumul_probs, axis=1)
    return idx.astype(int)