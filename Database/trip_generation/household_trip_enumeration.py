"""
Filename: household_trip_enumeration.py
Author: Craig Heither
Description: This function completes the household trip enumeration process
             for trip generation.
"""

import numpy as np

def tripEnumeration(starts, ends, selection_array, probability_array, 
                    rng=None, households_chosen=None, replicateMax=3000, 
                    resampleMax=200):
    """
    Select travel survey households to attach daily trips to synthetic households.

    Parameters
    ----------
    starts : array_like
        0-based inclusive start indices (length N)
    ends : array_like
        0-based inclusive end indices (length N)
    selection_array : ndarray
        1D array of survey identifiers (length M)
    probability_array : ndarray
        1D array of probabilities for each survey (length M)
    rng : numpy.random.Generator, optional
        NumPy Generator instance for reproducible draws. If None, uses
        np.random.default_rng().
    households_chosen : dict, optional
        Dictionary to track selected households and their replication counts.
        If None, no resampling occurs when thresholds are reached.
    replicateMax : int, optional
        Maximum times a specific survey household can be matched to synthetic
        households before resampling occurs. Default is 3000.
    resampleMax : int, optional
        Maximum times resampling will occur once replicateMax has been reached.
        Default is 200.

    Returns
    -------
    ndarray
        Integer array (dtype int64) of length N with selected survey identifiers.
    """
    # Use provided generator or default
    if rng is None:
        rng = np.random.default_rng()

    # Read inputs into numpy arrays, + 1 to ends to be inclusive of selection
    # Example: array = [1,2,3,4,5,6,7]
    # the slice for (1,2,3) is [0:3], slice for (4,5,6,7) is [3:7]
    starts = np.asarray(starts, dtype=np.int64)
    ends = np.asarray(ends + 1, dtype=np.int64)
    selection_array = np.asarray(selection_array).astype(np.int64)
    probability_array = np.asarray(probability_array).astype(float)

    # Cumulative probabilities with a leading zero for easy slice arithmetic
    cumul_prob = np.concatenate(([0.0], np.cumsum(probability_array)))

    # Compute total mass for each slice
    masses = cumul_prob[ends] - cumul_prob[starts]

    # Draw uniform random values within each slice's mass using RNG
    u = rng.random(size=masses.shape) * masses + cumul_prob[starts]
    # Map cumulative draws to travel survey household index
    idx = np.searchsorted(cumul_prob, u, side='right') - 1
    selected_households = selection_array[idx].astype(np.int64)

    # Place current selections into output
    keep_output = selected_households

    # Update counts and resample if threshold reached
    if households_chosen is not None:
        for survey_id in selected_households:
            households_chosen[survey_id] = households_chosen.get(survey_id, 0) + 1
            ### households_chosen[survey_id] += 1

        # Identify households needing resample
        needs_resample = np.array(
            [households_chosen.get(int(survey_id), 0) > replicateMax for 
             survey_id in selected_households], dtype=bool,
        )
        if np.any(needs_resample):
            # Iterate only on houseohlds above threshold
            for i, need in enumerate(needs_resample):
                if not need:
                    continue

                start_i = int(starts[i])
                end_i = int(ends[i])
                choices = selection_array[start_i : end_i]
                probs = probability_array[start_i : end_i]
                total_p = np.nansum(probs)

                if not np.isfinite(total_p) or total_p <= 0:
                    # Uniform if invalid probabilities
                    new_sel = int(rng.choice(choices))
                else:
                    probs = probs / total_p
                    new_sel = int(selected_households[i])  # default to current
                    for _ in range(resampleMax):
                        candidate = int(rng.choice(choices, p=probs))
                        if households_chosen.get(candidate, 0) < replicateMax:
                            # Roll back previous count of selected[i]
                            households_chosen[int(selected_households[i])] -= 1
                            # Use the candidate and increment its count
                            new_sel = candidate
                            households_chosen[new_sel] = households_chosen.get(new_sel, 0) + 1
                            break
                    # If we never found below-threshold, keep the last selection (new_sel)
                selected_households[i] = new_sel

            # Write back selections to out for the resampled subset
            keep_output = selected_households

    return keep_output.astype(np.int64)