import numpy as np
import numba as nb

def compile_parking_cost_v2(dh):

    cbd_parking_price_prob_array = np.zeros([dh.cbd_parking_price_prob.index.max(), 5], dtype=np.float32)
    for dtaz, priceprob in dh.cbd_parking_price_prob.iterrows():
        cbd_parking_price_prob_array[dtaz-1, :] = priceprob.to_numpy()

    cbd_parking_prices_array = np.zeros([dh.cbd_parking_price_prob.index.max(), 5], dtype=np.float32)
    for dtaz, prices in dh.cbd_parking_prices.iterrows():
        cbd_parking_prices_array[dtaz-1, :] = prices.to_numpy()

    zone_type_array = dh.m01.zone_type.to_numpy().astype(np.int32)

    default_parking_costs_array = np.zeros([
        4, # purposes
        4, # zonetypes
    ])

    default_parking_costs_array[0, 0] = dh.cfg.parking_costs.defaults.HW[1]
    default_parking_costs_array[0, 1] = dh.cfg.parking_costs.defaults.HW[2]
    default_parking_costs_array[0, 2] = dh.cfg.parking_costs.defaults.HW[3]
    default_parking_costs_array[0, 3] = dh.cfg.parking_costs.defaults.HW[4]

    default_parking_costs_array[1, 0] = dh.cfg.parking_costs.defaults.HW[1]
    default_parking_costs_array[1, 1] = dh.cfg.parking_costs.defaults.HW[2]
    default_parking_costs_array[1, 2] = dh.cfg.parking_costs.defaults.HW[3]
    default_parking_costs_array[1, 3] = dh.cfg.parking_costs.defaults.HW[4]

    default_parking_costs_array[2, 0] = dh.cfg.parking_costs.defaults.HO[1]
    default_parking_costs_array[2, 1] = dh.cfg.parking_costs.defaults.HO[2]
    default_parking_costs_array[2, 2] = dh.cfg.parking_costs.defaults.HO[3]
    default_parking_costs_array[2, 3] = dh.cfg.parking_costs.defaults.HO[4]

    default_parking_costs_array[3, 0] = dh.cfg.parking_costs.defaults.NH[1]
    default_parking_costs_array[3, 1] = dh.cfg.parking_costs.defaults.NH[2]
    default_parking_costs_array[3, 2] = dh.cfg.parking_costs.defaults.NH[3]
    default_parking_costs_array[3, 3] = dh.cfg.parking_costs.defaults.NH[4]

    IncomeCeiling = dh.cbd_parking2.IncomeCeiling.to_numpy()
    FreeParkingPct = dh.cbd_parking2.FreeParkingPct.to_numpy()

    @nb.njit
    def parking_cost_v2(
            DEST,
            INCOME,
            HOURS,
            purpose,
            random_seed=None,
    ):

        """
        Draw a parking cost from the random distribution in the destination zone.

        Parameters
        ----------
        DEST : int
            TAZ ID for destination
        INCOME : float
        HOURS : float
            Number of hours of parking to pay for
        purpose : int
            0:'HW', 1:'HW, 2:'HO', 3:'NH'
        random_seed : int, optional

        Returns
        -------
        CAPK, WALK3, INTOCC, BLK, SI : array, shape[ITER]
            parking cost, walktime, vehicle occupancy, blocks walked, savings rate
        """

        if random_seed is not None:
            np.random.seed(random_seed)

        rand_free_parking = np.random.rand() * 100.  # Free Parking randomizer
        rand_parking_price = np.random.rand()        # Parking Rate randomizer
        #RAN5 = np.random.rand() * 100.               # Auto Occupancy randomizer

        hourly_cost = 0
        if DEST <= cbd_parking_price_prob_array.shape[0]:
            for i in range(5):
                rand_parking_price -= cbd_parking_price_prob_array[DEST-1, i]
                if rand_parking_price < 0:
                    hourly_cost = cbd_parking_prices_array[DEST-1, i]
                    break

        if hourly_cost == 0:
            dest_zone_type = zone_type_array[DEST-1]
            hourly_cost = default_parking_costs_array[purpose, dest_zone_type-1]

        free_parking = False
        for j2 in range(5):
            if INCOME <= IncomeCeiling[j2]:
                if rand_free_parking <= FreeParkingPct[j2]:
                    hourly_cost = 0.0
                    free_parking = True
                break

        return hourly_cost*HOURS, free_parking

    return parking_cost_v2