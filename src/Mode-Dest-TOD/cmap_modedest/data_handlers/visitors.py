import numpy as np
import pandas as pd
import logging
from ..util import search_path


def load_visitor_trips(filenames, scale_factor=1.0):
    """
    Generate visitor trips.

    The raw visitor table includes fractional trips.
    This function converts to integer trips by making
    random draws from the trip table.

    Parameters
    ----------
    filenames : Filenames
    scale_factor : float, default 1.0

    Returns
    -------
    trips : pd.DataFrame
        A square trip table with integer O-D trips by visitors
    """
    filename = search_path(
        filenames.cache_dir / "CMAP_TripTable_VisitorsWeekday.csv.gz",
        filenames.emme_database_dir / "CMAP_TripTable_VisitorsWeekday.csv.gz",
        filenames.emme_database_dir / "defaults_base_year/CMAP_TripTable_VisitorsWeekday.csv.gz",
    )
    logging.getLogger('CMAP').info(f"reading visitor trip table from: {filename}")
    trips = pd.read_csv(filename)
    n = trips['trips'].sum()
    trip_wt = trips['trips'] / n
    n_trips = int(np.round(n * scale_factor))
    visit_trips = np.random.choice(len(trips), n_trips, p=trip_wt)
    trips.drop(columns=['trips'], inplace=True)
    trips['visitor_trips'] = pd.Series(visit_trips).value_counts()
    trips.fillna(0, inplace=True)
    return trips.pivot("start_taz", "end_taz", "visitor_trips").fillna(0).astype(int)
