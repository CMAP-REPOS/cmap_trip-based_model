import os
import tomllib
from pathlib import Path

PROJ_DIR = Path(os.getenv("EMME_PROJECT_DIR"))
DB_DIR = PROJ_DIR.joinpath("Database")
MODELLER = None

with open(PROJ_DIR.joinpath("Scripts", "transit_update.toml"), mode="rb") as f:
    settings = tomllib.load(f)

feeds = settings["feeds"]
cta_feed = Path(feeds["cta"])
metra_feed = Path(feeds["metra"])
pace_feed = Path(feeds["pace"])
nictd_feed = Path(feeds["nictd"])
date = feeds["date"]

base_network = settings["base network"]
year = base_network["year"]
mrn = Path(base_network["mrn"])
highway_nodes = Path(base_network["highway"]["nodes"])
highway_links = Path(base_network["highway"]["links"])
transit_modes = Path(base_network["transit"]["modes"])
transit_vehicles = Path(base_network["transit"]["vehicles"])
link_shape = Path(base_network["link shape"])
missing_rail_links = [tuple(ab) for ab in base_network["missing rail links"]]

calibration = settings["calibration"]
seasonal_cta = calibration["seasonal"]["cta"]
seasonal_pace = calibration["seasonal"]["pace"]
express_cta = calibration["express"]["cta"]
express_pace = calibration["express"]["pace"]

# add_mrn_links = {2035: [(42291, 42528),
#                         (42292, 42528),
#                         (42254, 42527),
#                         (49005, 42527),
#                         (42469, 42470),
#                         (42470, 42293)]}

# replace_mrn_links = {2024: [(42472, 42473),
#                             (32099, 32100),
#                             (32100, 32101),
#                             (32101, 32092),
#                             (42203, 42204),
#                             (32115, 39011)],
#                      2025: [(42336, 42337),
#                             (42335, 42336),
#                             (42434, 42525)],
#                      2035: [(42246, 42247),
#                             (42293, 42469),
#                             (42291, 42292),
#                             (49005, 42254)]}


def init_modeller():
    from tbmtools import project
    global MODELLER
    MODELLER = project.connect(PROJ_DIR)
