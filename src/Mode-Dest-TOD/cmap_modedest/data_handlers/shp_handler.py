
import geopandas as gpd


def load_zone_shapes(filenames):
	zone_shp = gpd.read_file(
		filenames.zone_shapefile
	).set_index('zone17')
	return zone_shp


