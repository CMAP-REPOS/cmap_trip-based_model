###############################################################################################
# DISTR_M01_SPATIAL_ANALYSIS.PY                                                               #
#  Craig Heither, rev. 08-07-2013                                                             #
#                                                                                             # 
#    This program performs the spatial analyses needed to create the                          #
#    DISTR and M01 files for a model run.                                                     #
#                                                                                             #
#                                                                                             #
#      -------------------------------------------------------------------------------        #
#   Revisions:                                                                                #
#       04-24-2013: Upgrade to ArcGIS 10.1 caused some issues -                               #
#            * I set arcpy environment workspace to temp folder - this fixed an issue that    #
#                       arose due to the inclusion of explicit paths to the folder.           #
#            * ESRI's 'Multi Ring Buffer' script seems to have a bug - I wrote code to        #
#                       iteratively perform the same functions.                               #
#                                                                                             #
#       08-07-2013: Additional Park-n-Ride distance to subzone calculation due to Eash's      #
#                   October 2012 change to DISTR file.                                        #
#                                                                                             #
#       Ferguson 10/2/2018: Updated zone system 09 references to zone system 17               #
#                                                                                             #
###############################################################################################

import sys, arcpy, os

# ---------------------------------------------------------------
# Local Variables.
# ---------------------------------------------------------------
    # -- Input -- 
work = str.replace(os.getcwd(), "prep_macros", "data\\distr")
arcpy.env.workspace = os.getcwd() + "\\temp"
temp = os.getcwd() + "\\temp"
metra_stop_dbf = "metra.dbf"
ctarail_stop_dbf = "ctarail.dbf"
subzone_shp = work + "\\sz17_centroids.shp"
zones = work + "\\zone17.shp"
bus_stop_dbf = "regbus.dbf"
feed_stop_dbf = "feed.dbf"
zone_shp = work + "\\zone17.shp"
pnr_dbf = "pnr.dbf"
zncntrd_dbf = "zncntrd.dbf"

    # -- Temporary --
# DISTR Rail
temp_metra_Layer = "tmp_metra_Layer"
temp_metra_Layer_shp = "tmp_metra_Layer.shp"
temp_ctarail_Layer = "tmp_ctarail_Layer"
temp_ctarail_Layer_shp = "tmp_ctarail_Layer.shp"
sz17_1_shp = "sz17_1.shp"
sz17_2_shp = "sz17_2.shp"
sz17_3_shp = "sz17_3.shp"
# DISTR Bus
temp_bus_Layer = "tmp_bus_Layer"
temp_bus_Layer_shp = "tmp_bus_Layer.shp"
temp_feed_Layer = "tmp_feed_Layer"
temp_feed_Layer_shp = "tmp_feed_Layer.shp"
temp_bus_ring_shp = "tmp_bus_ring.shp"
temp_feed_ring_shp = "tmp_feed_ring.shp"
temp_bus_dislv_shp = "tmp_bus_dislv.shp"
temp_feed_dislv_shp = "tmp_feed_dislv.shp"
inFeatBus = [temp_bus_dislv_shp, zones]
inFeatFeed = [temp_feed_dislv_shp, zones]
temp_bus_intrsct_shp = "tmp_bus_intrsct.shp"
temp_feed_intrsct_shp = "tmp_feed_intrsct.shp"
temp_bus_area_shp = "tmp_bus_area.shp"
temp_feed_area_shp = "tmp_feed_area.shp"
# M01 Park-N-Ride
temp_pnr_Layer = "tmp_pnr_Layer"
temp_pnr_Layer_shp = "tmp_pnr_Layer.shp"
temp_zncntrd_Layer = "tmp_zncntrd_Layer"
temp_zncntrd_Layer_shp = "tmp_zncntrd_Layer.shp"
parkrd = "parkrd"
pkrd_dbf = "pkrd.dbf"
# M01 Bus Wait
bushdwy_buf_shp = "bushdwy_buf.shp"
bhdwy_zone_shp = "bhdwy_zn.shp"
fdhdwy_buf_shp = "fdhdwy_buf.shp"
fhdwy_zone_shp = "fhdwy_zn.shp"


# ---------------------------------------------------------------
# Rail Station Analysis (DISTR).
# ---------------------------------------------------------------
# -- Create Shapefiles for analysis
print(" ")
print("  ---> STEP 1 OF 4: Finding Rail Station Nearest Each Subzone Centroid (DISTR)")
print(" ")
arcpy.MakeXYEventLayer_management(metra_stop_dbf, "xcoord", "ycoord", temp_metra_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_metra_Layer, temp)
arcpy.MakeXYEventLayer_management(ctarail_stop_dbf, "xcoord", "ycoord", temp_ctarail_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_ctarail_Layer, temp)
arcpy.CopyFeatures_management(subzone_shp, sz17_1_shp, "", "0", "0", "0")
arcpy.CopyFeatures_management(subzone_shp, sz17_2_shp, "", "0", "0", "0")
# -- Find Nearest Station
arcpy.Near_analysis(sz17_1_shp, temp_metra_Layer_shp, "528000 Feet", "NO_LOCATION", "NO_ANGLE")
arcpy.Near_analysis(sz17_2_shp, temp_ctarail_Layer_shp, "528000 Feet", "NO_LOCATION", "NO_ANGLE")


# ---------------------------------------------------------------
# Bus Stop Analysis (DISTR).
# ---------------------------------------------------------------
# -- Create Shapefiles for analysis
print(" ")
print("  ---> STEP 2 OF 4: Determining Bus Stop Accessibility (DISTR)")
print("       (This step takes the most time to complete)")
print(" ")
arcpy.MakeXYEventLayer_management(bus_stop_dbf, "xcoord", "ycoord", temp_bus_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_bus_Layer, temp)
arcpy.MakeXYEventLayer_management(feed_stop_dbf, "xcoord", "ycoord", temp_feed_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_feed_Layer, temp)

# -- Buffer Bus Stops for analysis (increment from 0.1 to 1.1 miles by 0.1 miles)
dist = (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 ,1.1)
x=0
for d in dist:
    m=dist[x]
    mi=str(m)+" Miles"

    arcpy.Buffer_analysis(temp_bus_Layer_shp, temp_bus_ring_shp, mi, "FULL", "ROUND", "ALL", "")
    arcpy.AddField_management(temp_bus_ring_shp, "distance", "FLOAT", "2", "1", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.CalculateField_management(temp_bus_ring_shp, "distance", m, "PYTHON", "")

    arcpy.Buffer_analysis(temp_feed_Layer_shp, temp_feed_ring_shp, mi, "FULL", "ROUND", "ALL", "")
    arcpy.AddField_management(temp_feed_ring_shp, "distance", "FLOAT", "2", "1", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.CalculateField_management(temp_feed_ring_shp, "distance", m, "PYTHON", "")

    if m == 0.1:
        arcpy.CopyFeatures_management(temp_bus_ring_shp, temp_bus_dislv_shp, "", "0", "0", "0")
        arcpy.CopyFeatures_management(temp_feed_ring_shp, temp_feed_dislv_shp, "", "0", "0", "0")
    else:
        arcpy.Append_management(temp_bus_ring_shp, temp_bus_dislv_shp, "NO_TEST", "", "")
        arcpy.Append_management(temp_feed_ring_shp, temp_feed_dislv_shp, "NO_TEST", "", "")

    if os.path.exists(temp_bus_ring_shp):
        arcpy.Delete_management(temp_bus_ring_shp, "ShapeFile")
    if os.path.exists(temp_feed_ring_shp):
        arcpy.Delete_management(temp_feed_ring_shp, "ShapeFile")

    x += 1
    print(" ")
    print("      ---> Iteration {0} of {1} ({2})".format(x, len(dist), mi))


# -- Intersect with Zones and Recalculate Area
arcpy.Intersect_analysis(inFeatBus, temp_bus_intrsct_shp, "ALL", "", "INPUT")
arcpy.Intersect_analysis(inFeatFeed, temp_feed_intrsct_shp, "ALL", "", "INPUT")
arcpy.management.Copy(temp_bus_intrsct_shp, temp_bus_area_shp)
arcpy.management.Copy(temp_feed_intrsct_shp, temp_feed_area_shp)
arcpy.management.CalculateField(temp_bus_area_shp.replace('.shp', '.dbf'), 'F_AREA', '!shape.area!')
arcpy.management.CalculateField(temp_feed_area_shp.replace('.shp', '.dbf'), 'F_AREA', '!shape.area!')


# ---------------------------------------------------------------
# Park-and-Ride Analysis (M01 & DISTR).
# ---------------------------------------------------------------
# -- Create Shapefile for analysis
print(" ")
print("  ---> STEP 3 OF 4: Determining Park-and-Ride Availability and Cost (M01)")
print(" ")
arcpy.MakeXYEventLayer_management(pnr_dbf, "xcoord", "ycoord", temp_pnr_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_pnr_Layer, temp)
arcpy.MakeXYEventLayer_management(zncntrd_dbf, "xcoord", "ycoord", temp_zncntrd_Layer, "PROJCS['NAD_1927_StatePlane_Illinois_East_FIPS_1201',GEOGCS['GCS_North_American_1927',DATUM['D_North_American_1927',SPHEROID['Clarke_1866',6378206.4,294.9786982]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-88.33333333333333],PARAMETER['Scale_Factor',0.999975],PARAMETER['Latitude_Of_Origin',36.66666666666666],UNIT['Foot_US',0.3048006096012192]];IsHighPrecision")
arcpy.FeatureClassToShapefile_conversion(temp_zncntrd_Layer, temp)
arcpy.GenerateNearTable_analysis(temp_zncntrd_Layer_shp, temp_pnr_Layer_shp, parkrd, "10 Miles", "NO_LOCATION", "NO_ANGLE", "ALL", "0")
arcpy.TableSelect_analysis(parkrd, pkrd_dbf, "\"OBJECTID\" >= 0")
# -- Find Nearest Park-n-Ride Station
arcpy.CopyFeatures_management(subzone_shp, sz17_3_shp, "", "0", "0", "0")
arcpy.Near_analysis(sz17_3_shp, temp_pnr_Layer_shp, "528000 Feet", "NO_LOCATION", "NO_ANGLE")


# ---------------------------------------------------------------
# Bus Wait Analysis (M01).
# ---------------------------------------------------------------
print(" ")
print("  ---> STEP 4 OF 4: Determining Bus Wait Times (M01)")
print(" ")
# -- Buffer stops 0.1 miles to account for spatial inaccuracies
arcpy.Buffer_analysis(temp_bus_Layer_shp, bushdwy_buf_shp, "0.1 Miles", "FULL", "ROUND", "NONE", "")
arcpy.Identity_analysis(zone_shp, bushdwy_buf_shp, bhdwy_zone_shp, "ALL", "", "NO_RELATIONSHIPS")
arcpy.Buffer_analysis(temp_feed_Layer_shp, fdhdwy_buf_shp, "0.1 Miles", "FULL", "ROUND", "NONE", "")
arcpy.Identity_analysis(zone_shp, fdhdwy_buf_shp, fhdwy_zone_shp, "ALL", "", "NO_RELATIONSHIPS")

print("-- SPATIAL ANALYSIS COMPLETED --")
print(" ")

