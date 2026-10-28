# -*- coding: utf-8 -*-

import sys
from pathlib import Path

import arcpy

toolbox_dir = Path(__file__).resolve().parent
tbmtools_src_dir = toolbox_dir.parents[1]
sys.path.append(str(tbmtools_src_dir))
from tbmtools.transit import verification


class Toolbox:
    def __init__(self):
        """Define the toolbox (the name of the toolbox is the name of the
        .pyt file)."""
        self.label = "Verify Transit Network Toolbox"
        self.alias = "verify_transit_network_toolbox"

        # List of tool classes associated with this toolbox
        self.tools = [VerifyRoute]


class VerifyRoute:
    def __init__(self):
        """Define the tool (tool name is the name of the class)."""
        self.label = "Verify Route"
        self.description = "Compares the GTFS shape to the EMME network for a single transit line."

    def getParameterInfo(self):
        """Define the tool parameters."""
        agency_name = arcpy.Parameter(displayName="Agency Name",
                                    name="agency_name",
                                    datatype="GPString",
                                    parameterType="Required",
                                    direction="Input")
        route_id = arcpy.Parameter(displayName="Route ID",
                                   name="route_id",
                                   datatype="GPString",
                                   parameterType="Required",
                                   direction="Input")
        dist = arcpy.Parameter(displayName="Distance",
                               name="dist",
                               datatype="GPLong",
                               parameterType="Required",
                               direction="Input")
        tseg_lyr = arcpy.Parameter(displayName="Transit Segment Layer",
                                   name="tseg_lyr",
                                   datatype="GPFeatureLayer",
                                   parameterType="Derived",
                                   direction="Output",
                                   symbology=toolbox_dir.joinpath('NetworkTransitSegments.lyrx'))
        route_lyr = arcpy.Parameter(displayName="Transit Route Layer",
                                    name="route_lyr",
                                    datatype="GPFeatureLayer",
                                    parameterType="Derived",
                                    direction="Output",
                                    symbology=toolbox_dir.joinpath('FeedRoutes.lyrx'))
        params = [agency_name, route_id, dist, tseg_lyr, route_lyr]
        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter. This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        report = verification.measure_transit_line_accuracy(parameters[0].value,
                                                            parameters[1].value,
                                                            'emme_tlines',
                                                            'emme_tsegs',
                                                            f"{parameters[0].value.replace(' ','')}_routes",
                                                            parameters[2].value)
        
        parameters[3].value = report[0]
        parameters[4].value = report[1]

        messages.addMessage(f"tseg_match_pct: {report[2]:.1f}\noutlier_srvcmi: {round(report[3], 1)}")

    def postExecute(self, parameters):
        """This method takes place after outputs are processed and
        added to the display."""

        def zoom_to_layer():
            aprx = arcpy.mp.ArcGISProject('CURRENT')
            mapview = aprx.activeView
            map_obj = mapview.map

            visible = ['emme_links', 'emme_nodes', parameters[3].valueAsText, parameters[4].valueAsText]
            for layer in map_obj.listLayers():
                if not layer.isGroupLayer and not layer.isBasemapLayer and layer.name not in visible:
                    layer.visible = False
                else:
                    layer.visible = True

            target_layer = parameters[4].value
            mapview.camera.setExtent(mapview.getLayerExtent(target_layer))
        
        arcpy.management.SelectLayerByLocation(in_layer=parameters[3].value,
                                               overlap_type='WITHIN_A_DISTANCE',
                                               select_features=parameters[4].value,
                                               search_distance=parameters[2].value,
                                               selection_type='NEW_SELECTION',
                                               invert_spatial_relationship='INVERT')
        zoom_to_layer()
