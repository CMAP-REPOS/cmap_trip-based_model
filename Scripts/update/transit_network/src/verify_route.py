import sys
from pathlib import Path

import arcpy

_src_dir = Path(__file__).resolve().parent
sys.path.append(str(_src_dir))
from verify_transit_network import select_outlier_segments


def apply_symbology(agency_id, route_id):
    layer_symbology = [(f'{agency_id}_routes_{route_id}', 'FeedRoute.lyrx'),
                       (f'emme_tlines_{agency_id}_{route_id}', 'NetworkTransitLines.lyrx'),
                       (f'emme_tsegs_emme_tlines_{agency_id}_{route_id}', 'NetworkTransitSegments.lyrx'),
                       (f'Network/emme_links', 'NetworkLinks.lyrx'),
                       (f'Network/emme_nodes', 'NetworkNodes.lyrx')]
    for layer, lyrx in layer_symbology:
        arcpy.management.ApplySymbologyFromLayer(in_layer=layer,
                                                 in_symbology_layer=str(_src_dir.joinpath(lyrx)))

def zoom_to_layer(agency_id, route_id):
    aprx = arcpy.mp.ArcGISProject('CURRENT')
    mapview = aprx.activeView
    map = mapview.map
    target_layer_name = f'{agency_id}_routes_{route_id}'
    visible = ['emme_links', 'emme_nodes', f'emme_tsegs_emme_tlines_{agency_id}_{route_id}', target_layer_name]
    for layer in map.listLayers():
        if not layer.isGroupLayer and not layer.isBasemapLayer and layer.name not in visible:
            layer.visible = False
        else:
            layer.visible = True
    target_layer = map.listLayers(target_layer_name)[0]
    mapview.camera.setExtent(mapview.getLayerExtent(target_layer))

def verify_cta_route(route_id, dist):
    agency_name = 'Chicago Transit Authority'
    agency_id = 'CTA'
    report = select_outlier_segments(agency_name, agency_id, route_id, False, 'emme_tlines', 'emme_tsegs', 'CTA_routes', dist)
    apply_symbology(agency_id, route_id)
    zoom_to_layer(agency_id, route_id)
    return report

def verify_pace_route(route_id, dist):
    agency_name = 'PACE'
    agency_id = 'PACE'
    report = select_outlier_segments(agency_name, agency_id, route_id, False, 'emme_tlines', 'emme_tsegs', 'PACE_routes', dist)
    apply_symbology(agency_id, route_id)
    zoom_to_layer(agency_id, route_id)
    return report

def verify_metra_route(route_id, dist):
    agency_name = 'Metra'
    agency_id = 'METRA'
    report = select_outlier_segments(agency_name, agency_id, route_id, False, 'emme_tlines', 'emme_tsegs', 'METRA_routes', dist)
    apply_symbology(agency_id, route_id)
    zoom_to_layer(agency_id, route_id)
    return report