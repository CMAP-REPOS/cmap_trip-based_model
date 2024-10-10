import sys
from pathlib import Path
import argparse
import multiprocessing
import time
import os

import arcpy
import yaml

sys.path.append(str(Path(__file__).resolve().parents[3]))
from tbmtools import transit_feed


def list_of_str(arg):
    return arg.split(',')

def feed_shps_to_gdb(feed_dir, gdb, prj):
    agency_id = transit_feed.get_agency_id(feed_dir)
    shp_fcs = dict()
    for shp in feed_dir.rglob('*.shp'):
        fc_name = f'{agency_id}_{shp.parent.name}_{shp.stem}'
        arcpy.management.Project(in_dataset=str(shp),
                                 out_dataset=f'{gdb}/{fc_name}',
                                 out_coor_system=str(prj))
        if shp.stem not in shp_fcs.keys():
            shp_fcs[shp.stem] = [fc_name]
        else:
            shp_fcs[shp.stem] += [fc_name]
    for shp, fcs in shp_fcs.items():
        arcpy.management.Merge(inputs=fcs, output=f'{gdb}/{agency_id}_{shp}')

def select_network_lines(tlines, agency_name, route_short_name):
    lyr = f"{tlines}_{agency_name}_{route_short_name}"
    sel_exp = (f"F_agency_na = '{agency_name}'"
               + f" And F_route_nam LIKE '{route_short_name} %'")
    print(f'select_network_lines in={tlines} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=tlines,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    tline_sel_count = int(arcpy.management.GetCount(lyr)[0])
    if tline_sel_count <= 0:
        raise ValueError(f'{agency_name} route {route_short_name} not found in network.')
    return lyr

def select_network_segments(tsegs, tline_sel):
    lyr = '{0}_{1}'.format(tsegs, tline_sel.split('\\')[-1])
    line_ids = list()
    with arcpy.da.SearchCursor(in_table=tline_sel, field_names='ID') as cursor:
        for row in cursor:
            line_ids.append(f"'{row[0]}'")
    sel_exp = f"LINE_ID IN ({', '.join(line_ids)})"
    print(f'select_network_segments in={tsegs} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=tsegs,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    return lyr

def select_feed_route(routes, route_short_name):
    lyr = f'{routes}_{route_short_name}'
    sel_exp = f"SHORT_NAME = '{route_short_name}'"
    print(f'select_feed_route in={routes} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=routes,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    return lyr

def select_outlier_segments(agency_name, route_short_name, tlines, tsegs,
                            feed_routes, dist_thresh):
    # Select network segments.
    tline_sel = select_network_lines(tlines,
                                     agency_name,
                                     route_short_name)
    tseg_sel = select_network_segments(tsegs, tline_sel)
    # Select feed route.
    route_sel = select_feed_route(feed_routes, route_short_name)
    # Select only outlier segments.
    tseg_sel = arcpy.management.SelectLayerByAttribute(in_layer_or_view=tseg_sel, selection_type='CLEAR_SELECTION')
    tseg_sel_count = arcpy.management.GetCount(tseg_sel)[0]
    try:
        tseg_outlier_sel, out_layers, tseg_outlier_count = arcpy.management.SelectLayerByLocation(in_layer=tseg_sel,
                                                                                                  overlap_type='WITHIN_A_DISTANCE',
                                                                                                  select_features=route_sel,
                                                                                                  search_distance=dist_thresh,
                                                                                                  selection_type='NEW_SELECTION',
                                                                                                  invert_spatial_relationship='INVERT')
    except Exception as e:
        raise Exception(f'Selection failed: {arcpy.env.workspace} {tseg_sel} {route_sel} {dist_thresh}') from e
    # Summarize outlier segments.
    tseg_match_pct = (1 - int(tseg_outlier_count)/int(tseg_sel_count)) * 100
    tseg_outlier_len = arcpy.da.TableToNumPyArray(tseg_outlier_sel, 'SHAPE@LENGTH')
    outlier_srvcmi = tseg_outlier_len['SHAPE@LENGTH'].sum() / 5280
    return tseg_match_pct, outlier_srvcmi

def mp_select_outlier_segments(agency_name, route_short_name, tlines, tsegs,
                               feed_routes, dist_tholds, gdb):
    main_workspace = gdb
    process = multiprocessing.current_process()
    pid = process.pid
    process_workspace = rf'memory\{pid}'
    if arcpy.env.workspace is None:
        arcpy.env.workspace = main_workspace
        arcpy.env.overwriteOutput = True
        fcs = [tlines, tsegs, feed_routes]
        for fc in fcs:
            try:
                arcpy.management.MakeFeatureLayer(in_features=fc,
                                                  out_layer=rf'{process_workspace}\{fc}')
            except Exception as e:
                raise Exception(f'Process {pid} failed to load {fc} from {gdb} to memory') from e
    for dist_thresh in dist_tholds:
        tseg_match_pct, outlier_srvcmi = select_outlier_segments(agency_name,
                                                                 route_short_name,
                                                                 rf'{process_workspace}\{tlines}',
                                                                 rf'{process_workspace}\{tsegs}',
                                                                 rf'{process_workspace}\{feed_routes}',
                                                                 dist_thresh)
        result = dict(route_short_name=route_short_name,
                      dist_thresh=dist_thresh,
                      tseg_match_pct=tseg_match_pct,
                      outlier_srvcmi=outlier_srvcmi)
        if tseg_match_pct is not None:
            if tseg_match_pct == 100:
                return result
            elif dist_thresh == dist_tholds[-1]:
                return result
        else:
            return result

def main():
    start = time.perf_counter()
    # Parse arguments.
    parser = argparse.ArgumentParser(description='verify network imported from GTFS')
    parser.add_argument('--arcgis_project',
                        help='path to ArcGIS project')
    parser.add_argument('--network_shp_dir',
                        help='path to directory containing network shapefiles')
    parser.add_argument('--feeds',
                        type=list_of_str,
                        help='comma-separated paths to feed directories')
    parser.add_argument('--notes',
                        help='path to YML file containing route verification notes')
    parser.add_argument('--out_dir',
                        help='path to output directory')
    args = parser.parse_args()
    # Add network shapefiles to project geodatabase.
    aprx = arcpy.mp.ArcGISProject(args.arcgis_project)
    arcpy.env.workspace = aprx.defaultGeodatabase
    for dataset in arcpy.ListDatasets():
        arcpy.management.Delete(dataset)
    network_prj = sorted(Path(args.network_shp_dir).glob('*.prj'))[0]
    network_dataset = arcpy.management.CreateFeatureDataset(out_dataset_path=aprx.defaultGeodatabase,
                                                            out_name='Network',
                                                            spatial_reference=str(network_prj))
    network_shps = ['emme_links.shp', 'emme_nodes.shp', 'emme_tlines.shp', 'emme_tsegs.shp']
    for shp in network_shps:
        arcpy.conversion.FeatureClassToGeodatabase(Input_Features=str(Path(args.network_shp_dir, shp)),
                                                   Output_Geodatabase=network_dataset)
    arcpy.management.CreateRelationshipClass(origin_table='emme_tlines',
                                             destination_table='emme_tsegs',
                                             out_relationship_class='transit_line_segments',
                                             relationship_type='COMPOSITE',
                                             forward_label='transit_segments',
                                             backward_label='transit_line',
                                             message_direction='FORWARD',
                                             cardinality='ONE_TO_MANY',
                                             attributed='NONE',
                                             origin_primary_key='ID',
                                             origin_foreign_key='LINE_ID')
    # Add feed shapefiles to project geodatabase.
    feeds_dataset = arcpy.management.CreateFeatureDataset(out_dataset_path=aprx.defaultGeodatabase,
                                                          out_name='Feeds',
                                                          spatial_reference=str(network_prj))
    for feed in args.feeds:
        feed_shps_to_gdb(Path(feed), feeds_dataset, network_prj)
    # Add datasets to a map.
    for map in aprx.listMaps():
        aprx.deleteItem(map)
    map = aprx.createMap('Network Vs. Feed')
    for fd in arcpy.ListDatasets():
        group_layer = map.createGroupLayer(fd)
        for fc in arcpy.ListFeatureClasses(feature_dataset=fd):
            fc_path = Path(aprx.defaultGeodatabase, fd, fc)
            layer = map.addDataFromPath(str(fc_path))
            map.addLayerToGroup(group_layer, layer)
            map.removeLayer(layer)
    aprx.save()
    # Write verification report.
    dist_tholds = [12,  # 1 lane
                   24,  # 2 lanes
                   48,  # 4 lanes
                   83,  # 1/4 block
                   165]  # 1/2 block
    for feed in args.feeds:
        agency_name = transit_feed.get_agency_name(feed)
        agency_id = transit_feed.get_agency_id(feed)
        route_short_names = transit_feed.get_route_short_names(feed)
        jobs = list()
        for route_name in route_short_names:
        # for route_name in ['1', '5', '92']:
            jobs.append((agency_name, route_name, 'emme_tlines', 'emme_tsegs', f'{agency_id}_routes', dist_tholds, aprx.defaultGeodatabase))
        with multiprocessing.Pool(processes=os.cpu_count() - 2) as pool:
            results = pool.starmap(mp_select_outlier_segments, jobs)
        print(results)
        with open(args.notes, 'r') as f:
            route_notes = yaml.safe_load(f)
        out_file = Path(args.out_dir, 'verification_report.csv')
        with open(out_file, 'w') as f:
            f.write('agency_id,route_short_name,dist_thresh,match_pct,outlier_srvcmi,note\n')
            for result in results:
                if result['tseg_match_pct'] is not None:
                    if result['route_short_name'] in route_notes[agency_id].keys():
                        note = route_notes[agency_id][result['route_short_name']]
                    else:
                        note = ''
                    txt = f"{agency_id} route {result['route_short_name']} within {result['dist_thresh']}ft: {result['tseg_match_pct']:.1f}% match."
                    row = f"{agency_id},{result['route_short_name']},{result['dist_thresh']},{result['tseg_match_pct']:.1f},{result['outlier_srvcmi']:.1f},{note}\n"
                    print(txt)
                    f.write(row)
                else:
                    print(f'No transit segments for {agency_id} route {route_name}.')
    end = time.perf_counter()
    elapsed = end - start
    print(f'Finished - {elapsed / 60:.6f} minutes')

if __name__ == '__main__':
    sys.exit(main())