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

_src_dir = Path(__file__).resolve().parent
_out_dir = _src_dir.parent.joinpath('output')


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

def select_network_lines(tlines, agency_name, agency_id, route_short_name):
    lyr = f"{tlines}_{agency_id}_{route_short_name}"
    sel_exp = (f"F_agency_na = '{agency_name}'"
               + f" And F_route_nam LIKE '{route_short_name} -%'")
    arcpy.management.MakeFeatureLayer(in_features=tlines,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    tline_sel_count = int(arcpy.management.GetCount(lyr)[0])
    if tline_sel_count <= 0:
        raise ValueError(f'{agency_name} route {route_short_name} not found in network. Check EMME Logbook for errors during GTFS import.')
    return lyr

def select_network_segments(tsegs, tline_sel):
    lyr = '{0}_{1}'.format(tsegs, tline_sel.split('\\')[-1])
    line_ids = list()
    with arcpy.da.SearchCursor(in_table=tline_sel, field_names='ID') as cursor:
        for row in cursor:
            line_ids.append(f"'{row[0]}'")
    sel_exp = f"LINE_ID IN ({', '.join(line_ids)})"
    arcpy.management.MakeFeatureLayer(in_features=tsegs,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    return lyr

def select_feed_route(routes, route_short_name, trip_aggregation):
    lyr = f'{routes}_{route_short_name}'
    if trip_aggregation:
        sel_exp = f"SHORT_NAME = '{route_short_name}'"
    else:
        sel_exp = f"ROUTE_ID = '{route_short_name}'"  # inro.emme.data.network.transit.import_from_gtfs uses route_id instead of route_short_name in imported route name
    arcpy.management.MakeFeatureLayer(in_features=routes,
                                      out_layer=lyr,
                                      where_clause=sel_exp)
    return lyr

def select_outlier_segments(agency_name, agency_id, route_short_name, trip_aggregation, tlines, tsegs,
                            feed_routes, dist_thresh):
    # Select network segments.
    tline_sel = select_network_lines(tlines,
                                     agency_name,
                                     agency_id,
                                     route_short_name)
    tseg_sel = select_network_segments(tsegs, tline_sel)
    # Select feed route.
    route_sel = select_feed_route(feed_routes, route_short_name, trip_aggregation)
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

def mp_select_outlier_segments(agency_name, agency_id, route_short_name, trip_aggregation, tlines, tsegs,
                               feed_routes, dist_tholds, gdb):
    main_workspace = gdb
    process = multiprocessing.current_process()
    process_workspace = rf'memory\{process.pid}'
    if arcpy.env.workspace is None:
        arcpy.env.workspace = main_workspace
        arcpy.env.overwriteOutput = True
        fcs = [tlines, tsegs, feed_routes]
        for fc in fcs:
            try:
                arcpy.management.MakeFeatureLayer(in_features=fc,
                                                  out_layer=rf'{process_workspace}\{fc}')
            except Exception as e:
                raise Exception(f'Process {process.pid} failed to load {fc} from {gdb} to memory') from e
    for dist_thresh in dist_tholds:
        tseg_match_pct, outlier_srvcmi = select_outlier_segments(agency_name,
                                                                 agency_id,
                                                                 route_short_name,
                                                                 trip_aggregation,
                                                                 rf'{process_workspace}\{tlines}',
                                                                 rf'{process_workspace}\{tsegs}',
                                                                 rf'{process_workspace}\{feed_routes}',
                                                                 dist_thresh)
        result = dict(route_short_name=route_short_name,
                      dist_thresh=dist_thresh,
                      tseg_match_pct=tseg_match_pct,
                      outlier_srvcmi=outlier_srvcmi)
        if tseg_match_pct == 100:
            break
    global complete_jobs, total_jobs
    with complete_jobs.get_lock():
        complete_jobs.value += 1
        print(f'{process.name} with {agency_id} {route_short_name} - {complete_jobs.value} of {total_jobs.value} jobs completed')
    return result

def init_worker(shared_job_counter, shared_job_count):
    global complete_jobs, total_jobs
    complete_jobs = shared_job_counter
    total_jobs = shared_job_count

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
    parser.add_argument('--trip_aggregation',
                        help='Boolean value controlling number of trips represented by a transit line')
    parser.add_argument('--notes',
                        help='path to YML file containing route verification notes')
    parser.add_argument('--out_dir',
                        help='path to output directory')
    args = parser.parse_args()
    # Add network shapefiles to project geodatabase.
    aprx_path = _out_dir.joinpath('VerifyTransitNetwork.aprx')
    if not aprx_path.exists():
        aprx = arcpy.mp.ArcGISProject(_src_dir.joinpath('Template.aprx'))
        aprx.saveACopy(aprx_path)
    aprx = arcpy.mp.ArcGISProject(aprx_path)
    gdb_name = 'VerifyTransitNetwork.gdb'
    gdb_path = _out_dir.joinpath(gdb_name)
    if arcpy.Exists(str(gdb_path)):
        arcpy.management.Delete(str(gdb_path))
    aprx.defaultGeodatabase = arcpy.management.CreateFileGDB(out_folder_path=str(_out_dir),
                                                             out_name=gdb_name)
    arcpy.env.workspace = aprx.defaultGeodatabase
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
    map.addBasemap('Streets')
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
        if eval(args.trip_aggregation):
            route_short_names = transit_feed.get_route_short_names(feed)  
        else:
            route_short_names = transit_feed.get_route_ids(feed)  # inro.emme.data.network.transit.import_from_gtfs uses route_id instead of route_short_name in imported route name
        jobs = list()
        for route_short_name in route_short_names:
        # for route_short_name in ['Org']:
            jobs.append((agency_name, agency_id, route_short_name, eval(args.trip_aggregation), 'emme_tlines', 'emme_tsegs', f'{agency_id}_routes', dist_tholds, aprx.defaultGeodatabase))
        shared_job_counter = multiprocessing.Value('i', 0)
        shared_job_count = multiprocessing.Value('i', len(jobs))
        with multiprocessing.Pool(processes=min(os.cpu_count(), 61), initializer=init_worker, initargs=(shared_job_counter, shared_job_count)) as pool:
            results = pool.starmap(mp_select_outlier_segments, jobs)
        with open(args.notes, 'r') as f:
            route_notes = yaml.safe_load(f)
        out_file = Path(args.out_dir, f'verification_report_{agency_id}.csv')
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
                    print(f'No transit segments for {agency_id} route {route_short_name}.')
    end = time.perf_counter()
    elapsed = end - start
    print(f'Finished verify - {elapsed / 60:.6f} minutes')

if __name__ == '__main__':
    sys.exit(main())