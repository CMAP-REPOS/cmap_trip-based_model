import multiprocessing
import os

import arcpy

complete_jobs = None
total_jobs = None


class TransitNetworkSelectionError(Exception):
    """Raised when selecting outlier transit network segments fails."""


class ProcessWorkspaceError(Exception):
    """Raised when a worker cannot load a feature class into memory."""


def sql_text(value):
    return "'" + str(value).replace("'", "''") + "'"


def select_network_lines(tlines, agency_name, route_id):

    lyr = f"{tlines}_{agency_name}_{route_id}"

    agency_name_field = arcpy.AddFieldDelimiters(tlines, "F_agency_na")
    route_name_field = arcpy.AddFieldDelimiters(tlines, "F_route_nam")
    
    sel_exp = (f"{agency_name_field} = {sql_text(agency_name)} "
               + f"AND {route_name_field} LIKE {sql_text(f'{route_id} -%')}")
    # print(f'select_network_lines in={tlines} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=tlines,
                                      out_layer=lyr,
                                      where_clause=sel_exp)

    tline_sel_count = int(arcpy.management.GetCount(lyr)[0])
    if tline_sel_count <= 0:
        raise ValueError(f'{agency_name} route {route_id} not found in network. Check EMME Logbook for errors during GTFS import.')
    
    return lyr


def select_network_segments(tsegs, tline_sel):

    tline_layer_name = tline_sel.split('\\')[-1]
    lyr = f"{tsegs}_{tline_layer_name}"

    line_ids = []
    with arcpy.da.SearchCursor(in_table=tline_sel, field_names='ID') as cursor:
        for row in cursor:
            line_ids.append(sql_text(row[0]))

    sel_exp = f"LINE_ID IN ({', '.join(line_ids)})"
    # print(f'select_network_segments in={tsegs} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=tsegs,
                                      out_layer=lyr,
                                      where_clause=sel_exp)

    return lyr


def select_feed_route(routes, route_id):

    lyr = f'{routes}_{route_id}'

    route_id_field = arcpy.AddFieldDelimiters(routes, "ROUTE_ID")

    sel_exp = f"{route_id_field} = {sql_text(route_id)}"
    # print(f'select_feed_route in={routes} out={lyr} overwrite={arcpy.env.overwriteOutput}', flush=True)
    arcpy.management.MakeFeatureLayer(in_features=routes,
                                      out_layer=lyr,
                                      where_clause=sel_exp)

    return lyr


def measure_transit_line_accuracy(agency_name, route_id, tlines,
                                  tsegs, feed_routes, dist):

    # Select network segments.
    tline_sel = select_network_lines(tlines,
                                     agency_name,
                                     route_id)
    tseg_sel = select_network_segments(tsegs, tline_sel)

    # Select feed route.
    route_sel = select_feed_route(feed_routes, route_id)

    # Select only outlier segments.
    tseg_sel = arcpy.management.SelectLayerByAttribute(in_layer_or_view=tseg_sel, selection_type='CLEAR_SELECTION')
    tseg_sel_count = arcpy.management.GetCount(tseg_sel)[0]
    try:
        tseg_outlier_sel, _out_layers, tseg_outlier_count = arcpy.management.SelectLayerByLocation(in_layer=tseg_sel,
                                                                                                  overlap_type='WITHIN_A_DISTANCE',
                                                                                                  select_features=route_sel,
                                                                                                  search_distance=dist,
                                                                                                  selection_type='NEW_SELECTION',
                                                                                                  invert_spatial_relationship='INVERT')
    except Exception as e:
        raise TransitNetworkSelectionError(
            f'Selection failed: {arcpy.env.workspace} {tseg_sel} {route_sel} {dist}'
        ) from e

    # Summarize outlier segments.
    tseg_match_pct = (1 - int(tseg_outlier_count)/int(tseg_sel_count)) * 100
    tseg_outlier_len = arcpy.da.TableToNumPyArray(tseg_outlier_sel, 'SHAPE@LENGTH')
    outlier_srvcmi = tseg_outlier_len['SHAPE@LENGTH'].sum() / 5280

    return tseg_outlier_sel, route_sel, tseg_match_pct, outlier_srvcmi


def measure_transit_line_precision(agency_name, route_id, tlines,
                                   tsegs, feed_routes, gdb):

    # Distances (feet) for measuring mapmatching precision.
    precision_distances = (
        12,  # ~1 lane
        24,  # ~2 lanes
        48,  # ~4 lanes
        83,  # ~1/4 block
        165  # ~1/2 block
    )
    # main_workspace = gdb
    process = multiprocessing.current_process()
    # process_workspace = rf'memory\{process.pid}'

    # if arcpy.env.workspace is None:
    arcpy.env.workspace = gdb
    arcpy.env.overwriteOutput = True
    fcs = [tlines, tsegs, feed_routes]
    for fc in fcs:
        try:
            arcpy.management.MakeFeatureLayer(in_features=fc,
                                            #   out_layer=rf'{process_workspace}\{fc}')
                                              out_layer=f'{fc}_lyr')
        except Exception as e:
            raise ProcessWorkspaceError(f'Process {process.pid} failed to make a {fc} layer from {gdb}') from e
    
    for dist in precision_distances:
        tseg_outlier_sel, route_sel, tseg_match_pct, outlier_srvcmi = measure_transit_line_accuracy(agency_name,
                                                                       route_id,
                                                                       #  rf'{process_workspace}\{tlines}',
                                                                       #  rf'{process_workspace}\{tsegs}',
                                                                       #  rf'{process_workspace}\{feed_routes}',
                                                                       f'{tlines}_lyr',
                                                                       f'{tsegs}_lyr',
                                                                       f'{feed_routes}_lyr',
                                                                       dist)
        result = {'agency_name': agency_name,
                  'route_id': route_id,
                  'dist': dist,
                  'tseg_match_pct': tseg_match_pct,
                  'outlier_srvcmi': outlier_srvcmi}
        if tseg_match_pct == 100:
            break

    # global complete_jobs, total_jobs
    with complete_jobs.get_lock():
        complete_jobs.value += 1
        print(f'{process.name} with {agency_name} {route_id} - {complete_jobs.value} of {total_jobs.value} jobs completed')
    
    return result


def init_worker(job_counter, job_count):
        global complete_jobs, total_jobs
        complete_jobs = job_counter
        total_jobs = job_count


def verify_transit_lines(jobs):
    job_counter = multiprocessing.Value('i', 0)
    job_count = multiprocessing.Value('i', len(jobs))
    with multiprocessing.Pool(processes=min(os.cpu_count(), 39), initializer=init_worker, initargs=(job_counter, job_count)) as pool:
        results = pool.starmap(measure_transit_line_precision, jobs)
    
    return results
