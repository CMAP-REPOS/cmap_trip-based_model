import argparse
import csv
import time
from collections import defaultdict
from pathlib import Path

import arcpy
import yaml

from tbmtools.transit import config, transit_feed, verification


def list_of_str(arg):
    return arg.split(',')


def feed_shps_to_gdb(feed_dir, gdb, prj):

    agency_name = transit_feed.get_agency_name(feed_dir).replace(" ", "")

    shp_fcs = defaultdict(list)
    for shp in feed_dir.rglob('*.shp'):
        fc_name = f'{agency_name}_{shp.parent.name}_{shp.stem}'
        arcpy.management.Project(in_dataset=str(shp),
                                 out_dataset=f'{gdb}/{fc_name}',
                                 out_coor_system=str(prj))
        shp_fcs[shp.stem] += [fc_name]

    for shp, fcs in shp_fcs.items():
        arcpy.management.Merge(inputs=fcs, output=f'{gdb}/{agency_name}_{shp}')


def create_verification_project(name):
    aprx_path = config.PROJ_DIR.joinpath(name, f'{name}.aprx')
    pyt_path = Path(__file__).resolve().parent.joinpath("VerifyTransitNetwork.pyt")
    if not aprx_path.exists():
        aprx = arcpy.mp.CreateArcGISProject(config.PROJ_DIR, name)
    else:
        aprx = arcpy.mp.ArcGISProject(aprx_path)
    aprx.defaultToolbox = pyt_path
    aprx.save()

    return aprx


def load_verification_database(aprx, network_shp_dir, feeds):

    # Load network shapefiles into project geodatabase.
    arcpy.env.workspace = aprx.defaultGeodatabase
    network_prj = min(Path(network_shp_dir).glob('*.prj'))

    if arcpy.Exists('Network'):
        arcpy.management.Delete('Network')

    network_dataset = arcpy.management.CreateFeatureDataset(out_dataset_path=aprx.defaultGeodatabase,
                                                            out_name='Network',
                                                            spatial_reference=str(network_prj))
    
    network_shps = ['emme_links.shp', 'emme_nodes.shp', 'emme_tlines.shp', 'emme_tsegs.shp']
    for shp in network_shps:
        arcpy.conversion.FeatureClassToGeodatabase(Input_Features=str(Path(network_shp_dir, shp)),
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
    
    # Load feed shapefiles into project geodatabase.
    if arcpy.Exists('Feeds'):
        arcpy.management.Delete('Feeds')
    
    feeds_dataset = arcpy.management.CreateFeatureDataset(out_dataset_path=aprx.defaultGeodatabase,
                                                          out_name='Feeds',
                                                          spatial_reference=str(network_prj))
    
    for feed in feeds:
        feed_shps_to_gdb(Path(feed), feeds_dataset, network_prj)


def build_verification_map(aprx):

    suffix_lyrx = [("nodes", "NetworkNodes.lyrx"),
                   ("links", "NetworkLinks.lyrx"),
                   ("tsegs", "NetworkTransitSegments.lyrx"),
                   ("tlines", "NetworkTransitLines.lyrx"),
                   ("routes", "FeedRoutes.lyrx")]
    arcpy.env.workspace = aprx.defaultGeodatabase
    
    # Add datasets to a map.
    for project_map in aprx.listMaps():
        aprx.deleteItem(project_map)

    verification_map = aprx.createMap('Network Vs. Feed')
    verification_map.addBasemap('Streets (Night)')

    for fd in arcpy.ListDatasets():
        group_layer = verification_map.createGroupLayer(fd)
        for fc in arcpy.ListFeatureClasses(feature_dataset=fd):
            fc_path = Path(aprx.defaultGeodatabase, fd, fc)
            layer = verification_map.addDataFromPath(str(fc_path))
            for suffix, lyrx in suffix_lyrx:
                if fc.endswith(suffix):
                    lyrx_path = Path(__file__).resolve().parent.joinpath(lyrx)
                    arcpy.management.ApplySymbologyFromLayer(layer, lyrx_path)
            verification_map.addLayerToGroup(group_layer, layer)
            verification_map.removeLayer(layer)
    aprx.save()


def list_verification_jobs(feeds, verification_gdb):
    jobs = []
    for feed in feeds:
        agency_name = transit_feed.get_agency_name(feed)
        route_ids = [id for id in transit_feed.get_route_ids(feed) if id not in transit_feed.get_seasonal_routes(feed)]
        for route_id in route_ids:
        # for route_id in ['71']:
            jobs.append((agency_name, route_id, 'emme_tlines', 'emme_tsegs', f'{agency_name.replace(" ", "")}_routes', verification_gdb))

    return jobs


def write_verification_report(results, notes, out_dir):

    out_file = Path(out_dir, 'transit_line_verification_report.csv')
    with open(out_file, 'w', newline='') as f:
        writer = csv.writer(f)
        header = ['agency_name',
                  'route_id',
                  'dist',
                  'match_pct',
                  'outlier_srvcmi',
                  'note']
        writer.writerow(header)
        for result in results:
            if result['tseg_match_pct'] is not None:
                if result['route_id'] in notes[result['agency_name']]:
                    note = notes[result['agency_name']][result['route_id']]
                else:
                    note = ''
                txt = f"{result['agency_name']} route {result['route_id']} within {result['dist']}ft: {result['tseg_match_pct']:.1f}% match."
                row = [result['agency_name'],
                       result['route_id'],
                       result['dist'],
                       round(result['tseg_match_pct'], 1),
                       round(result['outlier_srvcmi'], 1),
                       note]
                print(txt)
                writer.writerow(row)
            else:
                print(f"No transit segments for {result['agency_name']} route {result['route_id']}.")


def main():

    start_time = time.perf_counter()

    # Parse arguments.
    parser = argparse.ArgumentParser(description='verify network imported from GTFS')
    parser.add_argument('--network_shp_dir',
                        help='path to directory containing network shapefiles')
    parser.add_argument('--feeds',
                        type=list_of_str,
                        help='comma-separated paths to feed directories')
    # parser.add_argument('--trip_aggregation',
    #                     action='store_true',
    #                     help='Boolean value controlling number of trips represented by a transit line')
    parser.add_argument('--notes',
                        help='path to YML file containing route verification notes')
    parser.add_argument('--out_dir',
                        help='path to output directory')
    args = parser.parse_args()
    
    # Prepare verification analysis.
    aprx = create_verification_project("VerifyTransitNetwork")
    load_verification_database(aprx, args.network_shp_dir, args.feeds)
    build_verification_map(aprx)

    # Perform verification analysis.
    jobs = list_verification_jobs(args.feeds, aprx.defaultGeodatabase)
    results = verification.verify_transit_lines(jobs)
    with open(args.notes, 'r') as f:
        verification_notes = yaml.safe_load(f)
    write_verification_report(results, verification_notes, args.out_dir)

    print(f'Finished verify - {(time.perf_counter() - start_time) / 60:.6f} minutes')


if __name__ == '__main__':
    main()
