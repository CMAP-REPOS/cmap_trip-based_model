import argparse
from pathlib import Path

import arcpy
import pandas as pd

from tbmtools.transit import config


def filter_mrn(gdb, year):
    """
    Select rail links and nodes to represent a base network in the
    specified year.
    """

    # Work in the GDB.
    arcpy.env.workspace = gdb
    print(arcpy.env.workspace)
    # Make feature layers from feature classes.
    feature_classes = ["railnet/railnet_arc",
                       "railnet/railnet_node",
                       "railnet/future"]
    feature_layers = ["arc_lyr", "node_lyr", "future_lyr"]
    for fc, lyr in list(zip(feature_classes, feature_layers)):
        arcpy.management.Delete(lyr)
        arcpy.management.MakeFeatureLayer(fc, lyr)

    # Select current links.
    arcpy.management.SelectLayerByLocation("arc_lyr",
                                           "SHARE_A_LINE_SEGMENT_WITH",
                                           "railnet/all_runs")
    
    # Add links missing due to misalignment between railnet_arc and all_runs MRN feature classes.
    for ab in config.missing_rail_links:
        arcpy.management.SelectLayerByAttribute('arc_lyr',
                                                'ADD_TO_SELECTION',
                                                f'ANODE = {ab[0]} And BNODE = {ab[1]}')

    # Add completed links from future.
    arcpy.management.SelectLayerByAttribute("future_lyr",
                                            where_clause=f"COMPLETION_YEAR <= {year} AND SCENARIO <> '9'")
    arcpy.management.SelectLayerByLocation("arc_lyr",
                                           "SHARE_A_LINE_SEGMENT_WITH",
                                           "future_lyr",
                                           selection_type="ADD_TO_SELECTION")

    # Remove links replaced by completed future links.
    arcpy.management.AddJoin("future_lyr", "TR_LINE", "future_itin", "TR_LINE")
    past_links = []
    with arcpy.da.SearchCursor("future_lyr", ["future.ACTION", "future_itin.ITIN_A", "future_itin.ITIN_B"]) as cursor:
        for row in cursor:
            action = row[0]
            itin_a = row[1]
            itin_b = row[2]
            if action in [7]:
                past_links += [(itin_a, itin_b), (itin_b, itin_a)]
    for ab in list(set(past_links)):
        arcpy.management.SelectLayerByAttribute("arc_lyr",
                                                "REMOVE_FROM_SELECTION",
                                                f"ANODE = {ab[0]} AND BNODE = {ab[1]}")

    # Select nodes of remaining selected links.
    arcpy.management.SelectLayerByLocation("node_lyr",
                                           "INTERSECT",
                                           "arc_lyr")
    node_fields = ["NODE", "POINT_X", "POINT_Y", "LABEL"]
    link_fields = ["ANODE", "BNODE", "MILES", "DIRECTIONS", "MODES1", "MODES2"]
    node_array = arcpy.da.FeatureClassToNumPyArray("node_lyr", node_fields)
    link_array = arcpy.da.FeatureClassToNumPyArray("arc_lyr", link_fields)
    node_df = pd.DataFrame(node_array)
    link_df = pd.DataFrame(link_array)

    return node_df, link_df


def write_transaction_file(node_df, link_df, out_file):
    """
    Generates an Emme base network transaction file from MRN arc and node tables.
    Arc and node tables from the MRN geodatabase must be supplied as pandas DataFrames.
    """

    # Format node records.
    node = node_df.loc[:, ['NODE', 'POINT_X', 'POINT_Y', 'LABEL']]
    node = node.rename(columns={'NODE': 'i', 'POINT_X': 'xi', 'POINT_Y': 'yi', 'LABEL': 'lab'})
    node['ui1'] = 0
    node['ui2'] = 0
    node['ui3'] = 0
    node['update'] = 'a'
    node['centroid'] = ''
    node = node.loc[:, ['update', 'centroid', 'i', 'xi', 'yi', 'ui1', 'ui2', 'ui3', 'lab']]
    
    # Format link records.
    link = link_df.loc[:, ['ANODE', 'BNODE', 'MILES', 'MODES1']]
    link = link.rename(columns={'ANODE': 'i', 'BNODE': 'j', 'MILES': 'length', 'MODES1': 'modes'})
    rlink = link_df.loc[link_df['DIRECTIONS'] > 1, ['BNODE', 'ANODE', 'MILES', 'MODES2']]
    rlink = rlink.rename(columns={'BNODE': 'i', 'ANODE': 'j', 'MILES': 'length', 'MODES2': 'modes'})
    link = pd.concat([link, rlink])
    link['type'] = 1
    link['lanes'] = 0
    link['vdf'] = 1
    link['update'] = 'a'
    link = link.loc[:, ['update', 'i', 'j', 'length', 'modes', 'type', 'lanes', 'vdf']]
    
    # Write node section to file.
    out_file.parent.mkdir(exist_ok=True)
    with open(out_file, 'w') as f:
        f.write('t nodes\n')
        f.write('c update centroid i xi yi ui1 ui2 ui3 lab\n')
    node.to_csv(out_file,
                sep=' ',
                header=False,
                index=False,
                mode='a')
    
    # Write link section to file.
    with open(out_file, 'a') as f:
        f.write('t links\n')
        f.write('c update i j length modes type lanes vdf\n')
    link.to_csv(out_file,
                sep=' ',
                header=False,
                index=False,
                mode='a')
    with open(out_file, 'r') as f:
        content = f.read()
    modified_content = content.replace('"', '')
    with open(out_file, 'w') as f:
        f.write(modified_content)

    return out_file


def main():

    # Parse arguments.
    parser = argparse.ArgumentParser()
    parser.add_argument('--mrn',
                        type=Path,
                        help='path to MRN geodatabase')
    parser.add_argument('--year',
                        type=int,
                        help='year of base network to generate')
    args = parser.parse_args()
    
    # Rail base network transaction file.
    node_df, link_df = filter_mrn(args.mrn, args.year)
    write_transaction_file(node_df, link_df, config.PROJ_DIR.joinpath('output', f'mrn_{args.year}.txt'))


if __name__ == "__main__":
    main()
