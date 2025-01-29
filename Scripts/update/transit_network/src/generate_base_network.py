from pathlib import Path
import sys
import argparse

import pandas as pd
import arcpy

_src_dir = Path(__file__).resolve().parent
_in_dir = _src_dir.parent.joinpath('input')
_out_dir = _src_dir.parent.joinpath('output')

# Current MRN links that are replaced by future MRN links.
rmv_links = [
             # Through 2024
             (42472, 42473),
             (32099, 32100),
             (32100, 32101),
             (32101, 32092),
             (42203, 42204),
             (32115, 39011),
             # Through 2025
             #(42336, 42337),
             #(42335, 42336)
            ]


def filter_mrn(gdb, year, obsolete_links):
    """
    Select rail links and nodes to represent a base network in the
    specified year.
    """
    # Connect to GDB.
    arcpy.env.workspace = str(gdb.joinpath('railnet'))
    print(arcpy.env.workspace)
        
    arcpy.management.Delete('arc_lyr')
    arcpy.management.Delete('future_lyr')
    arcpy.management.Delete('node_lyr')
    arcpy.management.MakeFeatureLayer('railnet_arc', 'arc_lyr')
    arcpy.management.MakeFeatureLayer('future', 'future_lyr')
    arcpy.management.MakeFeatureLayer('railnet_node', 'node_lyr')
        
    # Select current links.
    arcpy.management.SelectLayerByLocation('arc_lyr',
                                           'SHARE_A_LINE_SEGMENT_WITH',
                                           'all_runs')
    # Add completed links from future.
    arcpy.management.SelectLayerByAttribute('future_lyr',
                                            where_clause=f"COMPLETION_YEAR <= {year} AND SCENARIO <> '9'")
    arcpy.management.SelectLayerByLocation('arc_lyr',
                                           'SHARE_A_LINE_SEGMENT_WITH',
                                           'future_lyr',
                                           selection_type='ADD_TO_SELECTION')
    # Remove links replaced by future.
    for ab in obsolete_links:
        arcpy.management.SelectLayerByAttribute('arc_lyr',
                                                'REMOVE_FROM_SELECTION',
                                                f'ANODE = {ab[0]} And BNODE = {ab[1]}')
    # Select nodes of remaining selected links.
    arcpy.management.SelectLayerByLocation('node_lyr',
                                           'INTERSECT',
                                           'arc_lyr')
    node_fields = ['NODE', 'POINT_X', 'POINT_Y', 'LABEL']
    link_fields = ['ANODE', 'BNODE', 'MILES', 'DIRECTIONS', 'MODES1', 'MODES2']
    node_array = arcpy.da.FeatureClassToNumPyArray('node_lyr', node_fields)
    link_array = arcpy.da.FeatureClassToNumPyArray('arc_lyr', link_fields)
    node_df = pd.DataFrame(node_array)
    link_df = pd.DataFrame(link_array)
    return node_df, link_df

def generate_railnet_file(node_df, link_df, out_file):
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
    _out_dir.mkdir(exist_ok=True)
    with open(out_file, 'w') as f:
        f.write('t nodes\n')
        f.write('c update centroid i xi yi ui1 ui2 ui3 lab\n')
    node.to_csv(out_file, sep=' ', header=False, index=False, mode='a', quotechar=' ')
    
    # Write link section to file.
    with open(out_file, 'a') as f:
        f.write('t links\n')
        f.write('c update i j length modes type lanes vdf\n')
    link.to_csv(out_file, sep=' ', header=False, index=False, mode='a', quotechar=' ')
    
    return out_file

def main():
    # Parse arguments.
    parser = argparse.ArgumentParser(description='prepare GTFS files to be read by Emme')
    parser.add_argument('--mrn_gdb_name',
                        help='name of MRN geodatabase')
    parser.add_argument('--year',
                        help='year of base network to generate')
    args = parser.parse_args()
    # Rail base network transaction file.
    node_df, link_df = filter_mrn(_in_dir.joinpath(args.mrn_gdb_name), args.year, rmv_links)
    # print(node_df.head())
    # print(link_df.head())
    generate_railnet_file(node_df, link_df, _out_dir.joinpath(f'mrn_{args.year}.txt'))


if __name__ == '__main__':
    sys.exit(main())