@echo off

cd %~dp0

call "..\..\manage\env\activate_env.cmd" "emme"
python ".\src\update_transit_network.py" ^
    --cta_feed=".\input\gtfs_cta_201907.zip" ^
    --metra_feed=".\input\gtfs_metra_201907.zip" ^
    --pace_feed=".\input\gtfs_pace_201907.zip" ^
    --nictd_feed=".\input\gtfs_nictd_201707.zip" ^
    --out_dir=".\output" ^
    --rail_network=".\input\mrn_2019.txt" ^
    --highway_network_nodes=".\input\10000.n1" ^
    --highway_network_links=".\input\10000.l1" ^
    --link_shape=".\input\linkshape_100.in"

pause