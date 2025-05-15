@echo off

cd %~dp0

call "C:\Program Files\ArcGIS\Pro\bin\Python\condabin\activate.bat" "arcgispro-py3"

python ".\src\generate_base_network.py" ^
    --mrn_gdb_name="mrn_c24q4.gdb" ^
    --year=2035
call conda deactivate
call "..\..\manage\env\activate_env.cmd" "emme"

@REM Base network
@REM python ".\src\update_transit_network.py" ^
@REM     --cta_feed=".\input\gtfs_cta_201907.zip" ^
@REM     --metra_feed=".\input\gtfs_metra_201907.zip" ^
@REM     --pace_feed=".\input\gtfs_pace_201907.zip" ^
@REM     --nictd_feed=".\input\gtfs_nictd_201707.zip" ^
@REM     --out_dir=".\output" ^
@REM     --rail_network=".\input\mrn_2019.txt" ^
@REM     --highway_network_nodes=".\input\10000.n1" ^
@REM     --highway_network_links=".\input\10000.l1" ^
@REM     --link_shape=".\input\linkshape_100.in"

@REM Current network
@REM python ".\src\update_transit_network.py" ^
@REM     --cta_feed="cta_202412.zip" ^
@REM     --metra_feed="metra_202501.zip" ^
@REM     --pace_feed="pace_202412.zip" ^
@REM     --nictd_feed="nictd_202408.zip" ^
@REM     --date="20241218"^
@REM     --rail_network=".\output\mrn_2024.txt" ^
@REM     --highway_network_nodes="20000.n1" ^
@REM     --highway_network_links="20000.l1" ^
@REM     --link_shape="linkshape_200.in" ^
@REM     --trip_aggregation="False"

@REM Alternate network
python ".\src\update_transit_network.py" ^
    --cta_feed="cta_202412.zip" ^
    --metra_feed="metra_snp_rev.zip" ^
    --pace_feed="pace_202412.zip" ^
    --nictd_feed="nictd_202408.zip" ^
    --date="20241218"^
    --rail_network=".\output\mrn_2035.txt" ^
    --highway_network_nodes="20000.n1" ^
    --highway_network_links="20000.l1" ^
    --link_shape="linkshape_400.in" ^
    --trip_aggregation="True"

python ".\src\transform_network_attributes.py"

pause