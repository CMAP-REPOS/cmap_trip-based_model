# initialize_scenarios.py (formerly initialize.scenarios emme macro) 
# ~/ INITIALIZE.SCENARIOS, kww 11/99, revised 5/05.
# ~#      Heither 01/06/09 - added line at end to overwrite bus.link file in database\data\ with new one.
# ~#      Heither 03/30/10 - added lines at end to copy M01 and DISTR files to appropriate location.
# ~#      Heither 09/14/10 - added line at end to copy MCHW_HH.TXT to appropriate location.
# ~#      Heither 04/15/11 - another program now copies M01/DISTR/MCHW_HH.TXT to appropriate location so 
# ~#                         coding from 03/30/10 & 09/14/10 updates removed.
# ~#      Heither 03/02/12 - dropped scenario %1%0 (copy of %1%00) because it serves no purpose.
# ~#      Heither 09/24/12 - copy linkshape file into data/.
# ~#      Heither 12/19/13 - points to new linkshape file location.
# ~#      Heither 09/15/14 - import linkshape information into highway scenarios.
# ~#      Ferguson 08/04/16 - now calls import.turn instead of import.dummy.turn to import turn prohibitions
# ~#      Heither 02/27/17 - implement vehicle class and time-of-day toll rates
# ~#	Bozic   03/23/2017 - implement bus veq as an extra highway attribute and set el2=@busveq
# ~#                           this means you should have the 8 period bus itineraries in the batchin file
# ~#                           REMOVE "INIT" FROM TRANSIT MODE BATCHIN FILE!
# ~#      Ferguson 8/20/2018: added @imarea to n2 batchin and updated toll rate definitions to use zone17
# ~#      OLeary 8/20/2024 - converted to Python, added transit assignment scenarios to setup, removed busveq (covered in net5I_7c.py now)
# ~#                         added reference to yaml file for setup
# ~#  =========================================================================================
# ~/  1. CREATE 8 HIGHWAY SCENARIOS REQUIRED FOR REGIONAL ANALYSIS
# ~/  2. CREATE 4 TRANSIT ASSIGNMENT SCENARIOS FOR BUSVEQ CALCULATIONS
# ~/  3. CREATE EXTRA ATTIBUTE TABLES 
# ~/  4. IMPORT 8 SETS OF HIGHWAY NETWORK DATA
# ~/  4. IMPORT 4 SETS OF TRANSIT NETWORK DATA
# ~/
# ~#  =========================================================================================

##
## SCRIPT SETUP ##
##

#import libraries
import os 
import sys
import yaml
from datetime import date
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm
sys.path.append(str(Path(__file__).resolve().parents[1].joinpath('macros')))
import Ftime_capacity as fc
import Arterial_delay as ad

#import variables

#find cmap_trip-based_model folder
db = Path(__file__).resolve().parents[1]  # database folder
proj_dir = db.parent  # cmap_trip-based_model folder

#set path to batchin files
with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)
scen_yr = config['scenario_code']  # e.g., '200'
batchin_path = config['transactionFilePath']  # e.g., M:/catslib/modelprod/c24q2

hwy_batchin_dir = os.path.join(batchin_path, 'highway')
trn_batchin_dir = os.path.join(batchin_path, 'transit')
print('highway transaction file location: ', hwy_batchin_dir)
print('transit transaction file location: ', trn_batchin_dir)

##
## EMME SETUP #####
##

# Connect to modeller
modeller = tbm.connect(proj_dir)
emmebank = modeller.emmebank

#define necessary emme modeller tools

#expanding database in case it's not large enough
change_database_properties = modeller.tool("inro.emme.data.database.change_database_properties")
change_database_dimensions = modeller.tool("inro.emme.data.database.change_database_dimensions")
change_primary_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")

#creating base highway network scens (e.g., 30001-30008)
#and transit assignment scenarios (e.g., 321, 323, 325, 327)
create_scenario = modeller.tool("inro.emme.data.scenario.create_scenario")
copy_scenario = modeller.tool("inro.emme.data.scenario.copy_scenario")

#transaction and attribute tools to build/populate networks
process_mode_transaction = modeller.tool("inro.emme.data.network.mode.mode_transaction")
process_vehicle_transaction = modeller.tool("inro.emme.data.network.transit.vehicle_transaction")
process_network_transaction = modeller.tool("inro.emme.data.network.base.base_network_transaction")
process_transit_line_transaction = modeller.tool("inro.emme.data.network.transit.transit_line_transaction")
create_extra = modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
import_attribute_values = modeller.tool("inro.emme.data.network.import_attribute_values")
net_calc = modeller.tool("inro.emme.network_calculation.network_calculator")
delete_function = modeller.tool("inro.emme.data.function.delete_function")
create_function = modeller.tool("inro.emme.data.function.create_function")

#for summary files -- haven't finished this yet - tko 8/29/2024
# export_base_network = modeller.tool("inro.emme.data.network.base.export_base_network")

#for running import.turn
run_macro = modeller.tool("inro.emme.prompt.run_macro")

#find and import Ftime_capacity.py and Arterial_delay.py -- need attributes for highway skim
sys.path.append(db)
from macros import Ftime_capacity as fc
from macros import Arterial_delay as ad

##
## INITIALIZE HIGHWAY SCENARIOS
##

print('create base daily network - all links... ')
base_scen = int(f'{scen_yr}00')

create_scenario(
    scenario_id = base_scen,
    scenario_title = 'base daily network - all links',
    overwrite = True,
    set_as_primary = False,
    emmebank = emmebank
)

process_mode_transaction(
    transaction_file = os.path.join(db, 'data/modes.in'),
    revert_on_error = True,
    scenario = emmebank.scenario(base_scen)
)

print('prepping database...')

change_database_properties(
    emmebank_node_number_digits = 5,
    emmebank = emmebank
)

#prep for trnt asmt scenarios (need more full matrices) -- if not already completed
if emmebank.dimensions['full_matrices']<1999:
    print('  - not enough full matrices for run. expanding emmebank now...')
    new_dimensions=emmebank.dimensions
    new_dimensions['full_matrices']=1999
    change_database_dimensions(
        emmebank_dimensions=new_dimensions,
        keep_backup=False
    )

print('create extra attributes')
new_attributes=[
    ['@zone', 'NODE', 'CMAP zone17'],
    ['@atype', 'NODE', 'CMAP capacity zone'],
    ['@imarea', 'NODE', 'IM area flag'],
    ['@speed', 'LINK', 'posted speed'],
    ['@width', 'LINK', 'average lane width'],
    ['@parkl', 'LINK', 'number of park lanes'],
    ['@toll', 'LINK', 'auto toll (dollars)'],
    ['@toll2', 'LINK', 'light truck toll (dollars)'],
    ['@toll3', 'LINK', 'medium truck toll (dollars)'],
    ['@toll4', 'LINK', 'heavy truck toll (dollars)'],
    ['@sigic', 'LINK', 'signal interconnect present'],
    ['@tipid', 'LINK', 'most recently applied tip id']
]

for attribute in new_attributes:
    create_extra(
        extra_attribute_name = attribute[0],
        extra_attribute_type = attribute[1],
        extra_attribute_description = attribute[2],
        overwrite = True,
        scenario = emmebank.scenario(base_scen)
    )
    
batchin_scens = [
    ['01', 'period 1.  8 pm to  6 am'],
    ['02', 'period 2.  6 am to  7 am'],
    ['03', 'period 3.  7 am to  9 am'],
    ['04', 'period 4.  9 am to  10 am'],
    ['05', 'period 5.  10 am to  2 pm'],
    ['06', 'period 6.  2 pm to  4 pm'],
    ['07', 'period 7.  4 pm to  6 pm'],
    ['08', 'period 8.  6 pm to  8 pm'],
    ['00', 'base daily network - all links']
]

print('creating tod highway scenarios...')
for bscen in batchin_scens:
    
    scen = int(f'{scen_yr}{bscen[0]}')
    tod = int(bscen[0][-1])
    print('    building scenario ', scen)
    # print('    building scenario ', scen, '\n  - tod: ', tod, '\n  - title: ', bscen[1])
    
    if bscen[0] != '00':
        copy_scenario(
            from_scenario = emmebank.scenario(base_scen),
            scenario_id = scen,
            scenario_title = bscen[1],
            overwrite=True
        )
    
    change_primary_scenario(emmebank.scenario(scen))
    
    print('      -- l1, n1...')
    #n1, l1 batchin
    l1_batchin = os.path.join(hwy_batchin_dir, str(scen_yr), f'{scen}.l1')
    n1_batchin = os.path.join(hwy_batchin_dir, str(scen_yr), f'{scen}.n1')
    
    process_network_transaction(
        transaction_file = n1_batchin
        # scenario = emmebank.scenario(scen)
    )

    process_network_transaction(
        transaction_file = l1_batchin
        # scenario = emmebank.scenario(scen)
    )
    
    print('      -- l2, n2...')
    
    #n2 batchin (extra attributes)
    n2_batchin = os.path.join(hwy_batchin_dir, str(scen_yr), f'{scen}.n2')
    n2_columns = {0: 'i_node', 1:'@zone', 2:'@atype', 3:'@imarea'}
    
    import_attribute_values(
        file_path=n2_batchin,
        column_labels=n2_columns
        # scenario = emmebank.scenario(scen)
    )
    
    #l2 batchin (extra attributes)
    l2_batchin = os.path.join(hwy_batchin_dir, str(scen_yr), f'{scen}.l2')
    l2_columns = {0: 'i_node', 1:'j_node', 2: '@speed', 3:'@width', 4:'@parkl', 6:'@toll', 7:'@sigic', 9: '@tipid'}
    
    import_attribute_values(
        file_path=l2_batchin,
        column_labels=l2_columns,
        # scenario = emmebank.scenario(scen)
    )
    
    print('      -- other link calculations...')
    
    truck_toll_factors = [
        [3.00, 'Light truck, day (p. 2-8)'], #r2
        [4.50, 'Medium truck, day (p. 2-8)'], #r3
        [8.00, 'Heavy truck, day (p. 2-8)'], #r4
        [2.00, 'Light truck, night (p. 1)'], #r5
        [3.50, 'Medium truck, night (p. 1)'], #r6
        [6.00, 'Heavy truck, night (p. 1)'], #r7
        [3.40, 'Skyway Medium truck, day (p. 2-8)'], #r8
        [5.60, 'Skyway Heavy truck, day (p. 2-8)'], #r9
        [2.40, 'Skyway Medium truck, night (p. 1)'], #r10
        [4.00, 'Skyway Heavy truck, night (p. 1)'], #r11
        [3.30, 'Indiana Tollway Medium truck, all periods'], #r12
        [9.00, 'Indiana Tollway Heavy truck, all periods'] #r13
    ]
    
    truck_toll_factors={
        'day': {
            'ltruck' : 3.00, #r2
            'mtruck' : 4.50, #r3
            'htruck' : 8.00, #r4
            'skyway_mtruck' : 3.40, #r8
            'skyway_htruck' : 5.60, #r9
        },
        'night': {
            'ltruck' : 2.00, #r5
            'mtruck' : 3.50, #r6
            'htruck' : 6.00, #r7
            'skyway_mtruck' : 2.40, #r10
            'skyway_htruck' : 4.00 #r11
        },
        'all_periods': {
            'indiana_toll_mtruck' : 3.30, #r12
            'indiana_toll_htruck' : 9.00 #r13
        }
    }
    
    #TRUCK TOLL RATES
    if tod==1:
        factors = truck_toll_factors['night']
    else:
        factors = truck_toll_factors['day']
    
    # -- ISTHA rates
    #ltruck - @toll2
    net_calc(
        specification={
            "result":"@toll2",
            "expression":f"@toll*{factors['ltruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=1,3247"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    #mtruck - @toll3
    net_calc(
        specification={
            "result":"@toll3",
            "expression":f"@toll*{factors['mtruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=1,3247"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    #htruck - @toll4
    net_calc(
        specification={
            "result":"@toll4",
            "expression":f"@toll*{factors['htruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=1,3247"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    # -- SKYWAY RATES
    #ltruck - @toll2
    net_calc(
        specification={
            "result":"@toll2",
            "expression":"@toll",
            "aggregation":None,
            "selections":{"link":"@zone=650 or @zone=697,699"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    #mtruck - @toll3
    net_calc(
        specification={
            "result":"@toll3",
            "expression":f"@toll*{factors['skyway_mtruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=650 or @zone=697,699"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    #htruck - @toll4
    net_calc(
        specification={
            "result":"@toll4",
            "expression":f"@toll*{factors['skyway_htruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=650 or @zone=697,699"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    # -- INDIANA TOLLWAY (NO TOD PRICING)
    in_factors = truck_toll_factors['all_periods']
    net_calc(
        specification={
            "result":"@toll2",
            "expression":"@toll",
            "aggregation":None,
            "selections":{"link":"@zone=3248,3467"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    net_calc(
        specification={
            "result":"@toll3",
            "expression":f"@toll*{in_factors['indiana_toll_mtruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=3248,3467"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )
    net_calc(
        specification={
            "result":"@toll4",
            "expression":f"@toll*{in_factors['indiana_toll_htruck']}",
            "aggregation":None,
            "selections":{"link":"@zone=3248,3467"},
            "type":"NETWORK_CALCULATION"
        }
        # scenario=emmebank.scenario(scen)
    )

    #setup network for highway skims
    run_macro(os.path.join(db, 'prep_macros/call/import.turn')) #import.turn macro
    fc.link_capacity() #ftime_capacity.py
    ad.arterial_delay() #arterial_delay.py
    
    
print('completed highway batchin. proceeding to transit...')

#######################
## BUILD TRANSIT ASSIGNMENT SCENARIO NETWORKS 
#######################

scen_yr = str(scen_yr) #just to ensure it works here - convert to string
scen3_yr4 = {
    '100': 2019,
    '200': 2025, 
    '300': 2030,
    '400': 2035,
    '500': 2040,
    '600': 2045,
    '700': 2050
}

network_batchin_list = [
    #[{transit asmt scenario number}, {transaction file time-of-day suffix}, {name of scenario}]
    [int(scen_yr)+21, 1, f'{scen3_yr4[scen_yr]} Night (6pm-6am)'],
    [int(scen_yr)+23, 2, f'{scen3_yr4[scen_yr]} AM (6am-9am)'],
    [int(scen_yr)+25, 3, f'{scen3_yr4[scen_yr]} Midday (9am-4pm)'],
    [int(scen_yr)+27, 4, f'{scen3_yr4[scen_yr]} PM (4pm-6pm)']
]

for asmt_scen in network_batchin_list:
    
    print(f'processing scenario {asmt_scen[0]}')

    #create transit assignment scenario
    create_scenario(
        scenario_id=asmt_scen[0],
        scenario_title=asmt_scen[2],
        overwrite=True
    )
    change_primary_scenario(emmebank.scenario(asmt_scen[0]))
    
    #specifies where to put the "report" file
    report = os.path.join(db, f'report/build_{asmt_scen[0]}transit.rpt')
    if os.path.exists(report):
        os.remove(report)
    modeller.desktop.modeller_tool_report_path = report
    
    #build transit network
    transit_modes = os.path.join(trn_batchin_dir, 'tranmodes.txt')
    transit_vehicles = os.path.join(trn_batchin_dir, 'transveh.txt')
    transit_transaction = os.path.join(trn_batchin_dir, str(scen_yr))
    
    print('  - import modes and vehicles')
    #import modes and vehicles
    process_mode_transaction(transaction_file=transit_modes)
    process_vehicle_transaction(transaction_file=transit_vehicles)
    print('  - import network')
    #import network
    process_network_transaction(os.path.join(transit_transaction, f'bus.network_{asmt_scen[1]}'))
    process_network_transaction(os.path.join(transit_transaction, f'rail.network_{asmt_scen[1]}'))
    process_network_transaction(os.path.join(transit_transaction, f'access.network_{asmt_scen[1]}'))
    print('  - import lines')
    #import lines
    process_transit_line_transaction(os.path.join(transit_transaction, f'rail.itinerary_{asmt_scen[1]}'))
    process_transit_line_transaction(os.path.join(transit_transaction, f'bus.itinerary_{asmt_scen[1]}'))
    
    print('  - create and calculate extra attributes')
    #create extra attributes
    atts = [
        ['@pcost', 'NODE', 'avg. daily parking cost at station'],
        ['@atype', 'NODE', 'area type for on-street parking'],
        ['@pspac', 'NODE', 'off-street parking spaces at station'],
        ['@zone', 'NODE', 'CMAP zone'],
        ['@ltime', 'TRANSIT_SEGMENT', 'line service time in minutes'],
        ['@hwytm', 'TRANSIT_SEGMENT', f'AM peak congested hwy time from scen {scen_yr}'],
        ['@zfare_link', 'TRANSIT_SEGMENT', 'incremental zone fare'],
        ['@timbo', 'NODE', 'Base boarding time by station type, min'],
        ['@easeb', 'TRANSIT_LINE', 'Ease of boarding 1=worst, 4=best']
    ]
    
    for att in atts:
        create_extra(
            extra_attribute_name=att[0],
            extra_attribute_type=att[1],
            extra_attribute_description=att[2],
            extra_attribute_default_value=0,
            overwrite=True
        )
        
    #set attribute values
    calcs = [
        ['@ltime', 'us1'],
        ['us1', '0'],
        ['@zfare_link', 'us2'],
        ['us2', '0']
    ]
    
    for calc in calcs:
        net_calc(
            specification= {
                "result": calc[0],
                "expression": calc[1],
                "aggregation": None,
                "selections": {
                    "link": "all",
                    "transit_line": "all"
                },
                "type": "NETWORK_CALCULATION"
            },
        )
    
    print('  - extra attribute transactions')
    
    #import bus node extra attributes  
    import_attribute_values(
        file_path = os.path.join(transit_transaction, f'busnode.extatt_{asmt_scen[1]}'),
        column_labels= {0:'inode', 1:'@atype', 2:'@zone', 3:'@pspac', 4:'@pcost'}
    )
    
    import_attribute_values(
        file_path = os.path.join(trn_batchin_dir, 'bus_node_extra_attributes.csv'),
        column_labels={0:'inode', 3:'@timbo'},
        field_separator=',',
        revert_on_error=False
    )
    
    #import rail node extra attributes
    import_attribute_values(
        file_path = os.path.join(transit_transaction, f'railnode.extatt_{asmt_scen[1]}'),
        column_labels = {0: 'inode', 1: '@pspac', 2: '@pcost', 3: '@zone'}
    )
    
    # !! OLEARY 9/25/2024: the macro only imports these values for nodes 1 through 6, which are all 0??
    # import_attribute_values(
    #     file_path = os.path.join(trn_batchin_dir, 'rail_node_extra_attributes.csv'),
    #     column_labels= {0: 'inode', 3: '@timbo'},
    #     field_separator=',',
    #     revert_on_error=False
    # )
    
    #set minimum values for boarding time -- minimum 0.5 for zones 5000-29999, 1.5 for 30000-49999
    minimums = [
        ['@timbo .max. 0.5', '5000,29999'],
        ['@timbo .max. 1.5', '30000, 49999']
    ]
    for min in minimums:
        net_calc(
            specification={
                "expression": min[0],
                "result": "@timbo",
                "aggregation": None,
                "selections": {"node":min[1]},
                "type": "NETWORK_CALCULATION"
            }
        )
    
    #ease of boarding import, set minimums, minor adjustments
    import_attribute_values(
        file_path=os.path.join(trn_batchin_dir, 'boarding_ease_by_line_id.csv'),
        column_labels={0:'line', 1:'@easeb'},
        field_separator=',',
        revert_on_error=False
    )
    
    #some corrections per mode, line
    minimums = [
        ['@easeb.max.2', 'mode=BELPQ'],
        ['@easeb.max.3', 'mode=C'],
        ['@easeb.max.1', 'mode=M'],
        ['1', 'mode=P'],
        ['2', 'line=cbl___'],
        ['2', 'line=mbn___'],
        ['0', 'line=mme___'],
        ['0', 'line=mnw___'],
        ['0', 'line=mu____'],
        ['1', 'line=cpr___'],
    ]
    for min in minimums:
        net_calc(
            specification={
                "expression":min[0],
                "result":"@easeb",
                "aggregation":None,
                "selections":{"transit_line": min[1]},
                "type":"NETWORK_CALCULATION"
            }
        )
        
    #increase wait convenience factor for MME & CBL
    wait_convenience_lines = [
        ["1", "ut3", "line=mme___"],
        ["1", "ut3", "line=cbl___"]
    ]    
    
    for calc in wait_convenience_lines:
        net_calc(
            specification={
                "expression":calc[0],
                "result":calc[1],
                "aggregation":None,
                "selections":{"transit_line":calc[2]},
                "type":"NETWORK_CALCULATION"
            }
        )
    
    wait_convenience_nodes = [
        ['ut3*10', 'ui3', 'line=mme___'],
        ['ut3*5', 'ui3', 'line=cbl___']
    ]
    
    for calc in wait_convenience_nodes:
        net_calc(
            specification={
                "expression":calc[0],
                "result":calc[1],
                "aggregation":".max.",
                "selections":{"link":"all", "transit_line":calc[2]},
                "type":"NETWORK_CALCULATION"
            }
        )
    
    #initialize function table
    #ft1 - us1
    #ft2 - us1
    #they will be transit time functions -- 1 is normal, 2 is brt

    for ft in ['ft1', 'ft2']:
        try:
            delete_function(ft)
        except:
            pass
        
        create_function(
            function_id = ft,
            function_expression = 'us1'
        )



    ##################################################
    ## GENERATE REPORT ##
    ##################################################
    
    # series of netcalcs for report -- 
    # keys of dictionary represent report categories
    # results in dictionary are parameters for emme netcalc specification
    
    #x21, x23, x25, x27 where x is scenario year
    period_length = {
        '21': 12,
        '23': 3,
        '25': 7,
        '27': 2
    }
    
    # asmt_scen[0] is 3-digit trnt asmt number-- we want last two digits
    # str(asmt_scen[0])[-2:] ==> '21' (for example)
    trn_tod = str(asmt_scen[0])[-2:]
    hrs_per_tod = period_length[trn_tod]
    
    net_calcs = {
        'dirmiles_svctype':
            # [result, expression, link selection]
            [
                [None, 'length', 'mode=BE'],
                [None, 'length', 'mode=LPQ'],
                [None, 'length', 'mode=C'],
                [None, 'length', 'mode=M']
            ],
            
        'svc_mi_by_type':
            # [result, expression, link selection, transit line selection]
            [
                ['us2', f'{hrs_per_tod}/hdw*length', 'mode=BE', 'mode=BE'],
                ['us2', '0', 'all', 'all'],
                ['us2', f'{hrs_per_tod}/hdw*length', 'mode=LPQ', 'mode=LPQ'],
                ['us2', '0', 'all', 'all'],
                ['us2', f'{hrs_per_tod}/hdw*length', 'mode=C', 'mode=C'],
                ['us2', '0', 'all', 'all'],
                ['us2', 'length', 'mode=M', 'mode=M'],
                ['us2', '0', 'all', 'all']      
            ],
        'svc_hrs_by_type':
            # [result, expression, link selection, transit line selection]
            [
                ['us2', f'({hrs_per_tod}/hdw*@ltime)/60', 'mode=BE', 'mode=BE'],
                ['us2', '0', 'all', 'all'],
                ['us2', f'({hrs_per_tod}/hdw*@ltime)/60', 'mode=LPQ', 'mode=LPQ'],
                ['us2', '0', 'all', 'all'],
                ['us2', f'({hrs_per_tod}/hdw*@ltime)/60', 'mode=C', 'mode=C'],
                ['us2', '0', 'all', 'all'],
                ['us2', '@ltime/60', 'mode=M', 'mode=M'],
                ['us2', '0', 'all', 'all'],
            ],
        'bus_route_vmt':
            # [result, expression, link selection, transit line selection]
            [
                ['us2', 'length', 'all', 'mode=BEPLQ'],
                ['us2', '0', 'all', 'all']
            ],
        'parking':
            # [result, expression, node selection]
            [
                [None, '@pcost', 'all'],
                [None, '@pspac', 'all']
            ]
    }
    
    ##################################################
    with open(report, 'a') as file:
        file.write(f'''
            ~" =============================================================================================
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            ~"   -- NETWORK {asmt_scen[0]}: DIRECTIONAL MILES BY SERVICE TYPE --
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        '''
        )    

    for calc in net_calcs['dirmiles_svctype']:
        net_calc(
            specification={
                "result":calc[0],
                "expression":calc[1],
                "aggregation":None,
                "selections":{'link':calc[2]},
                "type":"NETWORK_CALCULATION"
            }
        )

    ####################################################
    with open(report, 'a') as file:
        file.write(f'''
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            ~"   -- NETWORK {asmt_scen[0]}: SERVICE MILES BY SERVICE TYPE --
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        '''
        )
    
    for calc in net_calcs['svc_mi_by_type']:
        net_calc(
            specification= {
                "result":calc[0],
                "expression":calc[1],
                "aggregation":None,
                "selections": {'link':calc[2], 'transit_line':calc[3]},
                "type":"NETWORK_CALCULATION"
            }
        )    
    
    ##################################################
    with open(report, 'a') as file:
        file.write(f'''
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            ~"   -- NETWORK {asmt_scen[0]}: SERVICE HOURS BY SERVICE TYPE --
            ~" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        '''
        )
    
    for calc in net_calcs['svc_hrs_by_type']:
        net_calc(
            specification= {
                "result":calc[0],
                "expression":calc[1],
                "aggregation":None,
                "selections": {'link':calc[2], 'transit_line':calc[3]},
                "type":"NETWORK_CALCULATION"
            }
        )         

    with open(report, 'a') as file:
        file.write(f'''~" ==========================================================================''')
        file.write('-- REPORT BUS ROUTE VMT --')

    for calc in net_calcs['bus_route_vmt']:
        net_calc(
            specification= {
                "result":calc[0],
                "expression":calc[1],
                "aggregation":None,
                "selections":{'link':calc[2], 'transit_line':calc[3]},
                "type":"NETWORK_CALCULATION"
            }
        )
        
    with open(report, 'a') as file:
        file.write('-- REPORT PARKING DATA --')
    
    for calc in net_calcs['parking']:
        net_calc(
            specification={
                "result":calc[0],
                "expression":calc[1],
                "aggregation":None,
                "selections":{'node':calc[2]},
                "type":"NETWORK_CALCULATION"
            }
        )
    

    with open(report, 'a') as file:
        file.write('-- REPORT NETWORK SUMMARY --')

## Create transit skim scenarios ##
today = str(date.today().strftime('%Y%m%d'))
copy_scenario(
            from_scenario = emmebank.scenario(int(scen_yr)+23),
            scenario_id = int(scen_yr)+0,
            scenario_title = f'{scen3_yr4[scen_yr]} am (6am-9am) transit skim network - {today}',
			copy_linkshapes=True,
            overwrite=True
        )
copy_scenario(
            from_scenario = emmebank.scenario(int(scen_yr)+25),
            scenario_id = int(scen_yr)+5,
            scenario_title = f'{scen3_yr4[scen_yr]} midday (9am-4pm) transit skim network - {today}',
			copy_linkshapes=True,
            overwrite=True
        )

### BASE NETWORK SUMMARY -- COMMENTED OUT -- TAKING TOO LONG 


#     print(f'processing scenario - network summary {asmt_scen[0]}')
    

    
#     base_network_temp_dir = os.path.join(db, 'temp')
#     export_base_network(
#         selection={'node':'all', 'link':'all'},
#         export_file = base_network_temp_dir,
#         append_to_file=False,
#         field_separator=';',
#         export_format='PROMPT_DATA_FORMAT'
#     )
    
#     with open(base_network_temp_dir, 'r') as net_file:
#         summary_tables=[]
#         for line in file.readlines()[5:]:
#             if not line.startswith('a'):
#                 summary_tables.append([])
#                 summary_tables[-1].append(line.split(',').strip())
#             if line.startswith('a'):
#                 summary_tables[-1].append(line.split(',').strip())
                
    
    
# base_network_temp_dir = r"E:\tko\macro_conversion\net5i7c_busveq\base_network_321.csv"

# export_base_network(
#     selection={'node':'all', 'link':'all'},
#     export_file = base_network_temp_dir,
#     append_to_file=False,
#     field_separator=';',
#     export_format='PROMPT_DATA_FORMAT'
# )




# with open(base_network_temp_dir, 'r') as net_file:
#     summary_tables=[]
#     for line in net_file.readlines()[5:]:
#         if not line.startswith('a'):
#             #if the line doesn't start with 'a', it's a new table-- 
#             # -- so, this will create a new list at end of summary_tables to store the table
#             summary_tables.append([])
#             # clean up row to be list of entries, instead of one quoted string
#             # list comprehension will split by separator ';' and get rid of excess spaces
#             row = [col.strip() for col in line.split(';')]
#             summary_tables[-1].append(row)
#         if line.startswith('a'):
#             row = [col.strip() for col in line.split(';')]
#             #add row to whatever table we're in
#             summary_tables[-1].append(row)

# # os.remove(base_network_temp_dir)

# fin_tables = []
# for summary_table in summary_tables:
#     if len(summary_table) > 5:
#         fin_tables.append(summary_table)


      
# node_table = pd.DataFrame(data=fin_tables[0][1:])
# link_table = pd.DataFrame(data=fin_tables[1][1:])
# def centroid(x):
#     if '*' in x:
#         return 1
#     else:
#         return 0

# node_table['centroid'] = node_table[0].map(centroid)

# def node(x):
#     return x.split(' ')[-1]

# node_table['node'] = node_table[0].map(node)

# centroids = node_table['centroid'].sum()
# nodes = len(node_table)
# links = len(link_table)
# link_length = link_table[3].sum()

print('scenario setup complete!')
