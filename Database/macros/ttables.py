'''
ttables.py
Author: T OLeary, K Cazzato, translated from ttables.mac 2025-11
'''

#libraries
import os
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import yaml

#connect to modeller
cwd = Path(__file__).resolve()
print(cwd)
tbm_folder = None
for parent in [cwd] + list(cwd.parents):
    print(parent)
    maybe_script_dir = parent.joinpath('Scripts')
    maybe_db_dir = parent.joinpath('Database')
    if maybe_script_dir.is_dir() and maybe_db_dir.is_dir():
        tbm_folder = parent
        db_dir = maybe_db_dir
        break
if not tbm_folder:
    raise FileNotFoundError('ttables.py: Could not find appropriate directories!')

sys.path.append(str(tbm_folder.joinpath('Scripts').resolve()))
from tbmtools import project as tbm

modeller = tbm.connect(tbm_folder)
eb = modeller.emmebank
db_dir = tbm_folder.joinpath('Database')

# process sys arguments (if given-- otherwise, can grab from batch_file.yaml)
#time of day period
try:
    tod = sys.argv[1]
    try:
        tod = int(tod)
    except ValueError:
        raise ValueError(f'ttables.py: invalid tod argument: {sys.argv[1]} -- must be integer')
except IndexError:
    tod = None #if sys argument not passed

#3-digit scenario
try:
    scen_3dig = sys.argv[2]
    try:
        scen_3dig = int(scen_3dig)
    except ValueError:
        raise ValueError(f'ttables.py: invalid scenario argument: {sys.argv[2]} -- must be 3-digit scenario number')
except IndexError:
    scen_3dig = None #if sys argument not passed

def ttables(tod, scen_3dig=None, db_dir=None):
    '''
    Description: Function to generate TOD trip tables for assignment
    Parameters:
        - tod (str or int): time-of-day period (1, 2, ..., 8)
        - scen_3dig (str or int): 3-digit scenario (100, 200, ..., 700)
        - db_dir (string or Pathlike): directory to Database folder
    '''
    
    if not isinstance(tod, int):
        try: 
            tod = int(tod)
        except ValueError:
            raise ValueError(f'ttables.py: invalid tod argument: {tod} must be integer')
    
    if db_dir:
        try:
            db_dir = Path(db_dir)
            if not db_dir.exists():
                raise FileNotFoundError(f'ttables.py: database folder does not exist: {db_dir}')
        except ValueError:
            raise ValueError(f'ttables.py: db_dir parameter must be a string or Path-like object: {db_dir}')
    else:
        cwd = Path(__file__).resolve()
        tbm_folder = None
        for parent in [cwd] + list(cwd.parents):
            print(parent)
            maybe_script_dir = parent.joinpath('Scripts')
            maybe_db_dir = parent.joinpath('Database')
            if maybe_script_dir.is_dir() and maybe_db_dir.is_dir():
                tbm_folder = parent
                db_dir = maybe_db_dir
                break
        if not db_dir:
            raise FileNotFoundError(f'ttables.py: Could not find "Database" folder in any parent folder from {__file__}')
    
    if scen_3dig: 
        try:
            scen_3dig = int(scen_3dig)
        except ValueError: 
            raise ValueError(f'ttables.py: scen_3dig parameter must be 3-digit scenario: 100, 200,...,700: {scen_3dig}')
    else:
        batch_yaml = Path.joinpath(db_dir, 'batch_file.yaml')
        with open(batch_yaml) as f:
            linesnobackslash = f.read().replace('\\', '/')
            config = yaml.safe_load(linesnobackslash)
        scen_3dig = config['scenario_code']  # e.g., '400'

    #parameters from trip_tables_params.yaml
    trip_params = Path.joinpath(db_dir, 'trip_tables_params.yaml')
    with open(trip_params, "r") as f:
        factors = yaml.safe_load(f)
    
    #parameters from emme
    glob_iter = int(eb.matrix('ms98').data) # global iteration value
    emme_scen = eb.scenario(f'{scen_3dig}{glob_iter}{tod}')

    #input files
    #function config files
    vdf_tod_dir = Path.joinpath(db_dir, 'data', 'vdf_tod.in')
    tod_flags_dir = Path.joinpath(db_dir, 'data', 'tod_flags.in')

    #output directory
    report_dir = Path.joinpath(db_dir, 'report', f'iter_{glob_iter}')
    if not report_dir.exists():
        report_dir.mkdir(parents=True)
    report = report_dir.joinpath('tod_veh_trips_and_VOT.txt')

    #Emme tools
    mtx_calc = modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
    create_matrix = modeller.tool("inro.emme.data.matrix.create_matrix")
    delete_function = modeller.tool("inro.emme.data.function.delete_function")
    function_transaction = modeller.tool("inro.emme.data.function.function_transaction")
    mtx_triple_index = modeller.tool("inro.emme.matrix_calculation.matrix_triple_index_operation")
    mtx_transaction = modeller.tool("inro.emme.data.matrix.matrix_transaction")

    print(f'begin ttables.py - glob iter {glob_iter}, tod {tod}')

    # -- IMPORT VDF FUNCTIONS -- #
    for fn in range(1,11):
        if eb.function(f'fd{fn}'):
            delete_function(f'fd{fn}')
    function_transaction(vdf_tod_dir)

    # -- CREATE TOD FLAG MATRICES -- #
    #read batchin file for tod flags
    flag_list = []
    
    for mtx in ['mo5', 'md5']: 
        if eb.matrix(mtx):
            eb.delete_matrix(mtx)
    
    with open(tod_flags_dir, 'r') as f:
        rows = f.readlines()
        for row in rows:
            if any(row.startswith(x) for x in ['a', 'c', 'd', 't']):
                continue
            else:
                o = int(row.split()[0].strip())
                val = float(row.split(':')[-1].strip())
                flag_list.append([o, val])

    #create numpy arrays from batchin (mo5 and md5)
    #(array needs to be same size as zone structure-- used centroids)
    pqflag = np.zeros((eb.dimensions['centroids']), dtype=int)
    for centroid_id, value in flag_list:
        pqflag[centroid_id - 1] = value
    mo5_np = pqflag.copy()
    md5_np = pqflag.copy()

    #mo55, md55, and md56 are masks based on mo5 and md5 values:
    #create zeroed arrays, then apply masks based on tod_flags values (`pqflag`)
    mo55_np = np.zeros((eb.dimensions['centroids']), dtype=int) 
    md55_np = np.zeros((eb.dimensions['centroids']), dtype=int)
    md56_np = np.zeros((eb.dimensions['centroids']), dtype=int)
    mo55_np[pqflag >= 2] = 1 
    md55_np[pqflag == 0] = 1
    md56_np[pqflag >= 2] = 1

    #create and populate matrices
    mask_mtx_dict = {
        'mo5': ['mopflag', 'tod fraction flags', mo5_np],
        'md5': ['mdqflag', 'tod fraction flags', md5_np],
        'mo55': ['tempmo5', 'temporary matrix for ttables', mo55_np],
        'md55': ['tempmd5', 'temporary matrix if qflag = 0', md55_np],
        'md56': ['tempmd5', 'temporary matrix if qflag >= 2', md56_np]
    }

    for mtx, data in mask_mtx_dict.items():
        create_matrix(
            matrix_id=mtx,
            matrix_name=data[0],
            matrix_description=data[1],
            overwrite=True #will zero out if exists
        )
        eb.matrix(mtx).set_numpy_data(data[2], scenario_id=emme_scen)


    # perform transit convolution to get trips from origin to station
    # this matrix is needed for calculating park-and-ride trips
    # only necessary once during tod 1
    if tod == 1 or not eb.matrix('mf59'):
        if not eb.matrix('mf59'):
            eb.create_matrix('mf59')
            eb.matrix('mf59').name = 'parkrd'
            eb.matrix('mf59').description = 'drive to transit station'
        
        #combo HW transit demand matrix (low + high income)    
        hw_trn_hi = eb.matrix('mf40').get_numpy_data(emme_scen)
        hw_trn_lo = eb.matrix('mf41').get_numpy_data(emme_scen)
        hw_transit_trips = hw_trn_hi + hw_trn_lo

        #assign to matrix 20
        if eb.matrix('mf20'):
            eb.delete_matrix('mf20')
        eb.create_matrix('mf20')
        eb.matrix('mf20').name = 'hwtranst'
        eb.matrix('mf20').description = 'HW High and Low inc transit trips'
        eb.matrix('mf20').set_numpy_data(hw_transit_trips, emme_scen)
        
        if not eb.matrix('mo10'): 
            eb.create_matrix('mo10')
        eb.matrix('mo10').name = 'ptemp'
        eb.matrix('mo10').description = 'origins to extract first leg demand'
        mo10_np = np.zeros(eb.dimensions['centroids'], dtype=float)
        mo10_np[:factors['max_npoe']] = np.arange(1, factors['max_npoe']+1, dtype=float)
        eb.matrix('mo10').set_numpy_data(mo10_np, emme_scen)
        
        convolution_spec = {
            "pk_operand": "mf837",
            "kq_operand": None,
            "qk_operand": "mo10",
            "combination_operator": "==",
            "masks": [
                {
                    "operator": "*",
                    "pk_operand": "mf20",
                    "kq_operand": None,
                    "k_operand": None,
                    "constant_operand": None,
                    "pq_operand": None
                }
            ],
            "contraction_operator": "+",
            "result": "mf59",
            "index_result": None,
            "constraint": {
                "by_zone": {
                    "origins": f"1,{factors['max_npoe']}",
                    "destinations": f"1,{factors['max_npoe']}",
                    "intermediates": f"1,{factors['max_npoe']}"
                },
                "by_value": None
            },
            "type": "MATRIX_TRIPLE_INDEX_OPERATION"
        }
        
        # print('\t-computing matrix convolution, ~15 minutes')
        mtx_triple_index(specification=convolution_spec, scenario=emme_scen)
        # print('\t\t-success')
        
        
    # -------------
    # -- CREATE TRIP_TABLES DICTIONARY, AND CALCULATE TRIPS -- #
    # -------------

    #labels for matrix descriptions
    tod_labels = {
        1: 'p1. 8pm-6am',
        2: 'p2. 6am-7am',
        3: 'p3. 7am-9am',
        4: 'p4. 9am-10am',
        5: 'p5. 10am-2pm',
        6: 'p6. 2pm-4pm',
        7: 'p7. 4pm-6pm',
        8: 'p8. 6pm-8pm'
    }

    '''
    Create trip_tables dict, containing matrix ID, name, description, and calculation
    for time-of-day trips. 
    (1) mf14-17: trucks -- only need to multiply daily trips by tod factor
    (2) mf92-93: HOV -- only need to copy over demand matrices mf44x and mf45x
    (3) mf94-96: SOV -- need to add trips from several places:
        - copy over trips from demand model: mf41x-mf43x
        - add point-of-entry/external trips, times time-of-day factor, times vot factor
        - add airport trips, times time-of-day factor, times vot factor
        - add park-and-ride trips
    (4) mf18 (all HOV), mf97 (b-plate + light) -- add at end of procedure
        - mf18 (all HOV) = mf92 (HOV2) + mf93 (HOV3+)
        - mf97 (b-plate + light) = mf14 (b-plate) + mf15 (light truck)
    '''

    def sum_calc(spec, ms='ms15'):
        '''
        Helper function to calculate the aggregate sum of a given full matrix
        Parameters:
            - spec: the expression that goes into the "expression" parameter of a matrix calculation specification
            - ms (default: 'ms15'): the temporary scalar matrix in which to store values

        '''
        if not eb.matrix(ms):
            eb.create_matrix(ms)
            eb.matrix(ms).name = 'tmp15',
            eb.matrix(ms).description = 'temporary scalar matrix to store summary values'
            
        calc_spec = {
            "expression": spec,
            "result": ms,
            "constraint": {
                "by_value": None,
                "by_zone": None
            },
            "aggregation": {
                "origins": "+",
                "destinations": "+"
            },
            "type": "MATRIX_CALCULATION"
        }
        return mtx_calc(calc_spec)['result']


    trip_tables = { 
        # structure: 
        #   matrix ID: [
        #       matrix_name,
        #       matrix_description,
        #       calculation
        #   ]
    }
    report_items = {
        # structure:
        #   report_measure: value
    }

    # ------------
    # - (1)(2) ADD TRUCK AND HOV (mf14-17, mf92-93) TO TRIP_TABLES DICT, REPORT DICT - #
    # ------------

    truck_hov_tables = {         
        # mf14-17: truck trips
        "mf14" : [
            "bplt", 
            f"B-plate truck trips", 
            f"mf4 * {factors[f'factor_p{tod}']['btrk']}"
            ],
        "mf15" : [
            "ltrk", 
            f"Light truck trips",
            f"mf5 * {factors[f'factor_p{tod}']['ltrk']}"
            ], 
        "mf16" : [
            "mtrk", 
            f"Medium truck trips",
            f"(2 * mf6) * {factors[f'factor_p{tod}']['mtrk']}"
            ], 
        "mf17" : [
            "htrk", 
            f"Heavy truck trips",
            f"(3 * mf7) * {factors[f'factor_p{tod}']['htrk']} + (3 * mf9) * {factors[f'factor_p{tod}']['poetrk']}"
            ], 
        
        # mf92,93: hov2 and hov3+ trips
        "mf92" : [
            "modeH2", 
            f"HOV2 trips",
            f"mf44{tod}"
            ], 
        "mf93" : [
            "modeH3", 
            f"HOV3+ trips",
            f"mf45{tod}"
            ], 
    }

    #append specs to `trip_tables` and `report_items`
    for key, value in truck_hov_tables.items():
        trip_tables[key] = value
        report_items[value[1]] = value[2] 

    # ----------
    # - (3) ADD SOV CALCS TO TRIP_TABLES DICT, REPORT DICT - #
    # ----------
    '''
    - SOV trips require several steps to build matrix calculation expression:
    - 1. copy over demand from mf41{tod}-43{tod} (sov tod demand trip tables without poe/airport/pnr trips)
    - 2. add POE trips (using tod factors and shares)
    - 3. add airport trips (using tod factors and shares)
    - 4. add park-and-ride trips (using metra share, tod factors, and shares)
    '''

    sov_tables = {}
    sov_report = {}

    #create parameters dictionary for sov trips
    #used below to create calculation specs for trip_tables
    sov_params = {
        # matrix id: [
        #   base tod demand matrix,
        #   external share parameter,
        #   airport share parameter,
        #   park-and-ride share parameter,
        #   matrix name,
        #   matrix description
        #]
        
        #sov, vot1
        'mf94': [
            f'mf41{tod}',
            factors['shares']['poe_vot1'],
            factors['shares']['air_vot1'],
            factors['shares']['drive2trn_vot1'],
            "modeS1",
            f"SOV VOT1 trips"
            ], 
        #sov, vot2
        'mf95': [
            f'mf42{tod}', 
            factors['shares']['poe_vot2'],
            factors['shares']['air_vot2'],
            factors['shares']['drive2trn_vot2'],
            "modeS2", 
            f"SOV VOT2 trips"
            ], 
        #sov, vot3
        'mf96': [
            f'mf43{tod}', 
            factors['shares']['poe_vot3'],
            factors['shares']['air_vot3'],
            factors['shares']['drive2trn_vot3'],
            "modeS3", 
            f"SOV VOT3 trips"
            ]
    }

    #define specs for SOV poe/airport/pnr trips, and add to mtx calculation and report specs
    for mf, param in sov_params.items():
        #1. base demand: mf41x-43x (x == tod)
        calc_spec = str(param[0])
        sov_report[f'{param[5]}'] = str(param[0])
        #2. add POE trips ()
        poe_spec = f"(mf8*{factors[f'factor_p{tod}']['poe_auto']}*{param[1]})"
        calc_spec += f" + {poe_spec}"
        sov_report[f'{param[5]} External trips'] = poe_spec
        #3a. add trips to airport
        to_apt_spec = f"(mf10*md56*{factors[f'factor_p{tod}']['air_to']}*{param[2]})"
        calc_spec += f" + {to_apt_spec}"
        #3b. add trips from airport
        from_apt_spec = f"(mf10*mo55*md55*{factors[f'factor_p{tod}']['air_from']}*{param[2]})"
        calc_spec += f" + {from_apt_spec}"
        sov_report[f'{param[5]} Airport trips'] = f'{to_apt_spec} + {from_apt_spec}'
        #4. add park and ride trips
        avgrat_tofromcbd = f"({factors[f'factor_p{tod}']['sov_hbw2cbd']} + {factors[f'factor_p{tod}']['sov_hbwfmcbd']}) / 2"
        pnr_spec = f"mf59 *  {param[3]} * {factors[f'shares']['metra_board']} * {avgrat_tofromcbd}"
        calc_spec += f" + {pnr_spec}"
        sov_report[f'{param[5]} Park and Ride trips'] = pnr_spec
        
        #add SOV specs to trip_tables dict
        sov_tables[mf] = [
            param[4], #matrix name
            param[5], #matrix description
            calc_spec #matrix calculation
        ]
        
    for key, value in sov_tables.items():
        trip_tables[key] = value
    for key, value in sov_report.items():
        report_items[key] = value

    # ----------
    # - (4) ADD "B-PLATE+LIGHT" (mf97) AND "HOV2+HOV3" (mf18) TO TRIP_TABLES DICT, REPORT DICT - #
    # ----------
    summary_tables = {    # mf18 (hov2 + hov3), and mf97 (bplate + light)
        "mf18" : [
            "modeH", 
            f"HOV trips (HOV2 + HOV3)",
            'mf92 + mf93'
            ],
        "mf97" : [
            "bp_ltrk",
            f"B-Plate + light truck trips",
            'mf14 + mf15'
            ]
    }

    # -----
    # -- CALCULATE ALL FULL MATRIX TABLES (`trip_tables`)
    # -----

    for mf, namedesc in summary_tables.items():
        trip_tables[mf] = namedesc
        report_items[namedesc[1]] = namedesc[2]
    #calculate all matrices in trip_tables
    for mf, namedesc in trip_tables.items():
        # print(mf, namedesc[1])
        create_matrix(
            matrix_id=mf,
            matrix_name=namedesc[0],
            matrix_description=f'{namedesc[1]}: {tod_labels[tod]}',
            overwrite=True
        )
        mtx_calc_spec = {
            "type": "MATRIX_CALCULATION",
            "result": mf,
            "expression": namedesc[2],
        }
        mtx_calc(mtx_calc_spec, scenario=emme_scen)

    # -----
    # -- CALCULATE ALL SCALAR MATRIX VALUES (`report_calcs`)
    # -----

    report_calcs = {}
    #remove heavy from report_items, and re-add heavy and external separately
    report_items = {
        key: value for key, value in report_items.items() \
            if 'heavy truck' not in key.lower()
        }

    #calculations
    for rpt_item, spec in report_items.items():
        report_calcs[rpt_item] = sum_calc(spec)
    report_calcs[f'Heavy truck trips (excl. external)'] = (3 * sum_calc('mf7')) * factors[f'factor_p{tod}']['htrk']
    report_calcs[f'External truck trips'] = (3 * sum_calc('mf9')) * factors[f'factor_p{tod}']['poetrk']


    # calculate perceived vot relative to tolls ($/hour)
    # most of these exist as vot*perception from trip_tables_params.yaml, but
    # need to calculate weighted average on the summary trips (bplate+light, and hov2+hov3)
    hov2_trips = sum_calc('mf92')
    hov3_trips = sum_calc('mf93')
    bplt_trips = sum_calc('mf14')
    ltrk_trips = sum_calc('mf15')

    vot_hov23 = (
        (hov2_trips * (factors['vot']['hov2'] * factors['perception']['hov2'])) + 
        (hov3_trips * (factors['vot']['hov3'] * factors['perception']['hov3']))
        ) / (hov2_trips + hov3_trips)
    vot_bplt_ltrk = (
        (bplt_trips * (factors['vot']['btrk'] * factors['perception']['btrk'])) + 
        (ltrk_trips * (factors['vot']['ltrk'] * factors['perception']['ltrk']))
        ) / (bplt_trips + ltrk_trips)    


    # -- CALCULATE FINAL TOLL MULTIPLIERS (WEIGHT FACTORS) -- #
    #toll multipliers
    toll_mult_spec = {
        'ms84': [
            'tmp84',
            f'User Class 1 (SOV VOT1) toll multiplier (minutes/$) - TOD {tod}',
            60 / (factors['vot']['sov1'] * factors['perception']['sov1'])
        ],
        'ms85': [
            'tmp85',
            f'User Class 2 (SOV VOT2) toll multiplier (minutes/$) - TOD {tod}',
            60 / (factors['vot']['sov2'] * factors['perception']['sov2'])
        ],
        'ms86': [
            'tmp86',
            f'User Class 3 (SOV VOT3) toll multiplier (minutes/$) - TOD {tod}',
            60 / (factors['vot']['sov3'] * factors['perception']['sov3'])
        ],
        'ms87': [
            'tmp87',
            f'User Class 4 (HOV) toll multiplier (minutes/$) - TOD {tod}',
            60 / vot_hov23
        ],
        'ms88': [
            'tmp88',
            f'b-plate + light truck toll multiplier (minutes/$) - TOD {tod}',
            60 / vot_bplt_ltrk
        ],  
        'ms89': [
            'tmp89',
            f'medium truck toll multiplier (minutes/$) - TOD {tod}',
            60 / (factors['vot']['mtrk'] * factors['perception']['mtrk'])
        ],
        'ms90': [
            'tmp90',
            f'heavy truck toll multiplier (minutes/$) - TOD {tod}',
            60 / (factors['vot']['htrk'] * factors['perception']['htrk'])
        ]
    }

    for ms, spec in toll_mult_spec.items():
        if eb.matrix(ms):
            eb.delete_matrix(ms)
        eb.create_matrix(ms)
        eb.matrix(ms).name = spec[0]
        eb.matrix(ms).description = spec[1]
        eb.matrix(ms).data = spec[2]


    # -----
    # -- OUTPUT REPORT 
    # -----

    with open(report, 'a') as f:
        f.write(f'\n----- TRIP TABLES REPORT FOR TOD {tod} -----\n')
        f.write(f'\nTOTAL TRIPS:\n')
        for cat, value in report_calcs.items():
            f.write(f'{cat}: {round(value, 2)}\n')
        f.write(f'\nVOT REPORT:\n')
        
        # write trip shares from parameters file (`factors`)
        f.write(f'\nTrip Shares:\n')
        
        period_share_key = {
            'poe_auto': 'External Auto',
            'air_to': 'External Air Passenger To Airports',
            'air_from': 'External Air Passenger From Airports',
            'btrk': 'B-plate Truck',
            'ltrk': 'Light Truck',
            'mtrk': 'Medium Truck',
            'htrk': 'Heavy Truck',
            'poetrk': 'External Heavy Truck'
        }
        for par, name in period_share_key.items():
            f.write(f'{name} = {factors[f"factor_p{tod}"][par]}\n')
        f.write(f'\n\n')
        
        #create and write vot report table 
        # (1st column Trips, 2nd column VOT relative to tolls, 3rd column Toll multiplier)
        index_0th = [
            'SOV low VOT trips', 
            'SOV medium VOT trips', 
            'SOV high VOT trips',
            'HOV2 trips', 
            'HOV3+ trips', 
            'Overall HOV trips',
            'B-Plate Truck trips', 
            'Light Duty Truck trips', 
            'Combined B-Plate + Light trips',
            'Medium Duty Truck trips', 
            'Heavy Duty Truck trips', 
            'External Truck trips'
            ]
        trips_col1 = [
            report_calcs['SOV VOT1 trips'],
            report_calcs['SOV VOT2 trips'],
            report_calcs['SOV VOT3 trips'],
            report_calcs['HOV2 trips'],
            report_calcs['HOV3+ trips'],
            report_calcs['HOV trips (HOV2 + HOV3)'],
            report_calcs['B-plate truck trips'],
            report_calcs['Light truck trips'],
            report_calcs['B-Plate + light truck trips'],
            report_calcs['Medium truck trips'],
            report_calcs['Heavy truck trips (excl. external)'],
            report_calcs['External truck trips']
        ]
        trips_col1 = [round(x, 2) for x in trips_col1]
        
        vot_col2 = [
            factors['vot']['sov1'] * factors['perception']['sov1'],
            factors['vot']['sov2'] * factors['perception']['sov2'],
            factors['vot']['sov3'] * factors['perception']['sov3'],
            factors['vot']['hov2'] * factors['perception']['hov2'],
            factors['vot']['hov3'] * factors['perception']['hov3'],
            vot_hov23,
            factors['vot']['btrk'] * factors['perception']['btrk'],
            factors['vot']['ltrk'] * factors['perception']['ltrk'],
            vot_bplt_ltrk,
            factors['vot']['mtrk'] * factors['perception']['mtrk'],
            factors['vot']['htrk'] * factors['perception']['htrk'],
            factors['vot']['htrk'] * factors['perception']['htrk']
        ]
        vot_col2 = [round(x, 2) for x in vot_col2]
        
        mult_col3 = [
            eb.matrix('ms84').data,
            eb.matrix('ms85').data,
            eb.matrix('ms86').data,
            60 / (factors['vot']['hov2'] * factors['perception']['hov2']),
            60 / (factors['vot']['hov3'] * factors['perception']['hov3']),
            eb.matrix('ms87').data,
            60 / (factors['vot']['btrk'] * factors['perception']['btrk']),
            60 / (factors['vot']['ltrk'] * factors['perception']['ltrk']),
            eb.matrix('ms88').data,
            eb.matrix('ms89').data,
            eb.matrix('ms90').data,
            eb.matrix('ms90').data
        ]
        mult_col3 = [round(x, 2) for x in mult_col3]
        
        out_df = pd.DataFrame([trips_col1, vot_col2, mult_col3]).T
        out_df.columns = ['Trips', 'Perceived VOT ($/hour)', 'Toll Multiplier (minutes/$)']
        out_df.index = index_0th
        f.write(out_df.to_string(header=True, index=True))
        f.write('\n')
        print(f'\t-complete. report/iter{glob_iter}/tod_veh_trips_and_VOT.txt')

# EXECUTE TTABLES
ttables(tod=tod, scen_3dig=scen_3dig, db_dir=db_dir)