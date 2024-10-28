'''
 transit_triple_indexing.py

 Performs the matrix triple-indexing necessary to identify the least cost transit station used to support drive to transit trips.


 Arguments:  1= name of Emme project file
             2= time period indicator: AM or MD 

 Craig Heither, 03-25-2024
 ==========================================================================================         
'''

import os
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

timePeriod = sys.argv[1]

maxInternal=3632                                    ## -- highest non-POE zone number

autoTimeOpCost=2.00                                 ## -- auto time operating cost per minute
autoDistOpCost=1.00                                 ## -- auto distance operating cost per minute 
autoFixedCost=2.00                                  ## -- auto trip fixed cost
offstreetParkAvailWgt=-1.00                         ## -- off-street parking availability weight
offstreetParkFeeWgt= 1.00                           ## -- off-street parking fee weight	 
parkCostCoeff=-20.00                                ## -- generalized parking cost coefficient
parkOffset= 75                                      ## -- daily parking fee offset in cents (i.e. the cost at which parking may as well be free) 


proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)
my_emmebank = my_modeller.emmebank

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
init_partition = my_modeller.tool("inro.emme.data.zone_partition.init_partition")
change_partition = my_modeller.tool("inro.emme.data.zone_partition.change_partition_description")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
triple_index = my_modeller.tool("inro.emme.matrix_calculation.matrix_triple_index_operation")
###########################################################################################################################

## -- Initialize matrices -- ##
mtx = ("mf819","mf820","mf821","mf834","mo801","mo802","mo803","mo804","mo805","mo602","mf822","mf823","mf824","mf825","mf828","mf829",
       "mf830","mf831","mf832","mf833","mf44","mf45","mf807","mf808")
if timePeriod == 'MD':
    mtx = ("mf919","mf920","mf921","mf934","mo901","mo902","mo903","mo904","mo905","mo602","mf922","mf923","mf924","mf925","mf928","mf929",
           "mf930","mf931","mf932","mf933","mf46","mf47","mf907","mf908")
    
new_mf1 = matrix_init(matrix_id="%s" %(mtx[0]), matrix_name="hwygc_%s" %(timePeriod),
                        matrix_description="congested hwy generalized cost - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf2 = matrix_init(matrix_id="%s" %(mtx[1]), matrix_name="trngc_%s" %(timePeriod),
                        matrix_description="indexed transit generalized cost - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf3 = matrix_init(matrix_id="%s" %(mtx[2]), matrix_name="zone_%s" %(timePeriod),
                        matrix_description="indexed zone number - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf4 = matrix_init(matrix_id="%s" %(mtx[3]), matrix_name="ratio_%s" %(timePeriod),
                        matrix_description="indexed transit/auto only - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo1 = matrix_init(matrix_id="%s" %(mtx[4]), matrix_name="spacesof_%s" %(timePeriod),
                        matrix_description="off-street parking daily spaces - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo2 = matrix_init(matrix_id="%s" %(mtx[5]), matrix_name="centsof_%s" %(timePeriod),
                        matrix_description="off-street parking daily cents - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo3 = matrix_init(matrix_id="%s" %(mtx[6]), matrix_name="spaceson_%s" %(timePeriod),
                        matrix_description="on-street parking daily spaces - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo4 = matrix_init(matrix_id="%s" %(mtx[7]), matrix_name="centson_%s" %(timePeriod),
                        matrix_description="on-street parking daily cents - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo5 = matrix_init(matrix_id="%s" %(mtx[8]), matrix_name="parkgc_%s" %(timePeriod),
                        matrix_description="generalized parking cost - %s" %(timePeriod), overwrite=True, default_value=0)
new_mo6 = matrix_init(matrix_id="%s" %(mtx[9]), matrix_name="flagpr_%s" %(timePeriod),
                        matrix_description="flag Park and Ride zones - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf5 = matrix_init(matrix_id="%s" %(mtx[10]), matrix_name="minivt_%s" %(timePeriod),
                        matrix_description="indexed in-vehicle minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf6 = matrix_init(matrix_id="%s" %(mtx[11]), matrix_name="minxfr_%s" %(timePeriod),
                        matrix_description="indexed walk transfer minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf7 = matrix_init(matrix_id="%s" %(mtx[12]), matrix_name="mintwt_%s" %(timePeriod),
                        matrix_description="indexed total wait minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf8 = matrix_init(matrix_id="%s" %(mtx[13]), matrix_name="minfwt_%s" %(timePeriod),
                        matrix_description="indexed first wait minutes - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf9 = matrix_init(matrix_id="%s" %(mtx[14]), matrix_name="ifare_%s" %(timePeriod),
                        matrix_description="indexed final average fare - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf10 = matrix_init(matrix_id="%s" %(mtx[15]), matrix_name="ifmode_%s" %(timePeriod),
                        matrix_description="indexed first mode - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf11 = matrix_init(matrix_id="%s" %(mtx[16]), matrix_name="ipmode_%s" %(timePeriod),
                        matrix_description="indexed priority mode - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf12 = matrix_init(matrix_id="%s" %(mtx[17]), matrix_name="ilmode_%s" %(timePeriod),
                        matrix_description="indexed last mode - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf13 = matrix_init(matrix_id="%s" %(mtx[18]), matrix_name="min$_%s" %(timePeriod),
                        matrix_description="indexed auto generalized cost - %s" %(timePeriod), overwrite=True, default_value=0)
new_mf14 = matrix_init(matrix_id="%s" %(mtx[19]), matrix_name="minut_%s" %(timePeriod),
                        matrix_description="indexed auto minutes to transit - %s" %(timePeriod), overwrite=True, default_value=0)


#==============================================================================
## -- INDEX AUTO ACCESS AND PARKING
#==============================================================================
## -- Calculate parking generalized costs -- ##
## -- Convolute (index) auto, transit, mode with parking cost matrix -- ##
## -- Extract transit and auto legs -- ##

## -- Calculate auto acces generalized cost -- ##
autoGC = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[0]), 
        "expression": "(%s*%s) + (%s*%s) + %s" %(autoTimeOpCost, mtx[20], autoDistOpCost, mtx[21], autoFixedCost),
        "constraint": {"by_zone": None, "by_value": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(autoGC) 
## -- Calculate Park and Ride generalized cost -- ##
### --- Off-street availability: move RAIL node parking spaces attribute back to zone value on origin matrix --- ###
calcUl3 = {"type": "NETWORK_CALCULATION", "result": "ul3", "expression": "0", "aggregation": None, "selections": {"link": "all"}}
calcUi1 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
calcUl3_2 = {"type": "NETWORK_CALCULATION", "result": "ul3", "expression": "@pspacj*(j.ge.30000 .and. j.le.49999)", "aggregation": None, "selections": {"link": "mod=uvw"}}
calcUi1_2 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "ul3", "aggregation": "+", "selections": {"link": "i=1,%s" %(maxInternal)}}
### --- For the matrix convolution this needs to flag all rail stations - not just those with parking [05-06-2022] --- ###
calcUi1_3 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "ui1 + (i.ge.30000 .and. i.le.49999)", 
             "aggregation": None, "selections": {"node": "i=1,%s" %(maxInternal)}}
report=netcalc([calcUl3, calcUi1, calcUl3_2, calcUi1_2, calcUi1_3], full_report=False)
mo1Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[4]), "expression": "ui1",
        "constraint": {"by_value": None, "by_zone": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(mo1Spec) 
### --- Create Parking Lot Zone Group --- ###
mo2Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[9]), "expression": "%s.gt.0" %(mtx[4]),
        "constraint": {"by_value": None, "by_zone": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(mo2Spec) 
### --- Initialize partition gr --- ###
init_partition(partition=my_emmebank.partition("gr")) 
change_partition(partition=my_emmebank.partition("gr"), partition_description="Park and Ride Zones")
pnrSpec = {
        "type": "MATRIX_CALCULATION", "result": "gr", "expression": "%s" %(mtx[9]),
        "constraint": {"by_zone": None, "by_value": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(pnrSpec) 
### --- Off-street fee  --- ###
### --- move node parking fee attribute back to zone value on origin matrix, weighted cost calculated by: cost*cap/cap --- ###
calcUi2 = {"type": "NETWORK_CALCULATION", "result": "ui2", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
calcUl3_2 = {"type": "NETWORK_CALCULATION", "result": "ul3", "expression": "@pcostj*@pspacj", "aggregation": None, "selections": {"link": "mod=uvw"}}
calcUi2_2 = {"type": "NETWORK_CALCULATION", "result": "ui2", "expression": "ul3/ui1", "aggregation": "+", "selections": {"link": "ui1=1,100000"}}
report=netcalc([calcUl3, calcUi2, calcUl3_2, calcUi2_2], full_report=False)
mo3Spec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[5]), "expression": "ui2",
        "constraint": {"by_value": None, "by_zone": None},
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(mo3Spec) 
### --- Parking generalized cost --- ###
parkCostSpec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[8]), 
        "expression": "%s/(((%s*%s)/(%s*%s.max.%s))+(0))" %(parkCostCoeff, offstreetParkAvailWgt, mtx[4], offstreetParkFeeWgt, mtx[5], parkOffset),
        "constraint": {
                "by_value": {"interval_min": 0, "interval_max": 0, "condition": "EXCLUDE", "od_values": "%s" %(mtx[4])},
                "by_zone": {"origins": None, "destinations": None}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(parkCostSpec) 
## -- Matrix convolution (i.e., triple indexing) -- ##
tripleSpec = {
            "type": "MATRIX_TRIPLE_INDEX_OPERATION",
            "pk_operand": "%s" %(mtx[0]),
            "kq_operand": "%s" %(mtx[22]),
            "qk_operand": None,
            "combination_operator": "+",
            "masks": [
                {"operator": "+", "pq_operand": None, "pk_operand": None, "kq_operand": None, "constant_operand": None, "k_operand": "%s" %(mtx[8])}
            ],
            "contraction_operator": ".min.",
            "result": "%s" %(mtx[1]),
            "index_result": "%s" %(mtx[2]),
            "constraint": {
                "by_zone": {"origins": "1,%s" %(maxInternal), "intermediates": "gr1", "destinations": "1,%s" %(maxInternal)},
                "by_value": None
            },  
        }
report = triple_index(tripleSpec)

## -- Adjust kzone matrix - Keep the original zone if: the trip is intrazonal or if there is skimmed transit service available (mf8) and service -- ##
## -- originates within the City of Chicago. Otherwise, apply the indexed station zone. (mf8 is unindexed transit in-vehicle time)  [07-16-2022] -- ##
finIndexSpec = {
        "type": "MATRIX_CALCULATION", "result": "%s" %(mtx[2]), 
        "expression": "((%s.lt.1000 .and. ga(p).eq.1).or.(p.eq.q))*p + ((%s.ge.1000 .or. ga(p).ne.1).and.(p.ne.q))*%s" %(mtx[23], mtx[23], mtx[2]),
        "constraint": {
                "by_value": None,
                "by_zone": {"origins": "1,%s" %(maxInternal), "destinations": "1,%s" %(maxInternal)}
            },
        "aggregation": {"origins": None, "destinations": None},
        }
report = compute_matrix(finIndexSpec) 

print(' -- {0} transit triple indexing done'.format(timePeriod))