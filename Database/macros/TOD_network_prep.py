#TOD_network_prep.py
#OLeary, Heither 8/2024: This script was originally an Emme macro, and has since been translated to Python
#Intro notes/description from macro copy-pasted below -- 
#Old mid-script comments from macro are denoted with "#INHERITED FROM MACRO"

# ~/**********************************************************************
# ~/  net5I_7c.mac %1%
# ~/     where %1% is the time period number
# ~/
# ~/  This macro performs extra attribute network processing and loads
# ~/    values into ul? fields
# ~/
# ~/  Taken from Eash by Stryker (with modifications as noted), 12/8/99
# ~/  Modified by DBE for emission calc changes with Mobile6, Dec 2002
# ~/  Modified by DBE to return link capacity calc (stored in ul2) to 
# ~/    original RWE with vdf=1,5 at 75% and vdf=8 at 100%
# ~/  Modified by DBE 21Apr2004 for five class assignment.
# ~/  Modified by DBE 21Apr2004 for five class assignment w/ turns saved
# ~/    in extra attribs.
# ~/  Modified by DBE 22May2004 for two class assignment w/ turns saved
# ~/    in extra attribs.
# ~/  Modified by DBE 1OCT2004 for five class assignment w/ turns as part
# ~/    of fulliter
# ~/ 
# ~#  Heither, 12/03/2010
# ~#    Includes DBE's coding for 6 vehicle classes.
# ~/
# ~#  Heither, 02/07/2013
# ~#    Load last TOD period congested link time procedure is skipped due to path-based asmt.
# ~#
# ~#  Heither, 04-03-2014
# ~#    Run Ftime.Capacity & Arterial.Delay for iter 0 (ensures updated attributes).
# ~#    Compact macro syntax.
# ~#
# ~#  Ferguson, 02/10/2015 - change from 6 classes to 7 classes by dividing HOV into HOV2 and HOV3+
# ~#
# ~#  Heither, 07-15-2016: no loading of tolls into ul3; calculate vdf7 travel time using speed of incoming link.
# ~#         , rev. 07-06-2021: corrected calculation for df7 travel time using speed of incoming link.
# ~#
# ~#  JLemp, 07-07-2021: Update extra link attributes to new vehicle class framework (3 SOV classes + combined b-plate, light trucks)
# ~#
# ~/  7-class version: S is primary class
# ~/

# O'Leary, 8/2024
#     Converted to python, and added @busveq calculation

##################
## SCRIPT SETUP ##
##################
print('--TOD_network_prep.py')
#import libraries
from pathlib import Path
import sys
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm
import Ftime_capacity as fc
import Arterial_delay as ad
import calculate_busveq as bus

#import variables
yr = str(sys.argv[1])  #emme scenario number -- %val% in batch
tod = int(sys.argv[2])   #time of day -- %tod_cntr% in batch
glob_iter = int(sys.argv[3]) #global iteration -- %counter% in batch

# Connect to modeller
proj_dir = Path(__file__).resolve().parents[2]
modeller = tbm.connect(proj_dir)
emmebank = modeller.emmebank

#define necessary emme modeller tools
change_modes = modeller.tool("inro.emme.data.network.base.change_link_modes")
net_calc = modeller.tool("inro.emme.network_calculation.network_calculator")
mat_calc = modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
delete_extra = modeller.tool("inro.emme.data.extra_attribute.delete_extra_attribute")
create_extra = modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
run_macro = modeller.tool("inro.emme.prompt.run_macro")
change_primary_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")
export_transit_lines = modeller.tool("inro.emme.data.network.transit.export_transit_lines")
import_extra_attributes = modeller.tool("inro.emme.data.extra_attribute.import_extra_attributes")

#get desired emmebank scenario number
scen_txt = f'{yr}{glob_iter}{tod}' #e.g., '30013'

##################
## SCRIPT START ##
##################

print('  --clean up extra attributes')
#create scenario as emmebank.scenario object for Modeller API
scen = emmebank.scenario(f'{scen_txt}')
#change to correct scenario
change_primary_scenario(scen)

#clear out extra link and turn attributes
#  -- link attributes to clear
extra_atts_link = [
    ['@vauto' , f'class 1: SOV volume iter_{glob_iter}'],
    ['@vsov1' , f'class 1: SOV VOT1 volume iter_{glob_iter}'],
    ['@vsov2' , f'class 2: SOV VOT2 volume iter_{glob_iter}'],
    ['@vsov3' , f'class 3: SOV VOT3 volume iter_{glob_iter}'],
    ['@vhov2' , f'class 4: HOV2 volume iter_{glob_iter}'],
    ['@vhov3' , f'class 4: HOV3+ volume iter_{glob_iter}'],
    ['@vbplt' , f'class 5: b plate truck volume iter_{glob_iter}'],
    ['@vlght' , f'class 5: light truck volume iter_{glob_iter}'],
    ['@vmed' , f'class 6: medium truck volume iter_{glob_iter}'],
    ['@vhevy' , f'class 7: heavy truck volume iter_{glob_iter}'],
]

#  -- turn attributes to clear
extra_atts_turn = [
    ['@tauto' , f'class 1: SOV volume iter_{glob_iter}'],
    ['@tsov1' , f'class 1: SOV VOT1 volume iter_{glob_iter}'],
    ['@tsov2' , f'class 2: SOV VOT2 volume iter_{glob_iter}'],
    ['@tsov3' , f'class 3: SOV VOT3 volume iter_{glob_iter}'],
    ['@thov2' , f'class 4: HOV2 volume iter_{glob_iter}'],
    ['@thov3' , f'class 4: HOV3+ volume iter_{glob_iter}'],
    ['@tbplt' , f'class 5: b plate truck volume iter_{glob_iter}'],
    ['@tlght' , f'class 5: light truck volume iter_{glob_iter}'],
    ['@tmed' , f'class 6: medium truck volume iter_{glob_iter}'],
    ['@thevy' , f'class 7: heavy truck volume iter_{glob_iter}'],
]

#  -- overwrite link attributes with 0
for att in extra_atts_link:
    create_extra(
        extra_attribute_type = "LINK",
        extra_attribute_name = att[0],
        extra_attribute_description = att[1],
        extra_attribute_default_value = 0.0,
        overwrite = True,
        scenario = scen
    )

#  -- overwrite turn attributes with 0
for att in extra_atts_turn:
    create_extra(
        extra_attribute_type = "TURN",
        extra_attribute_name = att[0],
        extra_attribute_description = att[1],
        extra_attribute_default_value = 0.0,
        overwrite = True,
        scenario = scen
    )

#get hours per tod (but set period 1 == 5 hours)
#format: {<time-of-day> : <number of hours>}

tod_mult_dict = {
    1:5,
    2:1,
    3:2,
    4:1,
    5:4,
    6:2,
    7:2,
    8:2
}

tod_mult = tod_mult_dict[int(tod)] 


#if global iter == 0, call two emme macros -- Ftime.Capacity and Arterial.Delay
# INHERITED FROM MACRO:
# ~# heither, 04-03-2014: run ftime.capacity and arterial.delay for iteration 0
# ~# this ensures link attributes are updated if edits were made and the
# ~# procedures were not run
if int(glob_iter) == 0:
    # print('  --calculating ftime_capacity and arterial_delay')
    fc.link_capacity()              ## -- Calculate link capacity
    ad.arterial_delay()             ## -- Calculate arterial delay
    bus.busveq(tod, Path(modeller.desktop.project_file_name()).name, scen, scen_txt, tod_mult)  ## -- Calculate busveq
    

# INHERITED FROM MACRO
# #~ calculate link capacity and load into ul2
# #~ capacity in bpr volume-delay function is level of service D
# #~    = 0.75*lane capacity/hour*lanes for vdf=1,5
# #~    = lane capacity/hour*lanes for vdf=8
# #~
# #~ this section was modified by axs so that it can be called from tod.mac
# #~
# #~ the capacities for period 1 is factored by 5, all periods are factored
# #~ by the length of the period

#initialize temp link attributes ul1, ul2, ul3
#format: [<name of attribute>, <default value>]
userlinks = [["ul1","0"], ["ul2","1"], ["ul3","0"]]
for ul in userlinks:
    net_calc(
        specification={
            "result":ul[0],
            "expression":ul[1],
            "aggregation": None,
            "selections":{"link":"all"},
            "type":"NETWORK_CALCULATION"
        },
        scenario = scen,
        full_report=True
    )
    
#spec for vdf==1,2,3,4,5
ul2_vdf15_spec = {
    "result":"ul2",
    "expression": f"0.75*@emcap*lanes*{tod_mult}",
    "aggregation":None,
    "selections":{"link":"vdf=1,5"},
    "type":"NETWORK_CALCULATION"
}

#spec for vdf==8 (metered ramps)
ul2_vdf8_spec = {
    "result":"ul2",
    "expression": f"@emcap*lanes*{tod_mult}",
    "aggregation":None,
    "selections":{"link":"vdf=8"},
    "type":"NETWORK_CALCULATION"
}

#calculation
for spec in [ul2_vdf15_spec, ul2_vdf8_spec]:
    net_calc(
        specification=spec,
        scenario=scen,
        full_report=True
    )
    
#INHERITED FROM MACRO
# #~ load cycle length and g/c ratio from @cycle and @gc into ul3
ul3_vdf13_spec = {
    "result":"ul3",
    "expression":"(int(@cyclej*10)*1000)+int(@gc*100)",
    "aggregation":None,
    "selections":{"link":"vdf=1 or vdf=3"},
    "type":"NETWORK_CALCULATION"
}

net_calc(
    specification=ul3_vdf13_spec,
    scenario=scen,
    full_report=True
)


## -- @ftime calculated for all links in Ftime_capacity.py
## -- Load initial free flow times into ul1 -- ##
cSpec3 = {"type": "NETWORK_CALCULATION", "result": "ul1", "expression": "@ftime", "aggregation": None, "selections": {"link": "all"}} 
net_calc(
     specification=cSpec3,
     scenario=scen,
     full_report=True
 )

print('--TOD_network_prep.py complete')