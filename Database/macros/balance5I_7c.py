"""
**********************************************************************
**********************************************************************

  Macro balance5I_7c.py %counter%
   Where %counter% = current global iteration number.

**********************************************************************
  Macro for MSA balance of link volumes and turns in full model
    iteration run.
**********************************************************************

  Written by Englund

    22May2004 (for EMME/2 Release 9.5)
    Revised 1OCT2004 by DBE for five vehicle classes as part of
      fulliter
    Revised 30DEP2009 by DBE for I-290 HOV

    Revised: Heither, 10-30-2014: more compact code
    Revised 02/10/2015 by NRF to change from 6 classes to 7 classes by dividing HOV into HOV2 and HOV3+
    Revised 07/07/2021 by JLemp move to new vehicle class framework (3 SOV classes, combined b-plate + light trucks)
    Revised 09/26/2021 by Heither: update @avauv - replace @vauto with @vsov1+@vsov2+@vsov3
    Revised 08/13/2026 by DWells: convert to python

  7-class version: S is primary class

"""

import os
import sys
from pathlib import Path
import inro.modeller as _m
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

globalIter = int(sys.argv[1])

proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)


netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
delete_ea = my_modeller.tool("inro.emme.data.extra_attribute.delete_extra_attribute")
create_ea = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
copy_scenario = my_modeller.tool("inro.emme.data.scenario.copy_scenario")


# each step of this macro does the same thing for each vehicle type
# structuring all the specifications in one dictionary allows for easy iteration
vehtype_dict = {
    "SOV Total": {
        "vol_ea": "@avauv",
        "vol_label": f"SOV veh MSA average volume iter_{globalIter}",
        "vol_calc": "(@vsov1+@vsov2+@vsov3)",
        "turns_ea": "@avaut",
        "turns_label": f"SOV veh MSA average turns iter_{globalIter}",
        "turns_calc": "@tauto",
    },
    "SOV VOT1": {
        "vol_ea": "@avs1v",
        "vol_label": f"SOV1 veh MSA average volume iter_{globalIter}",
        "vol_calc": "@vsov1",
        "turns_ea": "@avs1t",
        "turns_label": f"SOV1 veh MSA average turns iter_{globalIter}",
        "turns_calc": "@tsov1",
    },
    "SOV VOT2": {
        "vol_ea": "@avs2v",
        "vol_label": f"SOV2 veh MSA average volume iter_{globalIter}",
        "vol_calc": "@vsov2",
        "turns_ea": "@avs2t",
        "turns_label": f"SOV2 veh MSA average turns iter_{globalIter}",
        "turns_calc": "@tsov2",
    },
    "SOV VOT3": {
        "vol_ea": "@avs3v",
        "vol_label": f"SOV3 veh MSA average volume iter_{globalIter}",
        "vol_calc": "@vsov3",
        "turns_ea": "@avs3t",
        "turns_label": f"SOV3 veh MSA average turns iter_{globalIter}",
        "turns_calc": "@tsov3",
    },
    "HOV2": {
        "vol_ea": "@avh2v",
        "vol_label": f"HOV2 veh MSA average volume iter_{globalIter}",
        "vol_calc": "@vhov2",
        "turns_ea": "@avh2t",
        "turns_label": f"HOV2 veh MSA average turns iter_{globalIter}",
        "turns_calc": "@thov2",
    },
    "HOV3": {
        "vol_ea": "@avh3v",
        "vol_label": f"HOV3 veh MSA average volume iter_{globalIter}",
        "vol_calc": "@vhov3",
        "turns_ea": "@avh3t",
        "turns_label": f"HOV3 veh MSA average turns iter_{globalIter}",
        "turns_calc": "@thov3",
    },
    "B-Plate": {
        "vol_ea": "@avbqv",
        "vol_label": f"b-truck veq MSA average volume iter_{globalIter}",
        "vol_calc": "@vbplt",
        "turns_ea": "@avbqt",
        "turns_label": f"b-truck veq MSA average turns iter_{globalIter}",
        "turns_calc": "@tbplt",
    },
    "Light Duty": {
        "vol_ea": "@avlqv",
        "vol_label": f"light trk veq MSA average volume iter_{globalIter}",
        "vol_calc": "@vlght",
        "turns_ea": "@avlqt",
        "turns_label": f"light trk veq MSA average turns iter_{globalIter}",
        "turns_calc": "@tlght",
    },
    "Meduim Duty": {
        "vol_ea": "@avmqv",
        "vol_label": f"medium trk veq MSA average volume iter_{globalIter}",
        "vol_calc": "@vmed",
        "turns_ea": "@avmqt",
        "turns_label": f"medium trk veq MSA average turns iter_{globalIter}",
        "turns_calc": "@tmed",
    },
    "Heavy Duty": {
        "vol_ea": "@avhqv",
        "vol_label": f"heavy trk veq MSA average volume iter_{globalIter}",
        "vol_calc": "@vhevy",
        "turns_ea": "@avhqt",
        "turns_label": f"heavy trk veq MSA average turns iter_{globalIter}",
        "turns_calc": "@thevy",
    }
}

# check for iteration zero - no loads to balance
# initialize average extra attributes as run_0 volume
if globalIter == 0:
    # delete old extra attributes, if necessary
    delete_ea(my_modeller.scenario.extra_attribute("@vauto"))
    for vehtype in vehtype_dict.values():
        delete_ea(my_modeller.scenario.extra_attribute(vehtype["vol_ea"]))
        delete_ea(my_modeller.scenario.extra_attribute(vehtype["turns_ea"]))

    # initialize extra attributes
    for vehtype in vehtype_dict.values():
        create_ea("LINK", vehtype["vol_ea"], vehtype["vol_label"], 0.0, overwrite = True)
        create_ea("TURN", vehtype["turns_ea"], vehtype["turns_label"], 0.0, overwrite = True)

    # store run_0 volumes and turns
    for vehtype in vehtype_dict.values():
        vol_calc_spec = {
            "type": "NETWORK_CALCULATION",
            "result": vehtype["vol_ea"],
            "expression": vehtype["vol_calc"],
            "aggregation": None,
            "selections": {"link": "all"}
        }
        netcalc(vol_calc_spec, full_report = False)
        turns_calc_spec = {
            "type": "NETWORK_CALCULATION",
            "result": vehtype["turns_ea"],
            "expression": vehtype["turns_calc"],
            "aggregation": None,
            "selections": {"incoming_link": "all", "outgoing_link": "all"}
        }
        netcalc(turns_calc_spec, full_report = False)
else:
    # do MSA averaging on volumes and turns
    for vehtype in vehtype_dict.values():
        vol_calc_spec = {
            "type": "NETWORK_CALCULATION",
            "result": vehtype["vol_ea"],
            "expression": f"({vehtype['vol_calc']}/({globalIter}+1))+({vehtype['vol_ea']}*(1-1/({globalIter}+1)))",
            "aggregation": None,
            "selections": {"link": "all"}
        }
        netcalc(vol_calc_spec, full_report = False)
        turns_calc_spec = {
            "type": "NETWORK_CALCULATION",
            "result": vehtype["turns_ea"],
            "expression": f"({vehtype['turns_calc']}/({globalIter}+1))+({vehtype['turns_ea']}*(1-1/({globalIter}+1)))",
            "aggregation": None,
            "selections": {"incoming_link": "all", "outgoing_link": "all"}
        }
        netcalc(turns_calc_spec, full_report = False)

# copy scenario w/ links averaged for next full iteration
if globalIter < 2:
    copy_scenario(
        from_scenario=int(my_modeller.scenario.id),
        scenario_id=int(my_modeller.scenario.id)+10,
        scenario_title=f"{my_modeller.scenario.title[:45]:<45} run_{globalIter+1}"
    )

