''' 
 complete_select_link.py

 Completes a select link analysis by accumulating the time-of-day vehicle class select link
 values (VEQs) into daily totals. Also converts the daily vehicle class totals in VEQs into a
 daily total volume in vehicles (@slvol).

 For a highway RSP it will also accumulate the daily total volume in vehicles of traffic
 from EDAs (@ejauto and @ejtruck).

 Craig Heither, rev. 01-30-2025
 ==========================================================================================
'''
import os
import sys
from pathlib import Path
import inro.modeller as _m
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

currentScen = int(sys.argv[1])
donorScenBase = int(sys.argv[2])
numSelLinkFiles = int(sys.argv[3])
rspFlag = sys.argv[4]

proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
copy_att = my_modeller.tool("inro.emme.data.network.copy_attribute")
del_att = my_modeller.tool("inro.emme.data.extra_attribute.delete_extra_attribute")
my_emmebank = my_modeller.emmebank

## -- Set primary scenario -- ##
change_scenario(scenario=currentScen)

if numSelLinkFiles > 0:
    with _m.logbook_trace("Accumulating TOD vehicle class select link values into daily totals"):
        for j in range(1,numSelLinkFiles+1):
            ## -- Create attribute to hold daily total select link volumes -- ##
            slvol = create_extra(extra_attribute_type="LINK", extra_attribute_name="@slvolp%s" %(j),
                                    extra_attribute_description="select link daily volumes (VEH) Proj%s" %(j), overwrite=True)

        ## -- Create attribute to temporarily hold time-of-day class select link volumes -- ##
        tmpvol = create_extra(extra_attribute_type="LINK",
                                extra_attribute_name="@temp1",
                                extra_attribute_description="temporarily hold class select volumes",
                                overwrite=True)

        ## -- Loop through scenarios to load volumes into daily accumulation scenario -- ##
        for j in range(1,9):
            fromScen = donorScenBase + j        ##-- scenario to import values from
            for k in range(1,numSelLinkFiles+1):    
                from_att = "@slvolp%s" %(k)
                to_att = "@temp1"
                from_scen = my_emmebank.scenario(fromScen)
                copy_att(from_scenario=from_scen,
                        from_attribute_name=from_att,
                        to_attribute_name=to_att,
                        selection={"link":"all"}
                        )

                ## -- Update the select link class volume -- ##
                calcSpec = {
                    "type": "NETWORK_CALCULATION",
                    "result": "@slvolp%s" %(k),
                    "expression": "@slvolp%s + @temp1" %(k),
                    "aggregation": None,
                    "selections": {"link": "all"}
                    }
                report=netcalc(calcSpec, full_report=False)

        ## -- Delete temporary attribute -- ##
        a = my_modeller.scenario.extra_attribute("@temp1")
        del_att(a) 
        print("         Select Link Analysis Completed") 
else:
    _m.logbook_trace("No select link values to accumulate into daily totals")


if rspFlag == "T":
    with _m.logbook_trace("Accumulating EJ auto and truck TOD vehicle class values into daily totals"):
        ## -- Create attributes to hold daily EDA link volumes -- ##
        ejvola = create_extra(extra_attribute_type="LINK",
                            extra_attribute_name="@ejauto",
                            extra_attribute_description="EDA daily total auto volumes (VEH)",
                            overwrite=True)
        ejvolt = create_extra(extra_attribute_type="LINK",
                            extra_attribute_name="@ejtruck",
                            extra_attribute_description="EDA daily total truck volumes (VEH)",
                            overwrite=True)               
        ## -- Create attribute to temporarily hold time-of-day EDA class volumes -- ##
        tmpejv = create_extra(extra_attribute_type="LINK",
                            extra_attribute_name="@temp2",
                            extra_attribute_description="temporarily hold EDA class volumes",
                            overwrite=True)

        ## -- Loop through scenarios to load volumes into daily accumulation scenario -- ##
        for j in range(1,9):
            fromScen = donorScenBase + j        ##-- scenario to import values from
            ## -- Get EDA auto volumes -- ##
            from_att = "@ejauto"
            to_att = "@temp2"
            from_scen = my_emmebank.scenario(fromScen)
            copy_att(from_scenario=from_scen,
                     from_attribute_name=from_att,
                     to_attribute_name=to_att,
                     selection={"link":"all"}
                    )
            ## -- Update the EDA link volume -- ##
            calcSpec2 = {
                "type": "NETWORK_CALCULATION",
                "result": "@ejauto",
                "expression": "@ejauto + @temp2",
                "aggregation": None,
                "selections": {"link": "all"}
                }
            report=netcalc(calcSpec2, full_report=False)

            ## -- Get EDA truck volumes -- ##
            from_att = "@ejtruck"
            to_att = "@temp2"
            from_scen = my_emmebank.scenario(fromScen)
            copy_att(from_scenario=from_scen,
                     from_attribute_name=from_att,
                     to_attribute_name=to_att,
                     selection={"link":"all"}
                    )
            ## -- Update the EDA link volume -- ##
            calcSpec2 = {
                "type": "NETWORK_CALCULATION",
                "result": "@ejtruck",
                "expression": "@ejtruck + @temp2",
                "aggregation": None,
                "selections": {"link": "all"}
                }
            report=netcalc(calcSpec2, full_report=False)

        ## -- Delete temporary attribute -- ##
        a = my_modeller.scenario.extra_attribute("@temp2")
        del_att(a)
        print("         EJ Auto and Truck Values Completed") 
else:
    _m.logbook_trace("No EJ auto and truck values to accumulate into daily totals")
    