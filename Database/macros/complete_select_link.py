''' 
 complete_select_link.py

 Completes a select link analysis by accumulating the time-of-day vehicle class select link
 values (VEQs) into daily totals. Also converts the daily vehicle class totals in VEQs into a
 daily total volume in vehicles (@slvol).

 For a highway RSP it will also accumulate the daily total volume in vehicles of traffic
 from EDAs (@ejvol).

 Craig Heither, 10-05-2023
 ==========================================================================================
'''
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
currentScen = int(sys.argv[2])
donorScenBase = int(sys.argv[3])
numSelLinkFiles = int(sys.argv[4])
rspFlag = sys.argv[5]


directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=True, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

change_scenario = my_modeller.tool("inro.emme.data.scenario.change_primary_scenario")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
copy_att = my_modeller.tool("inro.emme.data.network.copy_attribute")
del_att = my_modeller.tool("inro.emme.data.extra_attribute.delete_extra_attribute")
my_emmebank = my_modeller.emmebank

if rspFlag=="T":
    numSelLinkFiles = 1

## -- Set primary scenario -- ##
change_scenario(scenario=currentScen)

for j in range(1,numSelLinkFiles+1):
    ## -- Create attribute to hold daily total select link volumes -- ##
    slvol = create_extra(extra_attribute_type="LINK", extra_attribute_name="@slvolp%s" %(j),
                            extra_attribute_description="select link daily volumes (VEH) Proj%s" %(j), overwrite=True)

## -- Create attribute to temporarily hold time-of-day class select link volumes -- ##
tmpvol = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@temp1",
                           extra_attribute_description="temporarily hold class select volumes",
                           overwrite=True)

if rspFlag == "T":
    ## -- Create attribute to hold daily EDA link volumes -- ##
    ejvol = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@ejvol",
                           extra_attribute_description="EDA daily total volumes (VEH)",
                           overwrite=True)
    ## -- Create attribute to temporarily hold time-of-day EDA class volumes -- ##
    tmpejv = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@temp2",
                           extra_attribute_description="temporarily hold EDA class volumes",
                           overwrite=True)
    
with _m.logbook_trace("Complete Select Link Analysis"):
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

        if rspFlag == "T":
            ## -- Get EDA volumes -- ##
            from_att = "@ejvol"
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
                "result": "@ejvol",
                "expression": "@ejvol + @temp2",
                "aggregation": None,
                "selections": {"link": "all"}
                }
            report=netcalc(calcSpec2, full_report=False)
            

    ## -- Delete temporary attribute -- ##
    a = my_modeller.scenario.extra_attribute("@temp1")
    del_att(a) 
    if rspFlag == "T":
        a = my_modeller.scenario.extra_attribute("@temp2")
        del_att(a)
    
    print("         Select Link Analysis Completed")  
    