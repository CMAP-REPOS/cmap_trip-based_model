## complete_select_link.py
##
## Completes the select link analysis by accumulating the time-of-day vehicle class select link
## values (VEQs) into daily totals. Also converts the daily vehicle class totals in VEQs into a
## daily total volume in vehicles (@slvol).
##
## Heither rev. 03-18-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
currentScen = int(sys.argv[2])
donorScenBase = int(sys.argv[3])

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



## -- Set primary scenario -- ##
change_scenario(scenario=currentScen)

## -- Create attribute to hold daily total select link volumes -- ##
slvol = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@slvol",
                           extra_attribute_description="select link daily total volumes (VEH)",
                           overwrite=True)

## -- Create attribute to temporarily hold time-of-day class select link volumes -- ##
tmpvol = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@temp1",
                           extra_attribute_description="temporarily hold class select volumes",
                           overwrite=True)
                           
## -- Sum the select link volumes (VEQs) into vehicles -- ##
calcSpecSlvol = {
    "type": "NETWORK_CALCULATION",
	"result": "@slvol",
    "expression": "@slcl1 + @slcl2 + @slcl3 + @slcl4 + @slcl5 + (@slcl6/2) + (@slcl7/3)",
	"aggregation": None,
    "selections": {
        "link": "all"}}
        
with _m.logbook_trace("Complete Select Link Analysis"):
    ## -- Create select link vehicle class variables -- ##
    for i in range(1,8):
        slv1 = create_extra(extra_attribute_type="LINK",
                           extra_attribute_name="@slcl%s" %(i),
                           extra_attribute_description="select link class %s daily volumes (VEQ)" %(i),
                           overwrite=True)

    ## -- Loop through scenarios to load volumes into daily accumulation scenario -- ##
    for j in range(1,9):
        fromScen = donorScenBase + j        ##-- scenario to import values from
        for i in range(1,8):    
            from_att = "@slcl%s" %(i)
            to_att = "@temp1"
            from_scen = my_emmebank.scenario(fromScen)
            copy_att(from_scenario=from_scen,
                     from_attribute_name=from_att,
                     to_attribute_name=to_att,
                     selection={
                         "link":"all"}
                     )

            ## -- Update the select link class volume -- ##
            calcSpec = {
                "type": "NETWORK_CALCULATION",
                "result": "@slcl%s" %(i),
                "expression": "@slcl%s + @temp1" %(i),
                "aggregation": None,
                "selections": {
                    "link": "all"}}
            report=netcalc(calcSpec, full_report=False)
            
    ## -- Sum the select link volumes (VEQs) into vehicles -- ##
    report=netcalc(calcSpecSlvol, full_report=False)

    ## -- Delete temporary attribute -- ##
    a = my_modeller.scenario.extra_attribute("@temp1")
    del_att(a) 
    
    print("         Select Link Analysis Completed")  
    