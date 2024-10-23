'''
 Arterial_delay.py

 Calculates the intersection characteristics needed to calculate arterial and expressway-to-arterial intersection delays.
 Cycle length and green to cycle ratios are estimated at nodes that are likely to have signalized intersections.
 Ftime_capacity.py should precede this script.

 Node Variables required at start of script:
    -   @zone = modeling zone within which node i is located
    -   @atypej = area type within which node i is located
            1 = inside Chicago CBD 
            2 = remainder of Chicago Central Area 
            3 = remainder of City of Chicago 
            4 = inside inner ring suburbs where the Chicago major and minor arterial street grid is continued
            5 = remainder of Illinois portion of Chicago Urbanized Area
            6 = Indiana portion of Chicago Urbanized Area
            7 = other Urbanized Areas and Urban Clusters within the MPO Metropolitan Planning
                Area plus other Urbanized Areas in northeastern Illinois
            8 = other Urbanized Areas and Urban Clusters in northwestern Indiana
            9 = remainder of MPO Metropolitan Planning Area
           10 = remainder of Lake Co., IN (rural)
           11 = external area  
           99 = points of entry - not defined in Capacity Zone system

 Link Variables required at start of script:
    -   modes:  A = Generalized auto
                S = Single occupant auto
                H = High occupancy vehicle
                T = General truck
                b = B plate truck
                l = Light truck
                m = Medium truck
                h = heavy truck
    -   lanes = number of driving lanes
    -   vdf:    1 = arterial street
                2 = freeway
                3 = freeway/expressway to arterial street
                4 = expressway
                5 = freeway/expressway to freeway/expressway ramp
                6 = auto access to network
                7 = link where toll is paid
                8 = metered expressway entrance ramp
    -   link type = 1 = default
    -   @speed = link free speed copied from CATS network
    -   @parkl = number of parking lanes on link copied from CMAP network
    -   @width = one-way average lane width in feet
    -   @avelw = average width of link's driving lanes 
    -   @artfc = arterial functional class used in capacity calculations
                1 = principal
                2 = major
                3 = minor
                4 = collector


 Node Variable calculated in script:
    -   @cycle = cycle length at node i intersection

 Link Variable Calculated in script:
    -   @gc = green to cycle ratio for link 
              

 Craig Heither, 06-19-2024
 ==========================================================================================
'''

def arterial_delay():
    import inro.modeller as _m
    create_extra = _m.Modeller().tool("inro.emme.data.extra_attribute.create_extra_attribute")
    netcalc = _m.Modeller().tool("inro.emme.network_calculation.network_calculator")
    del_att = _m.Modeller().tool("inro.emme.data.extra_attribute.delete_extra_attribute")
    
    print('    starting arterial delay calculations...')
    ## -- Create extra attibutes -- ##
    create_extra('LINK', '@gc', 'green to cycle ratio on link', 0.0, overwrite = True)
    create_extra('LINK', '@tmpl3', 'temp link field 3', 0.0, overwrite = True)
    create_extra('LINK', '@tmpl6', 'temp link field 6', 0.0, overwrite = True)
    create_extra('NODE', '@cycle', 'cycle length at node in minutes', 0.0, overwrite = True)
    node_att = ['@tmpi1', '@tmpi2', '@tmpi3', '@tmpi4', '@tmpi5', '@tmpi6', '@tmpi7']
    node_att_def = ['temp node field 1', 'temp node field 2', 'temp node field 3', 'temp node field 4',
                     'temp node field 5', 'temp node field 6', 'temp node field 7']
    for natt, natt_def in zip(node_att, node_att_def):
        create_extra('NODE', natt, natt_def, 0.0, overwrite = True)

    ##***********************************************************************
    ##  Calculations for vdf 1 (arterial streets)
    ##***********************************************************************
    ## -- determine the number and types of inbound links to a node (j node variable) -- ##
    ### --- @tmpi1 = number of freeway-arterial ramp approaches (vdf=3,8) --- ###
    ### --- @tmpi2 = number of collector arterial approaches (@artfc=4) --- ###
    ### --- @tmpi3 = number of minor arterial approaches (@artfc=3) --- ###
    ### --- @tmpi4 = number of major arterial approaches (@artfc=2) --- ###
    ### --- @tmpi5 = number of principal arterial approaches (@artfc=1) combined with freeway (vdf=2,5,7) and expressway (vdf=4) approaches --- ###
    ### --- @tmpi6 = number of auto access link approaches --- ###
    ### --- @tmpi7 = number of auto access links outbound --- ###
    ### --- @napp = total number of approaches --- ###  
    cSpec1 = {"type": "NETWORK_CALCULATION", "result": "@tmpi1j", 
               "expression": "((vdf .eq. 3) .or. (vdf .eq. 8)) .and. (@atypej .le. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpi2j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@tmpi3j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 3)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tmpi4j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 2)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpi5j", 
               "expression": "(vdf.eq.1 .and. @artfc.eq.1) .or. (vdf.eq.2) .or. (vdf.eq.4) .or. (vdf.eq.5) .or. (vdf.eq.7)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@tmpi6j", 
               "expression": "(vdf.eq.6)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@tmpi7", 
               "expression": "(vdf.eq.6)",
               "aggregation": "+", "selections": {"link": "all"}}
    report=netcalc([cSpec1, cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7], full_report=False)
    #
    ## -- Determine correct opposing approach, stored in @tmpl3 -- ##
    ### --- 1 = ramp
    ### --- 2 = collector
    ### --- 3 = minor 
    ### --- 4 = major
    ### --- 5 = principal
    #
    ## -- Four or more approaches, from principal arterial link (@artfc=1) -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "(@artfc .eq. 1)*(((((@tmpi5j.eq.1)*(@tmpi4j.ge.1))*(((@tmpi4j.ge.2)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.ge.1))*(((@tmpi3j.ge.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi5j.ge.2)*(((@tmpi5j.gt.2)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,999"}}
    ## -- Four or more approaches, from major arterial link (@artfc=2) -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 2)*(((((@tmpi4j.eq.1)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.ge.1))*(((@tmpi3j.ge.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi4j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.gt.2)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,999"}}
    ## -- Four or more approaches, from minor arterial link (@artfc=3) -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 3)*(((((@tmpi3j.eq.1)*(@tmpi4j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.2)*4).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.eq.0)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.eq.0)*(@tmpi5j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi3j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.gt.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,999"}}
    ## -- Four or more approaches, from collector arterial link (@artfc=4) -- ##
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 4)*(((((@tmpi2j.eq.1)*(@tmpi3j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.2)*3).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.2)*4).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.eq.0)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.eq.0)*(@tmpi5j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi2j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.gt.2)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,999"}}
    ## -- Three approaches, from principal arterial link (@artfc=1) -- ##
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "(@artfc .eq. 1)*(((((@tmpi5j.eq.1)*(@tmpi4j.ge.1))*(((@tmpi4j.ge.2)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.ge.1))*(((@tmpi3j.ge.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi5j.eq.1)*(@tmpi4j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi5j.ge.2)*(((@tmpi5j.gt.2)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1"}}
    ## -- Three approaches, from major arterial link (@artfc=2) -- ##
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 2)*(((((@tmpi4j.eq.1)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.ge.1))*(((@tmpi3j.ge.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi4j.eq.1)*(@tmpi5j.eq.0)*(@tmpi3j.eq.0)*(@tmpi2j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi4j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.gt.2)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1"}}
    ## -- Three approaches, from minor arterial link (@artfc=3) -- ##
    cSpec8 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 3)*(((((@tmpi3j.eq.1)*(@tmpi4j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.2)*4).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi2j.ge.2)*2).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.eq.0)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.(@tmpi1j.ge.1)))+(((@tmpi3j.eq.1)*(@tmpi4j.eq.0)*(@tmpi2j.eq.0)*(@tmpi5j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi3j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.gt.2)*3).max.((@tmpi2j.ge.1)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1"}}
    ## -- Three approaches, from collector arterial link (@artfc=4) -- ##
    cSpec9 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq. 4)*(((((@tmpi2j.eq.1)*(@tmpi3j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.2)*3).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.ge.1))*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.2)*4).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.eq.0)*(@tmpi5j.ge.1))*(((@tmpi5j.ge.2)*5).max.(@tmpi1j.ge.1)))+(((@tmpi2j.eq.1)*(@tmpi3j.eq.0)*(@tmpi4j.eq.0)*(@tmpi5j.eq.0)*(@tmpi1j.ge.1))*(@tmpi1j.ge.2)))+((@tmpi2j.ge.2)*(((@tmpi5j.ge.1)*5).max.((@tmpi4j.ge.1)*4).max.((@tmpi3j.ge.1)*3).max.((@tmpi2j.gt.2)*2).max.(@tmpi1j.ge.1))))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1"}}
    ## -- Two approaches, from principal arterial link (@artfc=1) -- ##
    cSpec10 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "(@artfc .eq. 1)*((@tmpi5j.ge.1)*(((@tmpi5j.eq.2)*5).max.((@tmpi4j.eq.1)*4).max.((@tmpi3j.eq.1)*3).max.((@tmpi2j.eq.1)*2).max.(@tmpi1j.eq.1)))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    ## -- Two approaches, from major arterial link (@artfc=2) -- ##
    cSpec11 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq.2)*((@tmpi4j.ge.1)*(((@tmpi5j.eq.1)*5).max.((@tmpi4j.eq.2)*4).max.((@tmpi3j.eq.1)*3).max.((@tmpi2j.eq.1)*2).max.(@tmpi1j.eq.1)))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    ## -- Two approaches, from minor arterial link (@artfc=3) -- ##
    cSpec12 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq.3)*((@tmpi3j.ge.1)*(((@tmpi5j.eq.1)*5).max.((@tmpi4j.eq.1)*4).max.((@tmpi3j.eq.2)*3).max.((@tmpi2j.eq.1)*2).max.(@tmpi1j.eq.1)))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    ## -- Two approaches, from collector arterial link (@artfc=4)) -- ##
    cSpec13 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3+(@artfc .eq.4)*((@tmpi2j.ge.1)*(((@tmpi5j.eq.1)*5).max.((@tmpi4j.eq.1)*4).max.((@tmpi3j.eq.1)*3).max.((@tmpi2j.eq.2)*2).max.(@tmpi1j.eq.1)))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    ## -- Zero out @tmpl3 for two approaches when link is broken only for auto access -- ##
    cSpec14 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl3*(@tmpi6j.eq.0)*(@tmpi7j.eq.0)",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7, cSpec8], full_report=False)
    report=netcalc([cSpec9, cSpec10, cSpec11, cSpec12, cSpec13, cSpec14], full_report=False)
    #
    ## -- Calculate green to cycle ratios -- ##
    ### --- Four approaches --- ###
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@gc", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*0.40)+((@artfc.eq.1)*(@tmpl3.eq.4)*0.40)+((@artfc.eq.1)*(@tmpl3.eq.3)*0.55)+((@artfc.eq.1)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.1)*(@tmpl3.eq.1)*0.50)+((@artfc.eq.2)*(@tmpl3.eq.5)*0.40)+((@artfc.eq.2)*(@tmpl3.eq.4)*0.45)+((@artfc.eq.2)*(@tmpl3.eq.3)*0.55)+((@artfc.eq.2)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.2)*(@tmpl3.eq.1)*0.55)+((@artfc.eq.3)*(@tmpl3.eq.5)*0.25)+((@artfc.eq.3)*(@tmpl3.eq.4)*0.25)+((@artfc.eq.3)*(@tmpl3.eq.3)*0.45)+((@artfc.eq.3)*(@tmpl3.eq.2)*0.50)+((@artfc.eq.3)*(@tmpl3.eq.1)*0.30)+((@artfc.eq.4)*(@tmpl3.eq.5)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.4)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.3)*0.35)+((@artfc.eq.4)*(@tmpl3.eq.2)*0.45)+((@artfc.eq.4)*(@tmpl3.eq.1)*0.25))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,99"}}
    ### --- Three approaches --- ###
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@gc", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*0.45)+((@artfc.eq.1)*(@tmpl3.eq.4)*0.45)+((@artfc.eq.1)*(@tmpl3.eq.3)*0.60)+((@artfc.eq.1)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.1)*(@tmpl3.eq.1)*0.55)+((@artfc.eq.2)*(@tmpl3.eq.5)*0.45)+((@artfc.eq.2)*(@tmpl3.eq.4)*0.45)+((@artfc.eq.2)*(@tmpl3.eq.3)*0.60)+((@artfc.eq.2)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.2)*(@tmpl3.eq.1)*0.55)+((@artfc.eq.3)*(@tmpl3.eq.5)*0.25)+((@artfc.eq.3)*(@tmpl3.eq.4)*0.30)+((@artfc.eq.3)*(@tmpl3.eq.3)*0.45)+((@artfc.eq.3)*(@tmpl3.eq.2)*0.50)+((@artfc.eq.3)*(@tmpl3.eq.1)*0.30)+((@artfc.eq.4)*(@tmpl3.eq.5)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.4)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.3)*0.35)+((@artfc.eq.4)*(@tmpl3.eq.2)*0.45)+((@artfc.eq.4)*(@tmpl3.eq.1)*0.25))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1"}}
    ### --- Two approaches --- ###
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@gc", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*0.50)+((@artfc.eq.1)*(@tmpl3.eq.4)*0.50)+((@artfc.eq.1)*(@tmpl3.eq.3)*0.65)+((@artfc.eq.1)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.1)*(@tmpl3.eq.1)*0.55)+((@artfc.eq.2)*(@tmpl3.eq.5)*0.50)+((@artfc.eq.2)*(@tmpl3.eq.4)*0.50)+((@artfc.eq.2)*(@tmpl3.eq.3)*0.65)+((@artfc.eq.2)*(@tmpl3.eq.2)*0.70)+((@artfc.eq.2)*(@tmpl3.eq.1)*0.55)+((@artfc.eq.3)*(@tmpl3.eq.5)*0.30)+((@artfc.eq.3)*(@tmpl3.eq.4)*0.30)+((@artfc.eq.3)*(@tmpl3.eq.3)*0.45)+((@artfc.eq.3)*(@tmpl3.eq.2)*0.50)+((@artfc.eq.3)*(@tmpl3.eq.1)*0.30)+((@artfc.eq.4)*(@tmpl3.eq.5)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.4)*0.20)+((@artfc.eq.4)*(@tmpl3.eq.3)*0.35)+((@artfc.eq.4)*(@tmpl3.eq.2)*0.45)+((@artfc.eq.4)*(@tmpl3.eq.1)*0.25))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1"}}
    ## -- Cycle lengths (minutes) are stored in @cycle -- ##
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@cyclej", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*2.0)+((@artfc.eq.1)*(@tmpl3.eq.4)*2.0)+((@artfc.eq.1)*(@tmpl3.eq.3)*1.5)+((@artfc.eq.1)*(@tmpl3.eq.2)*1.0)+((@artfc.eq.1)*(@tmpl3.eq.1)*1.5)+((@artfc.eq.2)*(@tmpl3.eq.5)*2.0)+((@artfc.eq.2)*(@tmpl3.eq.4)*2.0)+((@artfc.eq.2)*(@tmpl3.eq.3)*1.5)+((@artfc.eq.2)*(@tmpl3.eq.2)*1.0)+((@artfc.eq.2)*(@tmpl3.eq.1)*1.5)+((@artfc.eq.3)*(@tmpl3.eq.5)*1.5)+((@artfc.eq.3)*(@tmpl3.eq.4)*1.5)+((@artfc.eq.3)*(@tmpl3.eq.3)*1.5)+((@artfc.eq.3)*(@tmpl3.eq.2)*1.0)+((@artfc.eq.3)*(@tmpl3.eq.1)*1.0)+((@artfc.eq.4)*(@tmpl3.eq.5)*1.0)+((@artfc.eq.4)*(@tmpl3.eq.4)*1.0)+((@artfc.eq.4)*(@tmpl3.eq.3)*1.0)+((@artfc.eq.4)*(@tmpl3.eq.2)*1.0)+((@artfc.eq.4)*(@tmpl3.eq.1)*1.0))",
               "aggregation": ".max.", "selections": {"link": "vdf=1 and @nappj=1.5,99 and i=5001,999999 and j=5001,999999"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5], full_report=False)
    #
    ##***********************************************************************
    ##  Calculations for freeway/expressway to arterial street ramps vdf=3
    ##***********************************************************************
    #
    ## -- Following series of calculations determine the number and types of inbound links to a node (j node variable) -- ##
    ### --- @tmpi2 = number of collector arterial approaches (@artfc=4) --- ###
    ### --- @tmpi3 = number of minor arterial approaches (@artfc=3) --- ###
    ### --- @tmpi4 = number of major arterial approaches (@artfc=2) --- ###
    ### --- @tmpi5 = number of principal arterial approaches (@artfc=1) --- ###
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpi2j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@tmpi3j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 3)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tmpi4j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 2)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpi5j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 1)",
               "aggregation": "+", "selections": {"link": "all"}}
    ## -- Determine whether ramp is a freeway entrance or freeway exit ramp -- ##
    ### ---  @tmpl6 = 1 for exit ramp, 0 for entrance ramp 
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@tmpl6", 
               "expression": "0+((@tmpi5j.ge.1).or.(@tmpi4j.ge.1).or.(@tmpi3j.ge.1).or.(@tmpi2j.ge.1))",
               "aggregation":None, "selections": {"link":"vdf=3"}}
    #
    ## -- Calculate freeway/expressway to arterial green to cycle ratio -- ##
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@gc", 
               "expression": "((@atypej.le.4).and.(@tmpl6.eq.1))*((((@tmpi5j.ge.1)*0.35)+((@tmpi5j.eq.0)*(@tmpi4j.ge.1)*0.35)+((@tmpi5j.eq.0)*(@tmpi4j.eq.0)*(@tmpi3j.ge.1)*0.50)).min.0.50)",
               "aggregation": None, "selections": {"link": "vdf=3"}}
    ## -- Calculate freeway/expressway to arterial cycle lengths -- ##
    cSpec8 = {"type": "NETWORK_CALCULATION", "result": "@cyclej", 
               "expression": "((@atypej.le.4).and.(@tmpl6.eq.1))*(1.0 + ((@tmpi5j.ge.1 .or. @tmpi4j.ge.1)*0.50))",
               "aggregation": ".max.", "selections": {"link": "vdf=3 and @cyclej=0"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7, cSpec8], full_report=False)
    ## -- End of arterial delay calculations -- ##

    ## -- Delete temporary attributes -- ##
    for attrb in node_att:
        del_att(_m.Modeller().scenario.extra_attribute(attrb)) 
    del_att(_m.Modeller().scenario.extra_attribute('@tmpl3'))
    del_att(_m.Modeller().scenario.extra_attribute('@tmpl6'))


if __name__ == '__main__':
    arterial_delay()
