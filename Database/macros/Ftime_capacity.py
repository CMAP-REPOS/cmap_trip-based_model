'''
 Ftime_capacity.py

 Calculates the uncongested travel time and capacities for links.

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

 Node Variable calculated in script:
    -   @napp = number of approaches at node i

 Link Variables Calculated in script:
    -   @ftime = uncongested travel time on link
    -   @avelw = average width of link's driving lanes (equal to @width)
    -   @emcap = lane capacity at level of service E
    -   @tcap = default lane capacity at level of service E
    -   @artfc = arterial functional class used in capacity calculations
                1 = principal
                2 = major
                3 = minor
                4 = collector   


 Craig Heither, 06-18-2024
 ==========================================================================================
'''

def link_capacity():
    import inro.modeller as _m
    create_extra = _m.Modeller().tool("inro.emme.data.extra_attribute.create_extra_attribute")
    netcalc = _m.Modeller().tool("inro.emme.network_calculation.network_calculator")
    del_att = _m.Modeller().tool("inro.emme.data.extra_attribute.delete_extra_attribute")
    
    print('    starting link capacity calculations...')
    ## -- Create extra attibutes -- ##
    link_att = ['@ftime', '@emcap', '@tcap', '@artfc', '@avelw']
    link_att_def = ['freeflow travel time', 'level of service E lane capacity', 'level of service E lane default capacity', 'arterial functional class',
                     'average lane width']
    for att, att_def in zip(link_att, link_att_def):
        create_extra('LINK', att, att_def, 0.0, overwrite = True)
    #
    link_att2 = ['@tmpl1', '@tmpl3', '@tmpl4', '@tmpl6', '@tmpl9']
    link_att_def2 = ['temp link field 1', 'temp link field 3', 'temp link field 4', 'temp link field 6', 'temp link field 9']
    for att2, att_def2 in zip(link_att2, link_att_def2):
        create_extra('LINK', att2, att_def2, 0.0, overwrite = True)
    #
    create_extra('NODE', '@napp', 'number of intersection approaches', 0.0, overwrite = True)
    node_att = ['@tmpi1', '@tmpi2', '@tmpi3', '@tmpi4', '@tmpi5', '@tmpi6', '@tmpi7']
    node_att_def = ['temp node field 1', 'temp node field 2', 'temp node field 3', 'temp node field 4',
                     'temp node field 5', 'temp node field 6', 'temp node field 7']
    for natt, natt_def in zip(node_att, node_att_def):
        create_extra('NODE', natt, natt_def, 0.0, overwrite = True)
    #
    ## --  Estimate average width of driving lanes.  Nine foot parking lanes assumed -- ##
    cSpec1 = {"type": "NETWORK_CALCULATION", "result": "@avelw", "expression": "((15 .min. @width) .max. 9)", "aggregation": None,
              "selections": {"link": "all"}}
    #
    ##***********************************************************************
    ##  Calculations for vdf types 2 (freeways) and 4 (expressways)
    ##***********************************************************************
    #
    ## -- @tmpl1 contains the posted speed -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl1", 
               "expression": "((@speed .le. 40)*40)+((@speed .gt. 40 .and. @speed .le. 45)*45)+((@speed .gt. 45 .and. @speed .le. 50)*50)+((@speed .gt. 50 .and. @speed .le. 55)*55)+((@speed .gt. 55 .and. @speed .le. 60)*60)+((@speed .gt. 60)*65)",
               "aggregation": None, "selections": {"link": "vdf=2 or vdf=4"}}
    ## -- Now calculate freeway/expressway uncongested travel time -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@ftime", 
               "expression": "(length/@tmpl1)*60",
               "aggregation": None, "selections": {"link": "vdf=2 or vdf=4"}}
    ## -- Default lane capacities for freeway/expressway links -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "1500+((@tmpl1 .ge. 50)*100)+((@avelw .ge. 10)*200)+((@avelw .ge. 11)*100)+((@avelw .ge. 12)*100)",
               "aggregation": None, "selections": {"link": "vdf=2 or vdf=4"}}
    ## -- Adjust capacities for freeway/expressway stub links that end at intersections -- ##
    ### --- @tmpi1j contains number of arterial street approaches at j node --- ###
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpi1j", 
               "expression": "(vdf .eq. 1)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "@emcap-((@tmpi1j .gt. 0)*0.2*@emcap)-((@tmpi1j .gt. 1)*0.1*@emcap)-((@tmpi1j .gt. 2)*0.1*@emcap)",
               "aggregation": None, "selections": {"link": "vdf=2 or vdf=4"}}
    report=netcalc([cSpec1, cSpec2, cSpec3, cSpec4, cSpec5, cSpec6], full_report=False)
    #
    ##***********************************************************************
    ##  Calculations for vdf 1 (arterial streets)
    ##***********************************************************************
    ## -- @tmpl1 contains the posted speed -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl1", 
               "expression": "((@speed .le. 15)*15)+((@speed .gt. 15 .and. @speed .le. 20)*20)+((@speed .gt. 20 .and. @speed .le. 25)*25)+((@speed .gt. 25 .and. @speed .le. 30)*30)+((@speed .gt. 30 .and. @speed .le. 35)*35)+((@speed .gt. 35 .and. @speed .le. 40)*40)+((@speed .gt. 40 .and. @speed .le. 45)*45)+((@speed .gt. 45 .and. @speed .le. 50)*50)+((@speed .gt. 50)*55)",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- Now calculate uncongested travel time (does not include intersection delay) -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@ftime", 
               "expression": "(length/@tmpl1)*60",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- calculate the functional class for arterials -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tmpl4", 
               "expression": "(1 + (lanes .lt. 2))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", 
               "expression": "@tmpl4+((lanes .lt. 2)*((@tmpl1 .eq. 50 .and. @atypej .ge. 10 .and. @avelw .lt. 11)+(@tmpl1 .eq. 45 .and. @atypej .eq. 7 .and. @avelw .lt. 11)+(@tmpl1 .eq. 45 .and. @atypej .eq. 8 .and. @avelw .lt. 11)+(@tmpl1 .eq. 45 .and. @atypej .ge. 9)+(@tmpl1 .eq. 40 .and. @atypej .ge. 7)+(@tmpl1 .eq. 35 .and. @atypej .eq. 5 .and. @avelw .lt. 11)+(@tmpl1 .eq. 35 .and. @atypej .eq. 6 .and. @avelw .lt. 11)+(@tmpl1 .eq. 35 .and. @atypej .ge. 7)+(@tmpl1 .eq. 30 .and. @atypej .eq. 3 .and. @avelw .lt. 11)+(@tmpl1 .eq. 30 .and. @atypej .eq. 4 .and. @avelw .lt. 11)+(@tmpl1 .eq. 30 .and. @atypej .ge. 5)+(@tmpl1 .lt. 30 .and. @atypej .eq. 2 .and. @avelw .lt. 11)+(@tmpl1 .lt. 30 .and. @atypej .ge. 3)))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@artfc", 
               "expression": "@tmpl3+((lanes .lt. 2)*((@tmpl1 .eq. 35 .and. @atypej .eq. 7 .and. @avelw .lt. 11)+(@tmpl1 .eq. 35 .and. @atypej .eq. 8 .and. @avelw .lt. 11)+(@tmpl1 .eq. 35 .and. @atypej .ge. 9)+(@tmpl1 .eq. 30 .and. @atypej .ge. 7)+(@tmpl1 .lt. 30 .and. @atypej .ge. 5)))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- determine the number and types of inbound links to a node (j node variable) -- ##
    ### --- @tmpi1 = number of freeway-arterial ramp approaches (vdf=3,8) --- ###
    ### --- @tmpi2 = number of collector arterial approaches (@artfc=4) --- ###
    ### --- @tmpi3 = number of minor arterial approaches (@artfc=3) --- ###
    ### --- @tmpi4 = number of major arterial approaches (@artfc=2) --- ###
    ### --- @tmpi5 = number of principal arterial approaches (@artfc=1) combined with freeway (vdf=2,5,7) and expressway (vdf=4) approaches --- ###
    ### --- @tmpi6 = number of auto access link approaches --- ###
    ### --- @tmpi7 = number of auto access links outbound --- ###
    ### --- @napp = total number of approaches --- ###
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@tmpi1j", 
               "expression": "((vdf .eq. 3) .or. (vdf .eq. 8)) .and. (@atypej .le. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec8 = {"type": "NETWORK_CALCULATION", "result": "@tmpi2j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec9 = {"type": "NETWORK_CALCULATION", "result": "@tmpi3j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 3)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec10 = {"type": "NETWORK_CALCULATION", "result": "@tmpi4j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 2)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec11 = {"type": "NETWORK_CALCULATION", "result": "@tmpi5j", 
               "expression": "(vdf.eq.1 .and. @artfc.eq.1) .or. (vdf.eq.2) .or. (vdf.eq.4) .or. (vdf.eq.5) .or. (vdf.eq.7)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec12 = {"type": "NETWORK_CALCULATION", "result": "@tmpi6j", 
               "expression": "(vdf.eq.6)",
               "aggregation": "+", "selections": {"link": "all"}}
    ## --  auto access links outbound (vdf=6) -- ##
    cSpec13 = {"type": "NETWORK_CALCULATION", "result": "@tmpi7", 
               "expression": "(vdf.eq.6)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec14 = {"type": "NETWORK_CALCULATION", "result": "@napp", 
               "expression": "@tmpi1+@tmpi2+@tmpi3+@tmpi4+@tmpi5",
               "aggregation": None, "selections": {"node": "all"}}
    ## -- Zero out temporary link variables @tmpl3 and @tmpl4 -- ##
    cSpec15 = {"type": "NETWORK_CALCULATION", "result": "@tmpl3", "expression": "0", "aggregation": None, "selections": {"link": "all"}}
    cSpec16 = {"type": "NETWORK_CALCULATION", "result": "@tmpl4", "expression": "0", "aggregation": None, "selections": {"link": "all"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7, cSpec8], full_report=False)
    report=netcalc([cSpec9, cSpec10, cSpec11, cSpec12, cSpec13, cSpec14, cSpec15, cSpec16], full_report=False)
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
    ## --  Arterial lane capacities are stored in @emcap (default capacities are calculated first and stored in @tcap) -- ##
    ### --- The default capacities are estimated assuming that traffic on all links is moving in platoons --- ###
    ### --- dictated by an earlier signalized intersections and are not free flow.  Default capacities are the max permitted --- ###
    ### --- through any signalized intersections. --- ###
    ### ---  Base capacities used in the calculations are: --- ###
    ### ---     - 1800 vph for 12 foot lanes --- ###
    ### ---     - 1750 vph for 11 foot lanes --- ###
    ### ---     - 1710 vph for 10 foot lanes --- ###
    ### ---     - 1620 vph for 9 foot lanes --- ###
    #
    ## -- 12 foot or wider lanes -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tcap", 
               "expression": "((@artfc.eq.1)*1260)+((@artfc.eq.2)*1260)+((@artfc.eq.3)*900)+((@artfc.eq.4)*810)",
               "aggregation": None, "selections": {"link": "vdf=1 and @avelw=11.5,999"}}
    ## -- 11 foot lanes -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@tcap", 
               "expression": "((@artfc.eq.1)*1220)+((@artfc.eq.2)*1220)+((@artfc.eq.3)*870)+((@artfc.eq.4)*790)",
               "aggregation": None, "selections": {"link": "vdf=1 and @avelw=10.5,11.5"}}
    ## -- 10 foot lanes -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tcap", 
               "expression": "((@artfc.eq.1)*1200)+((@artfc.eq.2)*1200)+((@artfc.eq.3)*860)+((@artfc.eq.4)*770)",
               "aggregation": None, "selections": {"link": "vdf=1 and @avelw=9.5,10.5"}}
    ## -- 9 foot or narrower lanes -- ##
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tcap", 
               "expression": "((@artfc.eq.1)*1130)+((@artfc.eq.2)*1130)+((@artfc.eq.3)*810)+((@artfc.eq.4)*730)",
               "aggregation": None, "selections": {"link": "vdf=1 and @avelw=0,9.5"}}
    ## --Calculate capacities considering conflicting approaches and store in @emcap -- ##
    ### --- 12 foot or wider lanes, four or more approaches --- ###
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*720)+((@artfc.eq.1)*(@tmpl3.eq.4)*720)+((@artfc.eq.1)*(@tmpl3.eq.3)*990)+((@artfc.eq.1)*(@tmpl3.eq.2)*1260)+((@artfc.eq.1)*(@tmpl3.eq.1)*900)+((@artfc.eq.2)*(@tmpl3.eq.5)*720)+((@artfc.eq.2)*(@tmpl3.eq.4)*810)+((@artfc.eq.2)*(@tmpl3.eq.3)*990)+((@artfc.eq.2)*(@tmpl3.eq.2)*1260)+((@artfc.eq.2)*(@tmpl3.eq.1)*990)+((@artfc.eq.3)*(@tmpl3.eq.5)*450)+((@artfc.eq.3)*(@tmpl3.eq.4)*450)+((@artfc.eq.3)*(@tmpl3.eq.3)*810)+((@artfc.eq.3)*(@tmpl3.eq.2)*900)+((@artfc.eq.3)*(@tmpl3.eq.1)*540)+((@artfc.eq.4)*(@tmpl3.eq.5)*360)+((@artfc.eq.4)*(@tmpl3.eq.4)*360)+((@artfc.eq.4)*(@tmpl3.eq.3)*630)+((@artfc.eq.4)*(@tmpl3.eq.2)*810)+((@artfc.eq.4)*(@tmpl3.eq.1)*450))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,99 and @avelw=11.5,999"}}
    ### --- 12 foot or wider lanes, three approaches --- ###
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*810)+((@artfc.eq.1)*(@tmpl3.eq.4)*810)+((@artfc.eq.1)*(@tmpl3.eq.3)*1080)+((@artfc.eq.1)*(@tmpl3.eq.2)*1260)+((@artfc.eq.1)*(@tmpl3.eq.1)*990)+((@artfc.eq.2)*(@tmpl3.eq.5)*810)+((@artfc.eq.2)*(@tmpl3.eq.4)*810)+((@artfc.eq.2)*(@tmpl3.eq.3)*1080)+((@artfc.eq.2)*(@tmpl3.eq.2)*1260)+((@artfc.eq.2)*(@tmpl3.eq.1)*990)+((@artfc.eq.3)*(@tmpl3.eq.5)*450)+((@artfc.eq.3)*(@tmpl3.eq.4)*540)+((@artfc.eq.3)*(@tmpl3.eq.3)*810)+((@artfc.eq.3)*(@tmpl3.eq.2)*900)+((@artfc.eq.3)*(@tmpl3.eq.1)*540)+((@artfc.eq.4)*(@tmpl3.eq.5)*360)+((@artfc.eq.4)*(@tmpl3.eq.4)*360)+((@artfc.eq.4)*(@tmpl3.eq.3)*630)+((@artfc.eq.4)*(@tmpl3.eq.2)*810)+((@artfc.eq.4)*(@tmpl3.eq.1)*450))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1 and @avelw=11.5,999"}}
    ### --- 12 foot or wider lanes, two approaches --- ###
    cSpec8 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*900)+((@artfc.eq.1)*(@tmpl3.eq.4)*900)+((@artfc.eq.1)*(@tmpl3.eq.3)*1170)+((@artfc.eq.1)*(@tmpl3.eq.2)*1260)+((@artfc.eq.1)*(@tmpl3.eq.1)*990)+((@artfc.eq.2)*(@tmpl3.eq.5)*900)+((@artfc.eq.2)*(@tmpl3.eq.4)*900)+((@artfc.eq.2)*(@tmpl3.eq.3)*1170)+((@artfc.eq.2)*(@tmpl3.eq.2)*1260)+((@artfc.eq.2)*(@tmpl3.eq.1)*990)+((@artfc.eq.3)*(@tmpl3.eq.5)*540)+((@artfc.eq.3)*(@tmpl3.eq.4)*540)+((@artfc.eq.3)*(@tmpl3.eq.3)*810)+((@artfc.eq.3)*(@tmpl3.eq.2)*900)+((@artfc.eq.3)*(@tmpl3.eq.1)*540)+((@artfc.eq.4)*(@tmpl3.eq.5)*360)+((@artfc.eq.4)*(@tmpl3.eq.4)*360)+((@artfc.eq.4)*(@tmpl3.eq.3)*630)+((@artfc.eq.4)*(@tmpl3.eq.2)*810)+((@artfc.eq.4)*(@tmpl3.eq.1)*450))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1 and @avelw=11.5,999"}}
    ### --- 11 foot lanes, four approaches --- ###
    cSpec9 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*700)+((@artfc.eq.1)*(@tmpl3.eq.4)*700)+((@artfc.eq.1)*(@tmpl3.eq.3)*960)+((@artfc.eq.1)*(@tmpl3.eq.2)*1220)+((@artfc.eq.1)*(@tmpl3.eq.1)*870)+((@artfc.eq.2)*(@tmpl3.eq.5)*700)+((@artfc.eq.2)*(@tmpl3.eq.4)*790)+((@artfc.eq.2)*(@tmpl3.eq.3)*960)+((@artfc.eq.2)*(@tmpl3.eq.2)*1220)+((@artfc.eq.2)*(@tmpl3.eq.1)*960)+((@artfc.eq.3)*(@tmpl3.eq.5)*440)+((@artfc.eq.3)*(@tmpl3.eq.4)*440)+((@artfc.eq.3)*(@tmpl3.eq.3)*790)+((@artfc.eq.3)*(@tmpl3.eq.2)*870)+((@artfc.eq.3)*(@tmpl3.eq.1)*520)+((@artfc.eq.4)*(@tmpl3.eq.5)*350)+((@artfc.eq.4)*(@tmpl3.eq.4)*350)+((@artfc.eq.4)*(@tmpl3.eq.3)*610)+((@artfc.eq.4)*(@tmpl3.eq.2)*790)+((@artfc.eq.4)*(@tmpl3.eq.1)*440))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,99 and @avelw=10.5,11.5"}}
    ### --- 11 foot lanes, three approaches --- ###
    cSpec10 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*790)+((@artfc.eq.1)*(@tmpl3.eq.4)*790)+((@artfc.eq.1)*(@tmpl3.eq.3)*1050)+((@artfc.eq.1)*(@tmpl3.eq.2)*1220)+((@artfc.eq.1)*(@tmpl3.eq.1)*960)+((@artfc.eq.2)*(@tmpl3.eq.5)*790)+((@artfc.eq.2)*(@tmpl3.eq.4)*790)+((@artfc.eq.2)*(@tmpl3.eq.3)*1050)+((@artfc.eq.2)*(@tmpl3.eq.2)*1220)+((@artfc.eq.2)*(@tmpl3.eq.1)*960)+((@artfc.eq.3)*(@tmpl3.eq.5)*440)+((@artfc.eq.3)*(@tmpl3.eq.4)*520)+((@artfc.eq.3)*(@tmpl3.eq.3)*790)+((@artfc.eq.3)*(@tmpl3.eq.2)*870)+((@artfc.eq.3)*(@tmpl3.eq.1)*520)+((@artfc.eq.4)*(@tmpl3.eq.5)*350)+((@artfc.eq.4)*(@tmpl3.eq.4)*350)+((@artfc.eq.4)*(@tmpl3.eq.3)*610)+((@artfc.eq.4)*(@tmpl3.eq.2)*790)+((@artfc.eq.4)*(@tmpl3.eq.1)*440))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1 and @avelw=10.5,11.5"}}
    ### --- 11 foot lanes, two approaches --- ###
    cSpec11 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*870)+((@artfc.eq.1)*(@tmpl3.eq.4)*870)+((@artfc.eq.1)*(@tmpl3.eq.3)*1130)+((@artfc.eq.1)*(@tmpl3.eq.2)*1220)+((@artfc.eq.1)*(@tmpl3.eq.1)*960)+((@artfc.eq.2)*(@tmpl3.eq.5)*870)+((@artfc.eq.2)*(@tmpl3.eq.4)*870)+((@artfc.eq.2)*(@tmpl3.eq.3)*1130)+((@artfc.eq.2)*(@tmpl3.eq.2)*1220)+((@artfc.eq.2)*(@tmpl3.eq.1)*960)+((@artfc.eq.3)*(@tmpl3.eq.5)*520)+((@artfc.eq.3)*(@tmpl3.eq.4)*520)+((@artfc.eq.3)*(@tmpl3.eq.3)*790)+((@artfc.eq.3)*(@tmpl3.eq.2)*870)+((@artfc.eq.3)*(@tmpl3.eq.1)*520)+((@artfc.eq.4)*(@tmpl3.eq.5)*350)+((@artfc.eq.4)*(@tmpl3.eq.4)*350)+((@artfc.eq.4)*(@tmpl3.eq.3)*610)+((@artfc.eq.4)*(@tmpl3.eq.2)*790)+((@artfc.eq.4)*(@tmpl3.eq.1)*440))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1 and @avelw=10.5,11.5"}}
    ### --- 10 foot lanes, four approaches --- ###
    cSpec12 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*680)+((@artfc.eq.1)*(@tmpl3.eq.4)*680)+((@artfc.eq.1)*(@tmpl3.eq.3)*940)+((@artfc.eq.1)*(@tmpl3.eq.2)*1200)+((@artfc.eq.1)*(@tmpl3.eq.1)*860)+((@artfc.eq.2)*(@tmpl3.eq.5)*680)+((@artfc.eq.2)*(@tmpl3.eq.4)*770)+((@artfc.eq.2)*(@tmpl3.eq.3)*940)+((@artfc.eq.2)*(@tmpl3.eq.2)*1200)+((@artfc.eq.2)*(@tmpl3.eq.1)*940)+((@artfc.eq.3)*(@tmpl3.eq.5)*430)+((@artfc.eq.3)*(@tmpl3.eq.4)*430)+((@artfc.eq.3)*(@tmpl3.eq.3)*770)+((@artfc.eq.3)*(@tmpl3.eq.2)*860)+((@artfc.eq.3)*(@tmpl3.eq.1)*510)+((@artfc.eq.4)*(@tmpl3.eq.5)*340)+((@artfc.eq.4)*(@tmpl3.eq.4)*340)+((@artfc.eq.4)*(@tmpl3.eq.3)*600)+((@artfc.eq.4)*(@tmpl3.eq.2)*770)+((@artfc.eq.4)*(@tmpl3.eq.1)*430))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,99 and @avelw=9.5,10.5"}}
    ### --- 10 foot lanes, three approaches --- ###
    cSpec13 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*770)+((@artfc.eq.1)*(@tmpl3.eq.4)*770)+((@artfc.eq.1)*(@tmpl3.eq.3)*1030)+((@artfc.eq.1)*(@tmpl3.eq.2)*1200)+((@artfc.eq.1)*(@tmpl3.eq.1)*940)+((@artfc.eq.2)*(@tmpl3.eq.5)*770)+((@artfc.eq.2)*(@tmpl3.eq.4)*770)+((@artfc.eq.2)*(@tmpl3.eq.3)*1030)+((@artfc.eq.2)*(@tmpl3.eq.2)*1200)+((@artfc.eq.2)*(@tmpl3.eq.1)*940)+((@artfc.eq.3)*(@tmpl3.eq.5)*430)+((@artfc.eq.3)*(@tmpl3.eq.4)*510)+((@artfc.eq.3)*(@tmpl3.eq.3)*770)+((@artfc.eq.3)*(@tmpl3.eq.2)*860)+((@artfc.eq.3)*(@tmpl3.eq.1)*510)+((@artfc.eq.4)*(@tmpl3.eq.5)*340)+((@artfc.eq.4)*(@tmpl3.eq.4)*340)+((@artfc.eq.4)*(@tmpl3.eq.3)*600)+((@artfc.eq.4)*(@tmpl3.eq.2)*770)+((@artfc.eq.4)*(@tmpl3.eq.1)*430))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1 and @avelw=9.5,10.5"}}
    ### --- 10 foot lanes, two approaches --- ###
    cSpec14 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*860)+((@artfc.eq.1)*(@tmpl3.eq.4)*860)+((@artfc.eq.1)*(@tmpl3.eq.3)*1110)+((@artfc.eq.1)*(@tmpl3.eq.2)*1200)+((@artfc.eq.1)*(@tmpl3.eq.1)*940)+((@artfc.eq.2)*(@tmpl3.eq.5)*860)+((@artfc.eq.2)*(@tmpl3.eq.4)*860)+((@artfc.eq.2)*(@tmpl3.eq.3)*1110)+((@artfc.eq.2)*(@tmpl3.eq.2)*1200)+((@artfc.eq.2)*(@tmpl3.eq.1)*940)+((@artfc.eq.3)*(@tmpl3.eq.5)*510)+((@artfc.eq.3)*(@tmpl3.eq.4)*510)+((@artfc.eq.3)*(@tmpl3.eq.3)*770)+((@artfc.eq.3)*(@tmpl3.eq.2)*860)+((@artfc.eq.3)*(@tmpl3.eq.1)*510)+((@artfc.eq.4)*(@tmpl3.eq.5)*340)+((@artfc.eq.4)*(@tmpl3.eq.4)*340)+((@artfc.eq.4)*(@tmpl3.eq.3)*600)+((@artfc.eq.4)*(@tmpl3.eq.2)*770)+((@artfc.eq.4)*(@tmpl3.eq.1)*430))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1 and @avelw=9.5,10.5"}}
    ### --- 9 foot lanes, four approaches --- ###
    cSpec15 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*650)+((@artfc.eq.1)*(@tmpl3.eq.4)*650)+((@artfc.eq.1)*(@tmpl3.eq.3)*890)+((@artfc.eq.1)*(@tmpl3.eq.2)*1130)+((@artfc.eq.1)*(@tmpl3.eq.1)*810)+((@artfc.eq.2)*(@tmpl3.eq.5)*650)+((@artfc.eq.2)*(@tmpl3.eq.4)*730)+((@artfc.eq.2)*(@tmpl3.eq.3)*890)+((@artfc.eq.2)*(@tmpl3.eq.2)*1130)+((@artfc.eq.2)*(@tmpl3.eq.1)*890)+((@artfc.eq.3)*(@tmpl3.eq.5)*410)+((@artfc.eq.3)*(@tmpl3.eq.4)*410)+((@artfc.eq.3)*(@tmpl3.eq.3)*730)+((@artfc.eq.3)*(@tmpl3.eq.2)*810)+((@artfc.eq.3)*(@tmpl3.eq.1)*490)+((@artfc.eq.4)*(@tmpl3.eq.5)*320)+((@artfc.eq.4)*(@tmpl3.eq.4)*320)+((@artfc.eq.4)*(@tmpl3.eq.3)*570)+((@artfc.eq.4)*(@tmpl3.eq.2)*730)+((@artfc.eq.4)*(@tmpl3.eq.1)*410))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=3.9,99 and @avelw=0,9.5"}}
    ### --- 9 foot lanes, three approaches --- ###
    cSpec16 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*730)+((@artfc.eq.1)*(@tmpl3.eq.4)*730)+((@artfc.eq.1)*(@tmpl3.eq.3)*970)+((@artfc.eq.1)*(@tmpl3.eq.2)*1130)+((@artfc.eq.1)*(@tmpl3.eq.1)*890)+((@artfc.eq.2)*(@tmpl3.eq.5)*730)+((@artfc.eq.2)*(@tmpl3.eq.4)*730)+((@artfc.eq.2)*(@tmpl3.eq.3)*970)+((@artfc.eq.2)*(@tmpl3.eq.2)*1130)+((@artfc.eq.2)*(@tmpl3.eq.1)*890)+((@artfc.eq.3)*(@tmpl3.eq.5)*410)+((@artfc.eq.3)*(@tmpl3.eq.4)*490)+((@artfc.eq.3)*(@tmpl3.eq.3)*730)+((@artfc.eq.3)*(@tmpl3.eq.2)*810)+((@artfc.eq.3)*(@tmpl3.eq.1)*490)+((@artfc.eq.4)*(@tmpl3.eq.5)*320)+((@artfc.eq.4)*(@tmpl3.eq.4)*320)+((@artfc.eq.4)*(@tmpl3.eq.3)*570)+((@artfc.eq.4)*(@tmpl3.eq.2)*730)+((@artfc.eq.4)*(@tmpl3.eq.1)*410))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=2.9,3.1 and @avelw=0,9.5"}}
    ### --- 9 foot lanes, two approaches --- ###
    cSpec17 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "(((@artfc.eq.1)*(@tmpl3.eq.5)*810)+((@artfc.eq.1)*(@tmpl3.eq.4)*810)+((@artfc.eq.1)*(@tmpl3.eq.3)*1050)+((@artfc.eq.1)*(@tmpl3.eq.2)*1130)+((@artfc.eq.1)*(@tmpl3.eq.1)*890)+((@artfc.eq.2)*(@tmpl3.eq.5)*810)+((@artfc.eq.2)*(@tmpl3.eq.4)*810)+((@artfc.eq.2)*(@tmpl3.eq.3)*1050)+((@artfc.eq.2)*(@tmpl3.eq.2)*1130)+((@artfc.eq.2)*(@tmpl3.eq.1)*890)+((@artfc.eq.3)*(@tmpl3.eq.5)*490)+((@artfc.eq.3)*(@tmpl3.eq.4)*490)+((@artfc.eq.3)*(@tmpl3.eq.3)*730)+((@artfc.eq.3)*(@tmpl3.eq.2)*810)+((@artfc.eq.3)*(@tmpl3.eq.1)*490)+((@artfc.eq.4)*(@tmpl3.eq.5)*320)+((@artfc.eq.4)*(@tmpl3.eq.4)*320)+((@artfc.eq.4)*(@tmpl3.eq.3)*570)+((@artfc.eq.4)*(@tmpl3.eq.2)*730)+((@artfc.eq.4)*(@tmpl3.eq.1)*410))",
               "aggregation": None, "selections": {"link": "vdf=1 and @nappj=1.9,2.1 and @avelw=0,9.5"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7, cSpec8], full_report=False)
    report=netcalc([cSpec9, cSpec10, cSpec11, cSpec12, cSpec13, cSpec14, cSpec15, cSpec16, cSpec17], full_report=False)
    #
    ## -- Use default capacities if @emcap is zero -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "@emcap+(@emcap.lt.1)*@tcap",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- Parking adjustment -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "@emcap*(lanesr.gt.0)*(((lanes.gt.0.1 .and. lanes.le.1.5)*(@parkl.eq.0)*1.00)+((lanes.gt.0.1 .and. lanes.le.1.5)*(@parkl.gt.0)*0.90)+((lanes.gt.1.5 .and. lanes.le.2.5)*(@parkl.eq.0)*1.00)+((lanes.gt.1.5 .and. lanes.le.2.5)*(@parkl.gt.0)*0.95)+((lanes.gt.2.5)*(@parkl.eq.0)*1.00)+((lanes.gt.2.5)*(@parkl.gt.0)*0.97))+@emcap*(lanesr.eq.0)*(((lanes.gt.0.1 .and. lanes.le.1.5)*(@parkl.eq.0)*1.00)+((lanes.gt.0.1 .and. lanes.le.1.5)*(@parkl.gt.0 .and. @parkl.lt.2)*0.90)+((lanes.gt.0.1 .and. lanes.le.1.5)*(@parkl.ge.2)*0.85)+((lanes.gt.1.5 .and. lanes.le.2.5)*(@parkl.eq.0)*1.00)+((lanes.gt.1.5 .and. lanes.le.2.5)*(@parkl.gt.0 .and. @parkl.lt.2)*0.95)+((lanes.gt.1.5 .and. lanes.le.2.5)*(@parkl.ge.2)*0.90)+((lanes.gt.2.5)*(@parkl.eq.0)*1.00)+((lanes.gt.2.5)*(@parkl.gt.0)*0.97))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- CBD capacity adjustment -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "@emcap*(((@atypej .eq. 1)*0.90)+ (@atypej .ne. 1))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    ## -- Signal interconnect capacity adjustment -- ##
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "@emcap*(((@sigic .eq. 1)*1.15)+(@sigic .ne. 1))",
               "aggregation": None, "selections": {"link": "vdf=1"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5], full_report=False)
    #
    ##***********************************************************************
    ##  Calculations for freeway/expressway to arterial street ramps vdf=3
    ##***********************************************************************
    ## -- @tmpl9 contains new estimate of posted speed -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl9", 
               "expression": "((@speed .le. 15)*15)+((@speed .gt. 15 .and. @speed .le. 20)*20)+((@speed .gt. 20 .and. @speed .le. 25)*25)+((@speed .gt. 25 .and. @speed .le. 30)*30)+((@speed .gt. 30 .and. @speed .le. 35)*35)+((@speed .gt. 35 .and. @speed .le. 40)*40)+((@speed .gt. 40 .and. @speed .le. 45)*45)+((@speed .gt. 45)*50)",
               "aggregation": None, "selections": {"link": "vdf=3"}}
    #
    ## -- Following series of calculations determine the number and types of inbound links to a node (j node variable) -- ##
    ### --- @tmpi1 = number of freeway-arterial ramp approaches (vdf=3,8) --- ###
    ### --- @tmpi2 = number of collector arterial approaches (@artfc=4) --- ###
    ### --- @tmpi3 = number of minor arterial approaches (@artfc=3) --- ###
    ### --- @tmpi4 = number of major arterial approaches (@artfc=2) --- ###
    ### --- @tmpi5 = number of principal arterial approaches (@artfc=1) --- ###
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@tmpi1j", 
               "expression": "(vdf .eq. 3) .or. (vdf .eq. 8)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@tmpi2j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 4)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "@tmpi3j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 3)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "@tmpi4j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 2)",
               "aggregation": "+", "selections": {"link": "all"}}
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@tmpi5j", 
               "expression": "(vdf .eq. 1 .and. @artfc .eq. 1)",
               "aggregation": "+", "selections": {"link": "all"}}
    #
    ## -- Determine whether ramp is a freeway entrance or freeway exit ramp -- ##
    ### ---  @tmpl6 = 1 for exit ramp, 0 for entrance ramp 
    cSpec8 = {"type": "NETWORK_CALCULATION", "result": "@tmpl6", 
               "expression": "0+((@tmpi5j.ge.1).or.(@tmpi4j.ge.1).or.(@tmpi3j.ge.1).or.(@tmpi2j.ge.1))",
               "aggregation":None, "selections": {"link":"vdf=3"}}
    ## -- Calculate freeway/expressway to arterial ramp uncongested time -- ##
    cSpec9 = {"type": "NETWORK_CALCULATION", "result": "@ftime", 
               "expression": "(length/@tmpl9)*60",
               "aggregation":None, "selections": {"link":"vdf=3"}}
    ## -- Calculate freeway/expressway to arterial ramp lane capacities -- ##
    cSpec10 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "((@atypej.gt.4).or.(@tmpl6.eq.0))*(((@tmpl9.le.25)*(lanes.le.1)*1250)+((@tmpl9.le.25)*(lanes.gt.1)*1060)+((@tmpl9.gt.25 .and. @tmpl9.le.35)*(lanes.le.1)*1450)+((@tmpl9.gt.25 .and. @tmpl9.le.35)*(lanes.gt.1)*1300)+((@tmpl9.gt.35)*(lanes.le.1)*1600)+((@tmpl9.gt.35)*(lanes.gt.1)*1500))+((@atypej.le.4).and.(@tmpl6.eq.1))*((((@tmpi5j.ge.1)*620)+((@tmpi5j.eq.0)*(@tmpi4j.ge.1)*620)+((@tmpi5j.eq.0)*(@tmpi4j.eq.0)*(@tmpi3j.ge.1)*890)).min.1040)",
               "aggregation":None, "selections": {"link":"vdf=3"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7, cSpec8, cSpec9, cSpec10], full_report=False)
    #
    ##***********************************************************************
    ##  Calculations for freeway/expressway to freeway/expressway ramps vdf=5
    ##***********************************************************************
    ## -- @tmpl9 contains new estimate of posted speed -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl9", 
               "expression": "((@speed .le. 15)*15)+((@speed .gt. 15 .and. @speed .le. 20)*20)+((@speed .gt. 20 .and. @speed .le. 25)*25)+((@speed .gt. 25 .and. @speed .le. 30)*30)+((@speed .gt. 30 .and. @speed .le. 35)*35)+((@speed .gt. 35 .and. @speed .le. 40)*40)+((@speed .gt. 40 .and. @speed .le. 45)*45)+((@speed .gt. 45)*50)",
               "aggregation": None, "selections": {"link": "vdf=5"}}
    ## -- Calculate freeway/expressway to freeway/expressway ramp uncongested time -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@ftime", 
               "expression": "(length/@tmpl9)*60",
               "aggregation":None, "selections": {"link":"vdf=5"}}
    ## -- Calculate freeway/expressway to freeway/expressway ramp lane capacities -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "((@tmpl9.le.25)*(lanes.le.1)*1250)+((@tmpl9.le.25)*(lanes.gt.1)*1060)+((@tmpl9.gt.25 .and. @tmpl9.le.35)*(lanes.le.1)*1450)+((@tmpl9.gt.25 .and. @tmpl9.le.35)*(lanes.gt.1)*1300)+((@tmpl9.gt.35)*(lanes.le.1)*1600)+((@tmpl9.gt.35)*(lanes.gt.1)*1500)",
               "aggregation":None, "selections": {"link":"vdf=5"}}
    report=netcalc([cSpec2, cSpec3, cSpec4], full_report=False)
    #
    ##***********************************************************************
    ##  Calculations for metered freeway/expressway entrance ramps vdf=8
    ##***********************************************************************
    ## -- @tmpl9 contains new estimate of posted speed -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@tmpl9", 
               "expression": "((@speed .le. 15)*15)+((@speed .gt. 15 .and. @speed .le. 20)*20)+((@speed .gt. 20 .and. @speed .le. 25)*25)+((@speed .gt. 25 .and. @speed .le. 30)*30)+((@speed .gt. 30 .and. @speed .le. 35)*35)+((@speed .gt. 35 .and. @speed .le. 40)*40)+((@speed .gt. 40 .and. @speed .le. 45)*45)+((@speed .gt. 45)*50)",
               "aggregation": None, "selections": {"link": "vdf=8"}}
    ## -- Calculate metered ramp uncongested time (five seconds of delay assumed at meter), flow rate = 720 vehicles per hour -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "@ftime", 
               "expression": "((length/@tmpl9)*60)+0.083",
               "aggregation":None, "selections": {"link":"vdf=8"}}
    ## -- Calculate metered ramp capacities, which equal maximum metered flow rate -- ##
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "@emcap", 
               "expression": "720",
               "aggregation":None, "selections": {"link":"vdf=8"}}
    report=netcalc([cSpec2, cSpec3, cSpec4], full_report=False)
    ## -- End of capacity calculations -- ##
    #
    ## -- Complete @ftime calculations -- ##
    cSpec2 = {"type": "NETWORK_CALCULATION", "result": "@ftime", "expression": "length*3", "aggregation": None, "selections": {"link": "vdf=6"}}
    ## -- Calculate Travel Time for vdf=7 Using Posted Speed From Incoming Link -- ##
    ## -- flag links and use rate from incoming link for toll link -- ##
    cSpec3 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
    cSpec4 = {"type": "NETWORK_CALCULATION", "result": "ui2", "expression": "0", "aggregation": None, "selections": {"node": "all"}}
    cSpec5 = {"type": "NETWORK_CALCULATION", "result": "ui1", "expression": "vdf.eq.7", "aggregation": ".max.", "selections": {"link": "all"}}
    cSpec6 = {"type": "NETWORK_CALCULATION", "result": "uj2", "expression": "@speed", "aggregation": ".max.", "selections": {"link": "all"}}
    cSpec7 = {"type": "NETWORK_CALCULATION", "result": "@ftime", "expression": "length/ui2*60", "aggregation": None, "selections": {"link": "vdf=7"}}
    report=netcalc([cSpec2, cSpec3, cSpec4, cSpec5, cSpec6, cSpec7], full_report=False)

    ## -- Delete temporary attributes -- ##
    for attrb in link_att2:
        del_att(_m.Modeller().scenario.extra_attribute(attrb)) 
    for attrb in node_att:
        del_att(_m.Modeller().scenario.extra_attribute(attrb)) 
    del_att(_m.Modeller().scenario.extra_attribute('@tcap')) 



if __name__ == '__main__':
    link_capacity()