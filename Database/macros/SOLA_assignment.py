'''
 SOLA_assignment.py

 Script performs the time-of-day SOLA traffic assignments.
  - During the AM peak and midday periods (3 and 5 respectively) tolled path information is saved for use in the destination choice-mode
    choice utility calculations.
  - During the model final global iteration the time-of-day assignments capture the link volumes for medium and heavy truck trips
    of 200+ miles, used as an input to MOVES.

    
 Save tolled paths for time periods 3 & 5 (mf):
                     AM peak                                   Midday
        Class1   Class2   Class3   Class4   |    Class1   Class2   Class3   Class4 
        ------   ------   ------   ------   |    ------   ------   ------   ------    
         413      423       433     443     |     415      425       435     445             ## -- Analyed demand for work/nonwork trip tolls
         134      135       136     132     |     140      141       142     138             ## -- Work/Nonwork trips on tolled paths
         123      124       125     112     |     126      127       128     118             ## -- Tolls between zone pairs for work/nonwork trips                 
         
    Input to destination choice-mode choice model (mf):
        111     ## -- Toll - SOV low income (HBW)
        114     ## -- Toll - SOV high income (HBW)
        112     ## -- Toll - HOV low income (HBW)
        115     ## -- Toll - HOV high income (HBW)
        117     ## -- Toll - (non-work purposes)
        
    Intermediate matrices (mf):
        113     ## -- hov 3+ per toll amount low_income_HW (TOD 3) 
        116     ## -- hov 3+ per toll amount high_income_HW (TOD 3) 
        117     ## -- sov toll amount HO (TOD 5)  
        118     ## -- hov 2 toll amount HO (TOD 5)
        119     ## -- hov 3+ toll amount HO (TOD 5)
        120     ## -- sov toll amount NH (TOD 5)
        121     ## -- hov 2 toll amount NH (TOD 5)
        122     ## -- hov 3+ toll amount NH (TOD 5)
        123     ## -- sov vot1 toll HW (TOD 3)
        124     ## -- sov vot2 toll HW (TOD 3)
        125     ## -- sov vot3 toll HW (TOD 3)
        126     ## -- sov vot1 toll non-work (TOD 5)
        127     ## -- sov vot2 toll non-work (TOD 5)
        128     ## -- sov vot3 toll non-work (TOD 5)
        131     ## -- sov tolled trips HW (TOD 3)
        137     ## -- sov tolled trips nonwork (TOD 5)

    temporary storage:
        AM peaK: ms23, ms24, ms25             Midday: ms26, ms27, ms28

 -------------------------------------------------------------------------------------------       
 Other options:

 Select link analysis demand storage matrices (mf):
              Proj1   Proj2   Proj3   Proj4   Proj5   |   Temp1   Temp2   Temp3   Temp4   Temp5
              -----   -----   -----   -----   -----   |   -----   -----   -----   -----   -----
    class1      61     601     611     621     631    |    640     647     654     661     668
    class2      62     602     612     622     632    |    641     648     655     662     669
    class3      63     603     613     623     633    |    642     649     656     663     670
    class4      64     604     614     624     634    |    643     650     657     664     671
    class5      65     605     615     625     635    |    644     651     658     665     672
    class6      66     606     616     626     636    |    645     652     659     666     673
    class7      67     607     617     627     637    |    646     653     660     667     674
    Total       60     600     610     620     630    |
    cl1-4 p3    68     608     618     628     638    |
    cl5-7 p3    69     609     619     629     639    |

  
 Regionally Significant Project 
  Time-of-day VOT demand (mf): 
                    TOD1   TOD2   TOD3   TOD4   TOD5   TOD6   TOD7   TOD8 
                    ----   ----   ----   ----   ----   ----   ----   ----
    class1          411    412    413    414    415    416    417    418
    class2          421    422    423    424    425    426    427    428      
    class3          431    432    433    434    435    436    437    438      
    class4 (HOV2)   441    442    443    444    445    446    447    448   
    class4 (HOV3+)  451    452    453    454    455    456    457    458 

  EDA demand used in assignment (mf):
    class1 - 51  
    class2 - 52
    class3 - 53
    class4 - 54
    class5 - 55
    class6 - 56
    class7 - 57    
    
  CCR demand used in assignment (mf):
    class1 - 81
    class2 - 82
    class3 - 83
    class4 - 84
    class5 - 85
    class6 - 86
    class7 - 87    

 Craig Heither, rev. 01-29-2025
## ==========================================================================================
'''
import os
import sys
import json             ##-- improved readability for printing assignment specification
from pathlib import Path
import inro.modeller as _m
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm

tmPeriod = int(sys.argv[1])
sThreads = int(sys.argv[2])
globalIter = int(sys.argv[3])
rspFlag = sys.argv[4]
numSelLinkFiles = int(sys.argv[5])
selLinkFiles = sys.argv[6]
trnAsmtFlag = int(sys.argv[7])
ejFile = sys.argv[8]
ccrFile = sys.argv[9]

proj_dir = Path(__file__).resolve().parents[2]
my_modeller = tbm.connect(proj_dir)

SOLA_spec_report = os.getcwd() + "\\report\\SOLA_spec_GlobalIteration{0}_TOD{1}.txt".format(globalIter, tmPeriod)
matrix_file =  os.getcwd() + "\\rsp_evaluation\\inputs\\" + ejFile      ##-- EDA share origin matrix 
matrix_file_ccr = os.getcwd() + '\\rsp_evaluation\\inputs\\' + ccrFile  ##-- CCR share origin matrix

assign_SOLA = my_modeller.tool("inro.emme.traffic_assignment.sola_traffic_assignment")
create_extra = my_modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
netcalc = my_modeller.tool("inro.emme.network_calculation.network_calculator")
matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")
compute_matrix = my_modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
change_db_dim = my_modeller.tool("inro.emme.data.database.change_database_dimensions")
mo_transact = my_modeller.tool("inro.emme.data.matrix.matrix_transaction")
my_emmebank = my_modeller.emmebank
## ==========================================================================================

## -- Process select link files -- ##
if numSelLinkFiles > 0:
    s = selLinkFiles.split(',')
    sl = [e for e in s if e != 'None']

## -- Set up calculation definitions -- ##
## -- Calculate the select link attribute values to flag tolled paths -- ##
calcSpec = {"type": "NETWORK_CALCULATION", "result": "@tollflag", "expression": "@toll > 0", "selections": {"link": "all"}}
## -- Calculate distance adjustment for long distance truck trips (set POE length to 50 miles) -- ##
calcSpec2 = {"type": "NETWORK_CALCULATION", "result": "@mvlen", "expression": "length", "selections": {"link": "all"}}
calcSpec2b = {"type": "NETWORK_CALCULATION", "result": "@mvlen", "expression": "50", "selections": {"link": "i=3633,3649 or j=3633,3649"}}


## -- Select link storage matrices -- ##
mtx1  = ("mf60", "mf600", "mf610", "mf620", "mf630")   ##-- daily total vehicle demand
mtx2  = ("mf61", "mf601", "mf611", "mf621", "mf631")   ##-- daily mode S VOT1 vehicle demand
mtx3  = ("mf62", "mf602", "mf612", "mf622", "mf632")   ##-- daily mode S VOT2 vehicle demand
mtx4  = ("mf63", "mf603", "mf613", "mf623", "mf633")   ##-- daily mode S VOT3 vehicle demand
mtx5  = ("mf64", "mf604", "mf614", "mf624", "mf634")   ##-- daily mode H vehicle demand
mtx6  = ("mf65", "mf605", "mf615", "mf625", "mf635")   ##-- daily mode b and l truck vehicle demand
mtx7  = ("mf66", "mf606", "mf616", "mf626", "mf636")   ##-- daily mode m truck vehicle demand
mtx8  = ("mf67", "mf607", "mf617", "mf627", "mf637")   ##-- daily mode h truck vehicle demand
mtx9  = ("mf68", "mf608", "mf618", "mf628", "mf638")   ##-- period 3 auto vehicle demand (user classes 1-4)
mtx10 = ("mf69", "mf609", "mf619", "mf629", "mf639")   ##-- period 3 truck vehicle demand (user classes 5-7)
#
mtx11  = ("mf640", "mf647", "mf654", "mf661", "mf668")   ##-- temporary daily mode S VOT1 vehicle demand
mtx12  = ("mf641", "mf648", "mf655", "mf662", "mf669")   ##-- temporary daily mode S VOT2 vehicle demand
mtx13  = ("mf642", "mf649", "mf656", "mf663", "mf670")   ##-- temporary daily mode S VOT3 vehicle demand
mtx14  = ("mf643", "mf650", "mf657", "mf664", "mf671")   ##-- temporary daily mode H vehicle demand
mtx15  = ("mf644", "mf651", "mf658", "mf665", "mf672")   ##-- temporary daily mode b and l truck vehicle demand
mtx16  = ("mf645", "mf652", "mf659", "mf666", "mf673")   ##-- temporary daily mode m truck vehicle demand
mtx17  = ("mf646", "mf653", "mf660", "mf667", "mf674")   ##-- temporary daily mode h truck vehicle demand
storeMtx = ("mtx11[j-1]", "mtx12[j-1]", "mtx13[j-1]", "mtx14[j-1]", "mtx15[j-1]","mtx16[j-1]", "mtx17[j-1]")

if globalIter == 2:
    if numSelLinkFiles > 0: 
        with _m.logbook_trace("Select Link Setup for Time Period %s" % tmPeriod):
            for i in range(1,numSelLinkFiles+1):
                ## -- Create select link flag variable -- ##
                new_att = create_extra(extra_attribute_type="LINK", extra_attribute_name="@sellk%s" %(i),
                               extra_attribute_description="select link flag for Project %s links" %(i), overwrite=True)
                ## -- Calculate the select link attribute values for a select link analysis -- ##
                calcSpecLink = {"type": "NETWORK_CALCULATION", "result": "@sellk%s" %(i), 
                                "expression": "1", "selections": {"link": "~<Select_Link\%s" %(sl[i-1])}}
                report=netcalc(calcSpecLink, full_report=False)
    
            for j in range(1,numSelLinkFiles+1):
                for i in range(1,8):
                    ## -- Create user class select link TOD variables for each select link file -- ##
                    slv1 = create_extra(extra_attribute_type="LINK", extra_attribute_name="@slcl%sp%s" %(i,j),
                                        extra_attribute_description="sel link class %s TOD vols (VEQ) Proj%s" %(i,j), overwrite=True)
                ## -- Create variable to hold select link TOD volume for each select link file -- ##
                slvol = create_extra(extra_attribute_type="LINK", extra_attribute_name="@slvolp%s" %(j),
                                     extra_attribute_description="sel link per %s total vols (VEH) Proj%s" %(tmPeriod,j), overwrite=True)
    
            ## -- Initialize matrices to hold select link demand -- ##
            for i in range(1,numSelLinkFiles+1):
                new_mf1 = matrix_init(matrix_id="%s" %(mtx1[i-1]), matrix_name="sel_link_total_proj%s" %(i),
                                    matrix_description="sel link daily total vehicle demand proj%s" %(i), overwrite=True, default_value=0) 
                new_mf2 = matrix_init(matrix_id="%s" %(mtx2[i-1]), matrix_name="sel_link_sov_vot1_proj%s" %(i),
                                    matrix_description="sel link daily mode S VOT1 vehicle demand proj%s" %(i), overwrite=True, default_value=0) 
                new_mf3 = matrix_init(matrix_id="%s" %(mtx3[i-1]), matrix_name="sel_link_sov_vot2_proj%s" %(i),
                                    matrix_description="sel link daily mode S VOT2 vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf4 = matrix_init(matrix_id="%s" %(mtx4[i-1]), matrix_name="sel_link_sov_vot3_proj%s" %(i),
                                    matrix_description="sel link daily mode S VOT3 vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf5 = matrix_init(matrix_id="%s" %(mtx5[i-1]), matrix_name="sel_link_hov_proj%s" %(i),
                                    matrix_description="sel link daily mode H vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf6 = matrix_init(matrix_id="%s" %(mtx6[i-1]), matrix_name="sel_link_b-light_proj%s" %(i),
                                    matrix_description="sel link daily mode B and L truck vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf7 = matrix_init(matrix_id="%s" %(mtx7[i-1]), matrix_name="sel_link_medium_proj%s" %(i),
                                    matrix_description="sel link daily mode M truck vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf8 = matrix_init(matrix_id="%s" %(mtx8[i-1]), matrix_name="sel_link_heavy_proj%s" %(i),
                                    matrix_description="sel link daily mode H truck vehicle demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf9 = matrix_init(matrix_id="%s" %(mtx9[i-1]), matrix_name="sel_link_auto_TOD3_proj%s" %(i),
                                    matrix_description="sel link period 3 auto vehicle demand (user classes 1-4) proj%s" %(i), overwrite=True, default_value=0)
                new_mf10 = matrix_init(matrix_id="%s" %(mtx10[i-1]), matrix_name="sel_link_truck_TOD3_proj%s" %(i),
                                    matrix_description="sel link period 3 truck vehicle demand (user classes 5-7) proj%s" %(i), overwrite=True, default_value=0)
                new_mf11 = matrix_init(matrix_id="%s" %(mtx11[i-1]), matrix_name="sel_link_class1_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 1 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf12 = matrix_init(matrix_id="%s" %(mtx12[i-1]), matrix_name="sel_link_class2_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 2 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf13 = matrix_init(matrix_id="%s" %(mtx13[i-1]), matrix_name="sel_link_class3_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 3 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf14 = matrix_init(matrix_id="%s" %(mtx14[i-1]), matrix_name="sel_link_class4_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 4 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf15 = matrix_init(matrix_id="%s" %(mtx15[i-1]), matrix_name="sel_link_class5_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 5 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf16 = matrix_init(matrix_id="%s" %(mtx16[i-1]), matrix_name="sel_link_class6_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 6 time period demand proj%s" %(i), overwrite=True, default_value=0)
                new_mf17 = matrix_init(matrix_id="%s" %(mtx17[i-1]), matrix_name="sel_link_class7_demand_proj%s" %(i),
                                    matrix_description="temporary sel link user class 7 time period demand proj%s" %(i), overwrite=True, default_value=0)
            
    ## -- This saves EDA demand if an RSP is being run -- ##
    if rspFlag == "T":
        with _m.logbook_trace("EDA Path Analysis Setup for Time Period %s" % tmPeriod):
            ## -- Import EDA zone share transaction file -- ##
            mo_transact(transaction_file=matrix_file, throw_on_error=True, scenario=my_modeller.scenario)

            ## -- Initialize matrices to hold path analysis EDA demand for RSP -- ## 
            rspMtx  = ("mf51", "mf52", "mf53", "mf54", "mf55", "mf56", "mf57")          ##-- matrices to hold EDA vehicle class demand
            edaMode = ("S VOT1", "S VOT2", "S VOT3", "H", "b+l truck", "m truck", "h truck")
            new_mf51 = matrix_init(matrix_id="%s" %(rspMtx[0]), matrix_name="EDA_sov_vot1",
                        matrix_description="EDA TOD mode %s vehicle demand" %(edaMode[0]), overwrite=True, default_value=0) 
            new_mf52 = matrix_init(matrix_id="%s" %(rspMtx[1]), matrix_name="EDA_sov_vot2",
                        matrix_description="EDA TOD mode %s vehicle demand" %(edaMode[1]), overwrite=True, default_value=0)
            new_mf53 = matrix_init(matrix_id="%s" %(rspMtx[2]), matrix_name="EDA_sov_vot3",
                        matrix_description="EDA TOD mode %s vehicle demand" %(edaMode[2]), overwrite=True, default_value=0)
            new_mf54 = matrix_init(matrix_id="%s" %(rspMtx[3]), matrix_name="EDA_hov",
                        matrix_description="EDA TOD mode %s vehicle demand" %(edaMode[3]), overwrite=True, default_value=0) 
            new_mf55 = matrix_init(matrix_id="%s" %(rspMtx[4]), matrix_name="EDA_bltruck",
                        matrix_description="EDA TOD mode %s vehicle demand" %(edaMode[4]), overwrite=True, default_value=0)
            new_mf56 = matrix_init(matrix_id="%s" %(rspMtx[5]), matrix_name="EDA_mtruck",
                        matrix_description="EDA TOD mode %s VEQ demand" %(edaMode[5]), overwrite=True, default_value=0)
            new_mf57 = matrix_init(matrix_id="%s" %(rspMtx[6]), matrix_name="EDA_htruck",
                        matrix_description="EDA TOD mode %s VEQ demand" %(edaMode[6]), overwrite=True, default_value=0)    

            ## -- Calculate TOD Demand Matrices for EDA share calculations -- ##
            rsp1  = ("mf411", "mf412", "mf413", "mf414", "mf415", "mf416", "mf417", "mf418")   ##-- SOV VOT1 TOD vehicle demand
            rsp2  = ("mf421", "mf422", "mf423", "mf424", "mf425", "mf426", "mf427", "mf428")   ##-- SOV VOT2 TOD vehicle demand
            rsp3  = ("mf431", "mf432", "mf433", "mf434", "mf435", "mf436", "mf437", "mf438")   ##-- SOV VOT3 TOD vehicle demand
            rsp4  = ("mf441+mf451", "mf442+mf452", "mf443+mf453", "mf444+mf454", "mf445+mf455", "mf446+mf456", "mf447+mf457",
                    "mf448+mf458")                                                            ##-- HOV2+3 TOD vehicle demand
            x = tmPeriod - 1
            specMf51 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[0]),
                        "expression": "mo50 * %s" %(rsp1[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf52 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[1]),
                        "expression": "mo50 * %s" %(rsp2[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf53 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[2]),
                        "expression": "mo50 * %s" %(rsp3[x]), "constraint": {"by_zone": None, "by_value": None}}	
            specMf54 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[3]),
                        "expression": "mo50 * (%s)" %(rsp4[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf55 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[4]),
                        "expression": "mo50 * (mf14 + mf15)", "constraint": {"by_zone": None, "by_value": None}}
            specMf56 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[5]),
                        "expression": "mo50 * mf16", "constraint": {"by_zone": None, "by_value": None}}
            specMf57 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx[6]),
                        "expression": "mo50 * mf17", "constraint": {"by_zone": None, "by_value": None}}
            report = compute_matrix([specMf51,specMf52,specMf53,specMf54,specMf55,specMf56,specMf57])	

            ## -- Create variables to hold EDA path analysis volumes -- ##
            ejaut = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ejauto",
                                extra_attribute_description="EDA period %s total auto volumes (VEH)" %(tmPeriod), overwrite=True)
            ejtrk = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ejtruck",
                                extra_attribute_description="EDA period %s total truck volumes (VEH)" %(tmPeriod), overwrite=True)
            for i in range(1,8):
                ejv1 = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ejcl%s" %(i),
                                    extra_attribute_description="EDA class %s TOD volumes (VEQ)" %(edaMode[i-1]), overwrite=True)


    ## -- This saves CCR demand if an RSP is being run -- ##
        with _m.logbook_trace("CCR Path Analysis Setup for Time Period %s" % tmPeriod):
            ## -- Import EDA zone share transaction file -- ##
            mo_transact(transaction_file=matrix_file_ccr, throw_on_error=True, scenario=my_modeller.scenario)

            ## -- Initialize matrices to hold path analysis EDA demand for RSP -- ## 
            rspMtx_ccr  = ("mf81", "mf82", "mf83", "mf84", "mf85", "mf86", "mf87")          ##-- matrices to hold EDA vehicle class demand
            ccrMode = ("S VOT1", "S VOT2", "S VOT3", "H", "b+l truck", "m truck", "h truck")
            new_mf81 = matrix_init(matrix_id="%s" %(rspMtx_ccr[0]), matrix_name="CCR_sov_vot1",
                        matrix_description="CCR TOD mode %s vehicle demand" %(ccrMode[0]), overwrite=True, default_value=0) 
            new_mf82 = matrix_init(matrix_id="%s" %(rspMtx_ccr[1]), matrix_name="CCR_sov_vot2",
                        matrix_description="CCR TOD mode %s vehicle demand" %(ccrMode[1]), overwrite=True, default_value=0)
            new_mf83 = matrix_init(matrix_id="%s" %(rspMtx_ccr[2]), matrix_name="CCR_sov_vot3",
                        matrix_description="CCR TOD mode %s vehicle demand" %(ccrMode[2]), overwrite=True, default_value=0)
            new_mf84 = matrix_init(matrix_id="%s" %(rspMtx_ccr[3]), matrix_name="CCR_hov",
                        matrix_description="CCR TOD mode %s vehicle demand" %(ccrMode[3]), overwrite=True, default_value=0) 
            new_mf85 = matrix_init(matrix_id="%s" %(rspMtx_ccr[4]), matrix_name="CCR_bltruck",
                        matrix_description="CCR TOD mode %s vehicle demand" %(ccrMode[4]), overwrite=True, default_value=0)
            new_mf86 = matrix_init(matrix_id="%s" %(rspMtx_ccr[5]), matrix_name="CCR_mtruck",
                        matrix_description="CCR TOD mode %s VEQ demand" %(ccrMode[5]), overwrite=True, default_value=0)
            new_mf87 = matrix_init(matrix_id="%s" %(rspMtx_ccr[6]), matrix_name="CCR_htruck",
                        matrix_description="CCR TOD mode %s VEQ demand" %(ccrMode[6]), overwrite=True, default_value=0)    

            ## -- Calculate TOD Demand Matrices for CCR share calculations -- ##
            rsp1  = ("mf411", "mf412", "mf413", "mf414", "mf415", "mf416", "mf417", "mf418")   ##-- SOV VOT1 TOD vehicle demand
            rsp2  = ("mf421", "mf422", "mf423", "mf424", "mf425", "mf426", "mf427", "mf428")   ##-- SOV VOT2 TOD vehicle demand
            rsp3  = ("mf431", "mf432", "mf433", "mf434", "mf435", "mf436", "mf437", "mf438")   ##-- SOV VOT3 TOD vehicle demand
            rsp4  = ("mf441+mf451", "mf442+mf452", "mf443+mf453", "mf444+mf454", "mf445+mf455", "mf446+mf456", "mf447+mf457",
                    "mf448+mf458")                                                            ##-- HOV2+3 TOD vehicle demand
            x = tmPeriod - 1
            specMf81 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[0]),
                        "expression": "mo51 * %s" %(rsp1[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf82 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[1]),
                        "expression": "mo51 * %s" %(rsp2[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf83 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[2]),
                        "expression": "mo51 * %s" %(rsp3[x]), "constraint": {"by_zone": None, "by_value": None}}	
            specMf84 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[3]),
                        "expression": "mo51 * (%s)" %(rsp4[x]), "constraint": {"by_zone": None, "by_value": None}}
            specMf85 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[4]),
                        "expression": "mo51 * (mf14 + mf15)", "constraint": {"by_zone": None, "by_value": None}}
            specMf86 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[5]),
                        "expression": "mo51 * mf16", "constraint": {"by_zone": None, "by_value": None}}
            specMf87 = {"type": "MATRIX_CALCULATION", "result": "%s" %(rspMtx_ccr[6]),
                        "expression": "mo51 * mf17", "constraint": {"by_zone": None, "by_value": None}}
            report = compute_matrix([specMf81,specMf82,specMf83,specMf84,specMf85,specMf86,specMf87])	

            ## -- Create variables to hold EDA path analysis volumes -- ##
            ccraut = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ccrauto",
                                extra_attribute_description="CCR period %s total auto volumes (VEH)" %(tmPeriod), overwrite=True)
            ccrtrk = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ccrtruck",
                                extra_attribute_description="CCR period %s total truck volumes (VEH)" %(tmPeriod), overwrite=True)
            for i in range(1,8):
                ccrv1 = create_extra(extra_attribute_type="LINK", extra_attribute_name="@ccrcl%s" %(i),
                                    extra_attribute_description="CCR class %s TOD volumes (VEQ)" %(ccrMode[i-1]), overwrite=True)

## -- get scalar values -- ##
ms84_val = my_emmebank.matrix("ms84").data
print("  - SOV low VOT minutes per dollar: {0:.3f}".format(ms84_val))
ms85_val = my_emmebank.matrix("ms85").data
print("  - SOV medium VOT minutes per dollar: {0:.3f}".format(ms85_val))
ms86_val = my_emmebank.matrix("ms86").data
print("  - SOV high VOT minutes per dollar: {0:.3f}".format(ms86_val))
ms87_val = my_emmebank.matrix("ms87").data
print("  - HOV weighted minutes per dollar: {0:.3f}".format(ms87_val))
ms88_val = my_emmebank.matrix("ms88").data
print("  - B-truck/Light truck weighted minutes per dollar: {0:.3f}".format(ms88_val))
ms89_val = my_emmebank.matrix("ms89").data
print("  - Medium truck minutes per dollar: {0:.3f}".format(ms89_val))
ms90_val = my_emmebank.matrix("ms90").data
print("  - Heavy truck minutes per dollar: {0:.3f}".format(ms90_val))


## ====================================================================================================================================== ##
## ====================================================================================================================================== ##
## === -- Set up path analyses for assignment user classes that will be inserted into the assignment specification -- === ##
### --> 1. Start with the basic path analyses for all assignment options -- ###
class1_path = []; class2_path = []; class3_path = []; class6_path = []; class7_path = []
### -- HOVs (class 4) and b-plate/light trucks (class 5) include extra processing to report vehicle class specific volumes and turns -- ###
class4_path = [
                {"analyzed_demand": "mf92", "results": {"selected_link_volumes": "@vhov2", "selected_turn_volumes": "@thov2"}},
                {"analyzed_demand": "mf93", "results": {"selected_link_volumes": "@vhov3", "selected_turn_volumes": "@thov3"}}
            ]
class5_path = [
                {"analyzed_demand": "mf14", "results": {"selected_link_volumes": "@vbplt", "selected_turn_volumes": "@tbplt"}},
                {"analyzed_demand": "mf15", "results": {"selected_link_volumes": "@vlght", "selected_turn_volumes": "@tlght"}}
            ]

### --> 2. Save tolled paths for periods 3 & 5 for the Destination-Mode Choice utility calculations -- ###
tollPeriods = [3, 5]            ## -- time periods to save toll information
if tmPeriod in tollPeriods:
    if tmPeriod == 3:
        ### -- Save AM Peak toll skim data for Work trips (classes 1-4), which is used by complete_toll_skim_matrices.py -- ###
        toll_dmd = ("mf413","mf423","mf433","mf443")                    ## -- Analyed demand for work trip tolls 
        toll_results = ("mf123","mf124","mf125","mf112")                ## -- Tolls between zone pairs for work trips
        trip_results = ("mf134","mf135","mf136","mf132")                ## -- Work trips on tolled paths 
        temp_ms = ("ms23","ms24","ms25")                                ## -- temporary storage
        toll_store = ("mf131","mf111","mf114","mf113","mf115","mf116" ) ## -- toll results storage
        labels = ("low_income_HW","high_income_HW","HW" )               ## -- matrix name and description labels

    elif tmPeriod == 5:
        ### -- Save Midday toll skim data for NonWork trips (classes 1-4), which is used by complete_toll_skim_matrices.py -- ###
        toll_dmd = ("mf415","mf425","mf435","mf445")                    ## -- Analyed demand for nonwork trip tolls 
        toll_results = ("mf126","mf127","mf128","mf118")                ## -- Tolls between zone pairs for nonwork trips
        trip_results = ("mf140","mf141","mf142","mf138")                ## -- NonWork trips on tolled paths 
        temp_ms = ("ms26", "ms27", "ms28")                              ## -- temporary storage
        toll_store = ("mf137","mf117","mf120","mf119","mf121","mf122" ) ## -- toll results storage
        labels = ("HO","NH","non-work")                                 ## -- matrix name and description labels
    toll = []
    for m in range(4):
        toll.append([
            {## -- Calculate average tolls between O-D pairs: Work trips -- ##
				"link_component": "@toll", "operator": "+", "selection_threshold": {"lower": 0.001, "upper": 9999},
                "path_to_od_composition": {"considered_paths": "ALL", "multiply_path_proportions_by": {"analyzed_demand": False, "path_value": True}},
                "analyzed_demand": toll_dmd[m],
                "results": {"od_values": toll_results[m]}
            },
            {## -- Save demand on tolled paths: Work Trips -- ##
				"link_component": "@tollflag", "operator": ".max.", "selection_threshold": {"lower": 1, "upper": 1},
                "path_to_od_composition": {"considered_paths": "SELECTED", "multiply_path_proportions_by": {"analyzed_demand": True, "path_value": False}},
                "analyzed_demand": toll_dmd[m],
                "results": {"od_values": trip_results[m]}
            }
        ])
    class1_path = class1_path + toll[0]
    class2_path = class2_path + toll[1]
    class3_path = class3_path + toll[2]
    class4_path = class4_path + toll[3]

    ## -- Initialize toll skim matrices -- ##
    new_mf111 = matrix_init(matrix_id="%s" %(toll_store[1]), matrix_name="sov_toll_%s" %(labels[0]), 
                        matrix_description="sov toll %s" %(labels[0]), overwrite=True, default_value=0) 
    new_mf112 = matrix_init(matrix_id="%s" %(toll_results[3]), matrix_name="hov2_toll_%s" %(labels[0]), 
                        matrix_description="hov 2 per toll %s" %(labels[0]), overwrite=True, default_value=0)
    new_mf113 = matrix_init(matrix_id="%s" %(toll_store[3]), matrix_name="hov3+_toll_%s" %(labels[0]), 
                        matrix_description="hov 3+ per toll %s" %(labels[0]), overwrite=True, default_value=0)
    new_mf114 = matrix_init(matrix_id="%s" %(toll_store[2]), matrix_name="sov_toll_%s" %(labels[1]), 
                        matrix_description="sov toll %s" %(labels[1]), overwrite=True, default_value=0)
    new_mf115 = matrix_init(matrix_id="%s" %(toll_store[4]), matrix_name="hov2_toll_%s" %(labels[1]), 
                        matrix_description="hov 2 per toll %s" %(labels[1]), overwrite=True, default_value=0)
    new_mf116 = matrix_init(matrix_id="%s" %(toll_store[5]), matrix_name="hov3+_toll_%s" %(labels[1]), 
                        matrix_description="hov 3+ per toll %s" %(labels[1]), overwrite=True, default_value=0)
    new_mf123 = matrix_init(matrix_id="%s" %(toll_results[0]), matrix_name="sov_VOT1_toll_%s" %(labels[2]), 
                        matrix_description="sov vot1 toll %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf124 = matrix_init(matrix_id="%s" %(toll_results[1]), matrix_name="sov_VOT2_toll_%s" %(labels[2]), 
                        matrix_description="sov vot2 toll %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf125 = matrix_init(matrix_id="%s" %(toll_results[2]), matrix_name="sov_VOT3_toll_%s" %(labels[2]), 
                        matrix_description="sov vot3 toll %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf131 = matrix_init(matrix_id="%s" %(toll_store[0]), matrix_name="tolled_trips_sov_%s" %(labels[2]), 
                        matrix_description="sov tolled trips %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf132 = matrix_init(matrix_id="%s" %(trip_results[3]), matrix_name="tolled_trips_hov_%s" %(labels[2]), 
                        matrix_description="hov tolled trips %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf134 = matrix_init(matrix_id="%s" %(trip_results[0]), matrix_name="tolled_trips_sov_VOT1_%s" %(labels[2]), 
                        matrix_description="sov vot1 tolled trips %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf135 = matrix_init(matrix_id="%s" %(trip_results[1]), matrix_name="tolled_trips_sov_VOT2_%s" %(labels[2]), 
                        matrix_description="sov vot2 tolled trips %s" %(labels[2]), overwrite=True, default_value=0)
    new_mf136 = matrix_init(matrix_id="%s" %(trip_results[2]), matrix_name="tolled_trips_sov_VOT3_%s" %(labels[2]), 
                        matrix_description="sov vot3 tolled trips %s" %(labels[2]), overwrite=True, default_value=0)
    new_ms23 = matrix_init(matrix_id="%s" %(temp_ms[0]), matrix_name="tmptl1", 
                       matrix_description="store SOV VOT1 tolled %s" %(labels[2]), overwrite=True, default_value=0)
    new_ms24 = matrix_init(matrix_id="%s" %(temp_ms[1]), matrix_name="tmptl2", 
                       matrix_description="store SOV VOT2 tolled %s" %(labels[2]), overwrite=True, default_value=0)
    new_ms25 = matrix_init(matrix_id="%s" %(temp_ms[2]), matrix_name="tmptl3", 
                       matrix_description="store SOV VOT3 tolled %s" %(labels[2]), overwrite=True, default_value=0)
    print("         -- Toll skim matrices initialized")  


### --> For the final global iteration:
if globalIter == 2:
    #### --> 3. Save link volumes for medium and heavy trucks traveling 200+ miles (for MOVES) -- ###
    final6 = [
                {## -- Save volau for long distance medium trucks -- ##
					"link_component": "@mvlen", "operator": "+", "selection_threshold": {"lower": 200.001, "upper": 999999},
                    "path_to_od_composition": {"considered_paths": "SELECTED", "multiply_path_proportions_by": {"analyzed_demand": True, "path_value": False}},
                    "analyzed_demand": None, 
                    "results": {"selected_link_volumes": "@m200"}},
            ]
    class6_path = class6_path + final6
    final7 = [
                {## -- Save volau for long distance heavy trucks -- ##
					"link_component": "@mvlen", "operator": "+", "selection_threshold": {"lower": 200.001, "upper": 999999},
                    "path_to_od_composition": {"considered_paths": "SELECTED", "multiply_path_proportions_by": {"analyzed_demand": True, "path_value": False}},
                    "analyzed_demand": None, 
                    "results": {"selected_link_volumes": "@h200"}
                },
            ]
    class7_path = class7_path + final7

    ### --> 4. Save EDA volumes for RSPs
    if rspFlag == "T":
        rspEDA = []
        for i in range(1,8):
            rspEDA.append([
                {## -- EDA user class volumes -- ##
                    "analyzed_demand": "%s" %(rspMtx[i-1]), "results": {"selected_link_volumes": "@ejcl%s" %(i)}}
            ])
        class1_path = class1_path + rspEDA[0]
        class2_path = class2_path + rspEDA[1]
        class3_path = class3_path + rspEDA[2]
        class4_path = class4_path + rspEDA[3]
        class5_path = class5_path + rspEDA[4]
        class6_path = class6_path + rspEDA[5]
        class7_path = class7_path + rspEDA[6]

        ### -- > 4(b). Save CCR volumes for RSPs
        rspCCR = []
        for i in range(1,8):
            rspCCR.append([
                {## -- CCR user class volumes -- ##
                    "analyzed_demand": "%s" %(rspMtx_ccr[i-1]), "results": {"selected_link_volumes": "@ccrcl%s" %(i)}}
            ])
        class1_path = class1_path + rspCCR[0]
        class2_path = class2_path + rspCCR[1]
        class3_path = class3_path + rspCCR[2]
        class4_path = class4_path + rspCCR[3]
        class5_path = class5_path + rspCCR[4]
        class6_path = class6_path + rspCCR[5]
        class7_path = class7_path + rspCCR[6]

    ### --> 5. Save select link volumes and demand for select link analyses
    if numSelLinkFiles > 0:
        selLink = []
        for j in range(1,numSelLinkFiles+1):    ## -- loop through select link projects
            for i in range(1,8):                ## -- loop through user classes
                selLink.append([
                    {## -- Save select link demand for each project -- ##
					        "link_component": "@sellk%s" %(j), "operator": ".max.", "selection_threshold": {"lower": 1, "upper": 1},
                            "path_to_od_composition": {"considered_paths": "SELECTED", "multiply_path_proportions_by": {"analyzed_demand": True, "path_value": False}},
                            "analyzed_demand": None, 
                            "results": {"selected_link_volumes": "@slcl%sp%s" %(i,j), "od_values": "%s" %(eval(storeMtx[i-1]))}
                    }
                ])

        for i in range(0,len(selLink),7):
            class1_path = class1_path + selLink[i]
            class2_path = class2_path + selLink[i+1]
            class3_path = class3_path + selLink[i+2]
            class4_path = class4_path + selLink[i+3]  
            class5_path = class5_path + selLink[i+4] 
            class6_path = class6_path + selLink[i+5] 
            class7_path = class7_path + selLink[i+6]  

## ====================================================================================================================================== ##
## ====================================================================================================================================== ##

solaSpec = {
    "type": "SOLA_TRAFFIC_ASSIGNMENT",
    "classes": [
        {
            ## -- CLASS 1: SOV VOT1 -- ##
            "mode": "S",
            "demand": "mf94",
            "generalized_cost": {"link_costs": "@toll", "perception_factor": ms84_val},
            "results": {"link_volumes": "@vsov1", "turn_volumes": "@tsov1"},
            "path_analyses": class1_path
        },
        {
            ## -- CLASS 2: SOV VOT2 -- ##
            "mode": "S",
            "demand": "mf95",
            "generalized_cost": {"link_costs": "@toll", "perception_factor": ms85_val},
            "results": {"link_volumes": "@vsov2", "turn_volumes": "@tsov2"},
            "path_analyses": class2_path
        },
        {
            ## -- CLASS 3: SOV VOT3 -- ##
            "mode": "S",
            "demand": "mf96",
            "generalized_cost": {"link_costs": "@toll", "perception_factor": ms86_val},
            "results": {"link_volumes": "@vsov3", "turn_volumes": "@tsov3"},
            "path_analyses": class3_path
        },
        {
            ## -- CLASS 4: HOVs -- ##
            "mode": "H",
            "demand": "mf18",
            "generalized_cost": {"link_costs": "@toll", "perception_factor": ms87_val},
            "results": {"link_volumes": None, "turn_volumes": None},
            "path_analyses": class4_path
        },
        {
            ## -- CLASS 5: LIGHT AND B-PLATE TRUCKS -- ##
            "mode": "l",
            "demand": "mf97",
            "generalized_cost": {"link_costs": "@toll2", "perception_factor": ms88_val},
            "results": {"link_volumes": None, "turn_volumes": None},
            "path_analyses": class5_path
        },
        {
            ## -- CLASS 6: MEDIUM TRUCKS -- ##
            "mode": "m",
            "demand": "mf16",
            "generalized_cost": {"link_costs": "@toll3", "perception_factor": ms89_val},
            "results": {"link_volumes": "@vmed", "turn_volumes": "@tmed"},
            "path_analyses": class6_path
        },
        {
            ## -- CLASS 7: HEAVY TRUCKS -- ##
            "mode": "h",
            "demand": "mf17",
            "generalized_cost": {"link_costs": "@toll4", "perception_factor": ms90_val},
            "results": {"link_volumes": "@vhevy", "turn_volumes": "@thevy"},
            "path_analyses": class7_path
        }
    ],
    "performance_settings": {"number_of_processors": sThreads},
    "stopping_criteria": {"max_iterations": 400, "relative_gap": 0.0001, "best_relative_gap": 0.01, "normalized_gap": 0.005}
}
## ====================================================================================================================================== ##
## ====================================================================================================================================== ##

## -- Keep a record of the full assignment specification used -- ##
with open(SOLA_spec_report,'w') as f:
    f.write(json.dumps(solaSpec, indent=4))
 
## -- Perform the traffic assignment -- ##
with _m.logbook_trace("SOLA Traffic Assignment for Time Period %s" % tmPeriod):
    if globalIter == 2:
        #- Create variables for MOVES inputs -#
        create_extra(extra_attribute_type="LINK", extra_attribute_name="@m200", extra_attribute_description="medium truck long distance volau (MOVES)", overwrite=True)
        create_extra(extra_attribute_type="LINK", extra_attribute_name="@h200", extra_attribute_description="heavy truck long distance volau (MOVES)", overwrite=True)
        create_extra(extra_attribute_type="LINK", extra_attribute_name="@mvlen", extra_attribute_description="length - truck long distance (MOVES)", overwrite=True)
        report=netcalc(calcSpec2, full_report=False)
        report=netcalc(calcSpec2b, full_report=False)

    ## -- Create variable for toll skim calculations -- ##
    if tmPeriod in tollPeriods:
        new_att = create_extra(extra_attribute_type="LINK", 
                           extra_attribute_name="@tollflag",
                           extra_attribute_description="select link flag to indicate a toll",
                           overwrite=True)
        report=netcalc(calcSpec, full_report=False)   

    ## -- Run the traffic assignment -- ##
    report = assign_SOLA(solaSpec)

    ## Complete toll skim calculations - the resulting matrices are used in the utility calculations of the Destination Choice-Mode Choice model.
    ## Weighted average toll values are calculated based on the actual trips made by user classes. Final output:
    ##  - mf111: Toll - SOV low income (HBW)
    ##  - mf114: Toll - SOV high income (HBW)
    ##  - mf112: Toll - HOV low income (HBW)
    ##  - mf115: Toll - HOV high income (HBW)
    ##  - mf117: Toll - (non-work purposes)
    if tmPeriod in tollPeriods:
        ## -- Accumulate SOV trips, compute average SOV toll, copy HOV2 toll for HOV3+  -- ##
        specMf1 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[0]), 
                     "expression": "%s + %s + %s" %(trip_results[0], trip_results[1], trip_results[2]), "constraint": {"by_zone": None, "by_value": None}}
        specMs1 = {"type": "MATRIX_CALCULATION", "result": "%s" %(temp_ms[0]), "expression": "%s" %(trip_results[0]), 
                    "constraint": {"by_value": None, "by_zone": None}, "aggregation": {"origins": "+", "destinations": "+"}}
        specMs2 = {"type": "MATRIX_CALCULATION", "result": "%s" %(temp_ms[1]), "expression": "%s" %(trip_results[1]), 
                    "constraint": {"by_value": None, "by_zone": None}, "aggregation": {"origins": "+", "destinations": "+"}}
        specMs3 = {"type": "MATRIX_CALCULATION", "result": "%s" %(temp_ms[2]), "expression": "%s" %(trip_results[2]), 
                    "constraint": {"by_value": None, "by_zone": None}, "aggregation": {"origins": "+", "destinations": "+"}}
        ## -- Convert tolls from dollars to cents -- ##
        specMf2 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[1]),
                    "expression": "(%s*%s + %s*%s + %s*%s)/(%s + %s + %s) * 100" %(toll_results[0],temp_ms[0],toll_results[1],temp_ms[1],toll_results[2],temp_ms[2],temp_ms[0],temp_ms[1],temp_ms[2]),
                    "constraint": {"by_zone": None, "by_value": None}}
        specMf3 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[2]), "expression": "%s" %(toll_store[1]), 
                     "constraint": {"by_zone": None, "by_value": None}}
        specMf4 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_results[3]), "expression": "%s * 100" %(toll_results[3]),
                    "constraint": {"by_zone": None, "by_value": None}}
        specMf5 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[3]), "expression": "%s" %(toll_results[3]),
                    "constraint": {"by_zone": None, "by_value": None}}
        specMf6 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[4]), "expression": "%s" %(toll_results[3]),
                    "constraint": {"by_zone": None, "by_value": None}}
        specMf7 = {"type": "MATRIX_CALCULATION", "result": "%s" %(toll_store[5]), "expression": "%s" %(toll_results[3]),
                    "constraint": {"by_zone": None, "by_value": None}}
        report = compute_matrix([specMf1,specMs1,specMs2,specMs3,specMf1,specMf2,specMf3,specMf4,specMf5,specMf6])
        ## -- Delete Scalar Matrices -- ##
        delete_matrix(matrix=my_emmebank.matrix("%s" %(temp_ms[0]))) 	
        delete_matrix(matrix=my_emmebank.matrix("%s" %(temp_ms[1])))	
        delete_matrix(matrix=my_emmebank.matrix("%s" %(temp_ms[2])))
        print("          -- Toll matrices calculated")

	
    ## -- Complete select link analysis -- ##
    if numSelLinkFiles > 0 and globalIter == 2:
        ## -- Sum TOD select link class volumes (VEQs) into total link volume (vehicles) -- ##
        for j in range(1,numSelLinkFiles+1):
            calcSpecSlvol = {"type": "NETWORK_CALCULATION", "result": "@slvolp%s" %(j),
                "expression": "@slcl1p%s + @slcl2p%s + @slcl3p%s + @slcl4p%s + @slcl5p%s + (@slcl6p%s/2) + (@slcl7p%s/3)" %(j,j,j,j,j,j,j),
	            "aggregation": None, "selections": {"link": "all"}}
            report=netcalc(calcSpecSlvol, full_report=False)

        for j in range(0,numSelLinkFiles):
            ## -- Sum the select link demand (VEQs) into daily vehicles -- ##
            specMf61 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx2[j]), "expression": "%s + %s" %(mtx2[j], mtx11[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}	
            specMf62 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx3[j]), "expression": "%s + %s" %(mtx3[j], mtx12[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}
            specMf63 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx4[j]), "expression": "%s + %s" %(mtx4[j], mtx13[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}	
            specMf64 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx5[j]), "expression": "%s + %s" %(mtx5[j], mtx14[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}	
            specMf65 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx6[j]), "expression": "%s + %s" %(mtx6[j], mtx15[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}	
            specMf66 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx7[j]), "expression": "%s + (%s/2)" %(mtx7[j], mtx16[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}
            specMf67 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx8[j]), "expression": "%s + (%s/3)" %(mtx8[j], mtx17[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}
            specMf60 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx1[j]), 
                        "expression": "%s + %s + %s + %s + %s + %s + (%s/2) + (%s/3)" %(mtx1[j], mtx2[j], mtx3[j], mtx4[j], mtx5[j], mtx6[j], mtx7[j], mtx8[j]), 
                        "constraint": {"by_zone": None, "by_value": None}}
            report = compute_matrix([specMf61,specMf62,specMf63,specMf64,specMf65,specMf66,specMf67,specMf60]) 	
            ## -- Sum AM peak trips: autos and trucks -- ##
            if tmPeriod == 3:
                specMf68 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx9[j]), 
                            "expression": "%s + %s + %s + %s + %s" %(mtx9[j], mtx11[j], mtx12[j], mtx13[j], mtx14[j]), 
                            "constraint": {"by_zone": None, "by_value": None}}
                specMf69 = {"type": "MATRIX_CALCULATION", "result": "%s" %(mtx10[j]),
                            "expression": "%s + %s + (%s/2) + (%s/3)" %(mtx10[j], mtx15[j], mtx16[j], mtx17[j]), 
                            "constraint": {"by_zone": None, "by_value": None}}
                report = compute_matrix([specMf68,specMf69])  

        if tmPeriod == 8:
            ## -- Delete temporary matrices -- ##
            delMtx = []
            for j in range(0,numSelLinkFiles):
                delMtx = delMtx + [mtx11[j]] + [mtx12[j]] + [mtx13[j]] + [mtx14[j]] + [mtx15[j]] + [mtx16[j]] + [mtx17[j]]
            for m in delMtx:
                delete_matrix(matrix=my_emmebank.matrix(m))
        print("          -- Select Link Analysis Completed for Time Period {0}".format(tmPeriod))     

    ## -- Complete EDA demand analysis -- ##
    if rspFlag == "T" and globalIter == 2:	
        ## -- Sum the EDA class volumes -- ##
        calcSpecEjaut = {"type": "NETWORK_CALCULATION", "result": "@ejauto", "expression": "@ejcl1 + @ejcl2 + @ejcl3 + @ejcl4",
                        "aggregation": None, "selections": {"link": "all"}}
        calcSpecEjtrk = {"type": "NETWORK_CALCULATION", "result": "@ejtruck", "expression": "@ejcl5 + (@ejcl6/2) + (@ejcl7/3)",
                         "aggregation": None, "selections": {"link": "all"}}

        report=netcalc([calcSpecEjaut,calcSpecEjtrk], full_report=False)
        print("          -- EDA Demand Analysis Completed for Time Period {0}".format(tmPeriod)) 
        
        ## -- Complete CCR demand analysis -- ##
        calcSpecCCRaut = {"type": "NETWORK_CALCULATION", "result": "@ccrauto", "expression": "@ccrcl1 + @ccrcl2 + @ccrcl3 + @ccrcl4",
                          "aggregation": None, "selections": {"link": "all"}}
        calcSpecCCRtrk = {"type": "NETWORK_CALCULATION", "result": "@ccrtruck", "expression": "@ccrcl5 + (@ccrcl6/2) + (@ccrcl7/3)",
                          "aggregation": None, "selections": {"link": "all"}}

        report=netcalc([calcSpecCCRaut,calcSpecCCRtrk], full_report=False)
        print("          -- CCR Demand Analysis Completed for Time Period {0}".format(tmPeriod)) 

print("         SOLA Assignment Completed for Global Iteration {0} Time Period {1}".format(globalIter, tmPeriod))
