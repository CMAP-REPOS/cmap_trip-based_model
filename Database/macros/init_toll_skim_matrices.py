## init_toll_skim_matrices.py
##
## Initialize matrices for toll skims during time period 3 & 5 (used in complete_toll_skim_matrices.py).
##
## Heither rev. 02-21-2023
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
tod = int(sys.argv[2])
directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")


if tod == 3:
    new_mf111 = matrix_init(matrix_id="mf111",
                            matrix_name="sov_toll_low_income_HW",
                            matrix_description="low inc h-w sov toll",
                            overwrite=True,
                            default_value=0) 
                            
    new_mf112 = matrix_init(matrix_id="mf112",
                            matrix_name="hov2_toll_low_income_HW",
                            matrix_description="low inc h-w hov 2 per toll",
                            overwrite=True,
                            default_value=0)						
                            
    new_mf113 = matrix_init(matrix_id="mf113",
                            matrix_name="hov3+_toll_low_income_HW",
                            matrix_description="low inc h-w hov 3+ per toll",
                            overwrite=True,
                            default_value=0)

    new_mf114 = matrix_init(matrix_id="mf114",
                            matrix_name="sov_toll_high_income_HW",
                            matrix_description="high inc h-w sov toll",
                            overwrite=True,
                            default_value=0)

    new_mf115 = matrix_init(matrix_id="mf115",
                            matrix_name="hov2_toll_high_income_HW",
                            matrix_description="high inc h-w hov 2 per toll",
                            overwrite=True,
                            default_value=0)

    new_mf116 = matrix_init(matrix_id="mf116",
                            matrix_name="hov3+_toll_high_income_HW",
                            matrix_description="high inc h-w hov 3+ per toll",
                            overwrite=True,
                            default_value=0)
                            
    new_mf123 = matrix_init(matrix_id="mf123",
                            matrix_name="sov_VOT1_toll_HW",
                            matrix_description="h-w sov vot1 toll",
                            overwrite=True,
                            default_value=0)

    new_mf124 = matrix_init(matrix_id="mf124",
                            matrix_name="sov_VOT2_toll_HW",
                            matrix_description="h-w sov vot2 toll",
                            overwrite=True,
                            default_value=0)

    new_mf125 = matrix_init(matrix_id="mf125",
                            matrix_name="sov_VOT3_toll_HW",
                            matrix_description="h-w sov vot3 toll",
                            overwrite=True,
                            default_value=0)

    new_mf131 = matrix_init(matrix_id="mf131",
                            matrix_name="tolled_trips_sov_HW",
                            matrix_description="h-w sov tolled trips",
                            overwrite=True,
                            default_value=0)

    new_mf132 = matrix_init(matrix_id="mf132",
                            matrix_name="tolled_trips_hov_HW",
                            matrix_description="h-w hov tolled trips",
                            overwrite=True,
                            default_value=0)                            

    new_mf134 = matrix_init(matrix_id="mf134",
                            matrix_name="tolled_trips_sov_VOT1_HW",
                            matrix_description="h-w sov vot1 tolled trips",
                            overwrite=True,
                            default_value=0)

    new_mf135 = matrix_init(matrix_id="mf135",
                            matrix_name="tolled_trips_sov_VOT2_HW",
                            matrix_description="h-w sov vot2 tolled trips",
                            overwrite=True,
                            default_value=0)	

    new_mf136 = matrix_init(matrix_id="mf136",
                            matrix_name="tolled_trips_sov_VOT3_HW",
                            matrix_description="h-w sov vot3 tolled trips",
                            overwrite=True,
                            default_value=0)

    new_ms23 = matrix_init(matrix_id="ms23",
                            matrix_name="tmp23",
                            matrix_description="store h-w SOV VOT1 tolled",
                            overwrite=True,
                            default_value=0)

    new_ms24 = matrix_init(matrix_id="ms24",
                            matrix_name="tmp24",
                            matrix_description="store h-w SOV VOT2 tolled",
                            overwrite=True,
                            default_value=0)

    new_ms25 = matrix_init(matrix_id="ms25",
                            matrix_name="tmp25",
                            matrix_description="store h-w SOV VOT3 tolled",
                            overwrite=True,
                            default_value=0)
else:
    new_mf117 = matrix_init(matrix_id="mf117",
                            matrix_name="sov_toll_HO",
                            matrix_description="h-o sov toll",
                            overwrite=True,
                            default_value=0)

    new_mf118 = matrix_init(matrix_id="mf118",
                            matrix_name="hov2_toll_HO",
                            matrix_description="h-o hov 2 per toll",
                            overwrite=True,
                            default_value=0)

    new_mf119 = matrix_init(matrix_id="mf119",
                            matrix_name="hov3+_toll_HO",
                            matrix_description="h-o hov 3+ per toll",
                            overwrite=True,
                            default_value=0)

    new_mf120 = matrix_init(matrix_id="mf120",
                            matrix_name="sov_toll_NH",
                            matrix_description="nh sov toll",
                            overwrite=True,
                            default_value=0)

    new_mf121 = matrix_init(matrix_id="mf121",
                            matrix_name="hov2_toll_NH",
                            matrix_description="nh hov 2 per toll",
                            overwrite=True,
                            default_value=0)

    new_mf122 = matrix_init(matrix_id="mf122",
                            matrix_name="hov3+_toll_NH",
                            matrix_description="nh hov 3+ per toll",
                            overwrite=True,
                            default_value=0)

    new_mf126 = matrix_init(matrix_id="mf126",
                            matrix_name="sov_VOT1_toll_non-work",
                            matrix_description="non-work sov vot1 toll",
                            overwrite=True,
                            default_value=0)

    new_mf127 = matrix_init(matrix_id="mf127",
                            matrix_name="sov_VOT2_toll_non-work",
                            matrix_description="non-work sov vot2 toll",
                            overwrite=True,
                            default_value=0)

    new_mf128 = matrix_init(matrix_id="mf128",
                            matrix_name="sov_VOT3_toll_non-work",
                            matrix_description="non-work sov vot3 toll",
                            overwrite=True,
                            default_value=0)

    new_mf137 = matrix_init(matrix_id="mf137",
                            matrix_name="tolled_trips_sov_non-work",
                            matrix_description="non-work sov tolled trips",
                            overwrite=True,
                            default_value=0)

    new_mf138 = matrix_init(matrix_id="mf138",
                            matrix_name="tolled_trips_hov_non-work",
                            matrix_description="non-work hov tolled trips",
                            overwrite=True,
                            default_value=0)

    new_mf140 = matrix_init(matrix_id="mf140",
                            matrix_name="tolled_trips_sov_VOT1_non-work",
                            matrix_description="non-work sov vot1 tolled trips",
                            overwrite=True,
                            default_value=0)

    new_mf141 = matrix_init(matrix_id="mf141",
                            matrix_name="tolled_trips_sov_VOT2_non-work",
                            matrix_description="non-work sov vot2 tolled trips",
                            overwrite=True,
                            default_value=0)						

    new_mf142 = matrix_init(matrix_id="mf142",
                            matrix_name="tolled_trips_sov_VOT3_non-work",
                            matrix_description="non-work sov vot3 tolled trips",
                            overwrite=True,
                            default_value=0)						

    new_ms26 = matrix_init(matrix_id="ms26",
                            matrix_name="tmp26",
                            matrix_description="store non-work SOV VOT1 tolled",
                            overwrite=True,
                            default_value=0)

    new_ms27 = matrix_init(matrix_id="ms27",
                            matrix_name="tmp27",
                            matrix_description="store non-work SOV VOT2 tolled",
                            overwrite=True,
                            default_value=0)

    new_ms28 = matrix_init(matrix_id="ms28",
                            matrix_name="tmp28",
                            matrix_description="store non-work SOV VOT3 tolled",
                            overwrite=True,
                            default_value=0)

print("         Toll skim matrices initialized")  
