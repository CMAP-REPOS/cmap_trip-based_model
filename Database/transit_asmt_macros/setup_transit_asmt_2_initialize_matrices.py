## setup_transit_asmt_2_initialize_matrices.py
##
## Initialize matrices to hold time-of-day transit demand.
##
## Heither 11-22-2022
## ==========================================================================================
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)

matrix_init = my_modeller.tool("inro.emme.data.matrix.create_matrix")

new_mf501 = matrix_init(matrix_id="mf501",
                        matrix_name="TRN_WALK_L_NT",
                        matrix_description="Period NT transit demand (O-D format) - VOT 1",
						overwrite=True,
                        default_value=0) 
						
new_mf502 = matrix_init(matrix_id="mf502",
                        matrix_name="TRN_WALK_M_NT",
                        matrix_description="Period NT transit demand (O-D format) - VOT 2",
						overwrite=True,
                        default_value=0)						

new_mf503 = matrix_init(matrix_id="mf503",
                        matrix_name="TRN_WALK_H_NT",
                        matrix_description="Period NT transit demand (O-D format) - VOT 3",
						overwrite=True,
                        default_value=0)
                        
new_mf504 = matrix_init(matrix_id="mf504",
                        matrix_name="TRN_WALK_L_AM",
                        matrix_description="Period AM transit demand (O-D format) - VOT 1",
						overwrite=True,
                        default_value=0) 
						
new_mf505 = matrix_init(matrix_id="mf505",
                        matrix_name="TRN_WALK_M_AM",
                        matrix_description="Period AM transit demand (O-D format) - VOT 2",
						overwrite=True,
                        default_value=0)						

new_mf506 = matrix_init(matrix_id="mf506",
                        matrix_name="TRN_WALK_H_AM",
                        matrix_description="Period AM transit demand (O-D format) - VOT 3",
						overwrite=True,
                        default_value=0)
                        
new_mf507 = matrix_init(matrix_id="mf507",
                        matrix_name="TRN_WALK_L_MD",
                        matrix_description="Period MD transit demand (O-D format) - VOT 1",
						overwrite=True,
                        default_value=0) 
						
new_mf508 = matrix_init(matrix_id="mf508",
                        matrix_name="TRN_WALK_M_MD",
                        matrix_description="Period MD transit demand (O-D format) - VOT 2",
						overwrite=True,
                        default_value=0)						

new_mf509 = matrix_init(matrix_id="mf509",
                        matrix_name="TRN_WALK_H_MD",
                        matrix_description="Period MD transit demand (O-D format) - VOT 3",
						overwrite=True,
                        default_value=0)  
                        
new_mf510 = matrix_init(matrix_id="mf510",
                        matrix_name="TRN_WALK_L_PM",
                        matrix_description="Period PM transit demand (O-D format) - VOT 1",
						overwrite=True,
                        default_value=0) 
						
new_mf511 = matrix_init(matrix_id="mf511",
                        matrix_name="TRN_WALK_M_PM",
                        matrix_description="Period PM transit demand (O-D format) - VOT 2",
						overwrite=True,
                        default_value=0)						

new_mf512 = matrix_init(matrix_id="mf512",
                        matrix_name="TRN_WALK_H_PM",
                        matrix_description="Period PM transit demand (O-D format) - VOT 3",
						overwrite=True,
                        default_value=0) 
                        