#filename: init_HOVsim_databk_mac.py
#description: deletes old matrices and initializes matrices for each iteration of model run
#author: Karly Cazzato

from asyncio.windows_events import NULL
import os
import os.path
import sys
   
def main():
    import inro.emme.desktop.app as _app
    import inro.modeller as _m
    #paths
    #set paths
    DBdir = os.getcwd().replace('\\Database','')
    EmpDir = DBdir.replace('\\Database','')
    EmmatDir = os.getcwd() + '/emmemat'
    # Define the path to the Emme project (.emp file)
    #check below
    scenario = int(sys.argv[1])
    scen = str(scenario)
    
    # Define the path to the Emme project (.emp file)
    empFl = sys.argv[3]
    empFile = os.path.join(EmpDir,empFl)
    desktop = _app.start_dedicated(
        visible=True,
        user_initials='KCC',
        project= empFile
    )

    x = str(sys.argv[2]) #original macro calls %ms98%, calling the counter is easier here

    # Connect to the Modeller
    modeller = _m.Modeller(desktop=desktop)

    #define tools
    create_matrix = modeller.tool("inro.emme.data.matrix.create_matrix")
    iteration = modeller.tool("inro.emme.matrix_calculation.matrix_calculator")
        
    #create 
    new_mat38 = create_matrix(matrix_id = "mf38", matrix_name = "vsttr" + x,
                            matrix_description = "alt" + scen + "visitor transit trips sim_" + x,
                            default_value = 0, overwrite = True)
    
    new_mat39 = create_matrix(matrix_id = "mf39", matrix_name = "hshtr" + x,
                            matrix_description = "alt" + scen + "hshop transit trips sim_" + x,
                            default_value = 0, overwrite = True)

    new_mat40 = create_matrix(matrix_id = "mf40", matrix_name = "hwtrL" + x,
                            matrix_description = "alt" + scen + "hw transit trips sim_" + x + "- low inc",
                            default_value = 0, overwrite = True)
    
    new_mat41 = create_matrix(matrix_id = "mf41", matrix_name = "hwtrH" + x,
                            matrix_description = "alt" + scen + "hw transit trips sim_" + x + "- high inc",
                            default_value = 0, overwrite = True)
    
    new_mat42 = create_matrix(matrix_id = "mf42", matrix_name = "hotr" + x,
                            matrix_description = "alt" + scen + "ho transit trips sim_" + x,
                            default_value = 0, overwrite = True)
    
    new_mat43 = create_matrix(matrix_id = "mf43", matrix_name = "nhtr" + x,
                            matrix_description = "alt" + scen + "nh transit trips sim_" + x,
                            default_value = 0, overwrite = True)
    #SOV VOT1
    new_mat411 = create_matrix(matrix_id = "mf411", matrix_name = "s1t1veh" + x,
                            matrix_description = "SOV VOT1 Time Period 1 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat412 = create_matrix(matrix_id = "mf412", matrix_name = "s1t2veh" + x,
                            matrix_description = "SOV VOT1 Time Period 2 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat413 = create_matrix(matrix_id = "mf413", matrix_name = "s1t3veh" + x,
                            matrix_description = "SOV VOT1 Time Period 3 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat414 = create_matrix(matrix_id = "mf414", matrix_name = "s1t4veh" + x,
                            matrix_description = "SOV VOT1 Time Period 4 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat415 = create_matrix(matrix_id = "mf415", matrix_name = "s1t5veh" + x,
                            matrix_description = "SOV VOT1 Time Period 5 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat416 = create_matrix(matrix_id = "mf416", matrix_name = "s1t6veh" + x,
                            matrix_description = "SOV VOT1 Time Period 6 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat417 = create_matrix(matrix_id = "mf417", matrix_name = "s1t7veh" + x,
                            matrix_description = "SOV VOT1 Time Period 7 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat418 = create_matrix(matrix_id = "mf418", matrix_name = "s1t8veh" + x,
                            matrix_description = "SOV VOT1 Time Period 8 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    #SOV VOT2
    new_mat421 = create_matrix(matrix_id = "mf421", matrix_name = "s2t1veh" + x,
                            matrix_description = "SOV VOT2 Time Period 1 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat422 = create_matrix(matrix_id = "mf422", matrix_name = "s2t2veh" + x,
                            matrix_description = "SOV VOT2 Time Period 2 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat423 = create_matrix(matrix_id = "mf423", matrix_name = "s2t3veh" + x,
                            matrix_description = "SOV VOT2 Time Period 3 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat424 = create_matrix(matrix_id = "mf424", matrix_name = "s2t4veh" + x,
                            matrix_description = "SOV VOT2 Time Period 4 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat425 = create_matrix(matrix_id = "mf425", matrix_name = "s2t5veh" + x,
                            matrix_description = "SOV VOT2 Time Period 5 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat426 = create_matrix(matrix_id = "mf426", matrix_name = "s2t6veh" + x,
                            matrix_description = "SOV VOT2 Time Period 6 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat427 = create_matrix(matrix_id = "mf427", matrix_name = "s2t7veh" + x,
                            matrix_description = "SOV VOT2 Time Period 7 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat428 = create_matrix(matrix_id = "mf428", matrix_name = "s2t8veh" + x,
                            matrix_description = "SOV VOT2 Time Period 8 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    #SOV VOT3
    new_mat431 = create_matrix(matrix_id = "mf431", matrix_name = "s3t1veh" + x,
                            matrix_description = "SOV VOT3 Time Period 1 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat432 = create_matrix(matrix_id = "mf432", matrix_name = "s3t2veh" + x,
                            matrix_description = "SOV VOT3 Time Period 2 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat433 = create_matrix(matrix_id = "mf433", matrix_name = "s3t3veh" + x,
                            matrix_description = "SOV VOT3 Time Period 3 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat434 = create_matrix(matrix_id = "mf434", matrix_name = "s3t4veh" + x,
                            matrix_description = "SOV VOT3 Time Period 4 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat435 = create_matrix(matrix_id = "mf435", matrix_name = "s3t5veh" + x,
                            matrix_description = "SOV VOT3 Time Period 5 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat436 = create_matrix(matrix_id = "mf436", matrix_name = "s3t6veh" + x,
                            matrix_description = "SOV VOT3 Time Period 6 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat437 = create_matrix(matrix_id = "mf437", matrix_name = "s3t7veh" + x,
                            matrix_description = "SOV VOT3 Time Period 7 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat438 = create_matrix(matrix_id = "mf438", matrix_name = "s3t8veh" + x,
                            matrix_description = "SOV VOT3 Time Period 8 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    #HOV2
    new_mat441 = create_matrix(matrix_id = "mf441", matrix_name = "h2t1veh" + x,
                            matrix_description = "HOV2 Time Period 1 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat442 = create_matrix(matrix_id = "mf442", matrix_name = "h2t2veh" + x,
                            matrix_description = "HOV2 Time Period 2 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat443 = create_matrix(matrix_id = "mf443", matrix_name = "h2t3veh" + x,
                            matrix_description = "HOV2 Time Period 3 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat444 = create_matrix(matrix_id = "mf444", matrix_name = "h2t4veh" + x,
                            matrix_description = "HOV2 Time Period 4 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat445 = create_matrix(matrix_id = "mf445", matrix_name = "h2t5veh" + x,
                            matrix_description = "HOV2 Time Period 5 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat446 = create_matrix(matrix_id = "mf446", matrix_name = "h2t6veh" + x,
                            matrix_description = "HOV2 Time Period 6 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat447 = create_matrix(matrix_id = "mf447", matrix_name = "h2t7veh" + x,
                            matrix_description = "HOV2 Time Period 7 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat448 = create_matrix(matrix_id = "mf448", matrix_name = "h2t8veh" + x,
                            matrix_description = "HOV2 Time Period 8 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    
    #HOV3
    new_mat451 = create_matrix(matrix_id = "mf451", matrix_name = "h3t1veh" + x,
                            matrix_description = "HOV3 Time Period 1 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat452 = create_matrix(matrix_id = "mf452", matrix_name = "h3t2veh" + x,
                            matrix_description = "HOV3 Time Period 2 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat453 = create_matrix(matrix_id = "mf453", matrix_name = "h3t3veh" + x,
                            matrix_description = "HOV3 Time Period 3 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat454 = create_matrix(matrix_id = "mf454", matrix_name = "h3t4veh" + x,
                            matrix_description = "HOV3 Time Period 4 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat455 = create_matrix(matrix_id = "mf455", matrix_name = "h3t5veh" + x,
                            matrix_description = "HOV3 Time Period 5 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat456 = create_matrix(matrix_id = "mf456", matrix_name = "h3t6veh" + x,
                            matrix_description = "HOV3 Time Period 6 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat457 = create_matrix(matrix_id = "mf457", matrix_name = "h3t7veh" + x,
                            matrix_description = "HOV3 Time Period 7 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    new_mat458 = create_matrix(matrix_id = "mf458", matrix_name = "h3t8veh" + x,
                            matrix_description = "HOV3 Time Period 8 vehicle trips sim_" + x,
                            default_value = 0, overwrite = True)
    
    #Report existing matrices in databank
    #going to code it so it reports all matrices in Data/emmemat
    file_path = EmmatDir
    with open("report\iter.report.rxt", mode='w', newline='') as fp:
        for file in os.listdir(file_path):
            if file.endswith('.emx'):
                fp.write(str(file) + os.linesep) 
    
    #end 
                
if __name__ == '__main__':
    main()