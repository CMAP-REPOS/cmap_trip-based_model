#filename: cleanup_for_rerun.py
#description: Python script uses the EMME Modeler API clean up EMME database prior to full model run
#author: Karly Cazzato, 10/10/2023

import os
import os.path
import shutil
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm
import pandas as pd
import glob
   
def main():

    #set variables
    currentScen = int(sys.argv[1])
    cScenAM = currentScen + 3
    scenario = str(cScenAM)
    cScenMD = currentScen + 5
    moveF11 = "build_" + str(cScenAM) + "transit.rpt"
    moveF12 = "report/" + moveF11
    moveF21 = "build_" + str(cScenMD) + "transit.rpt"        
    moveF22 = "report/" + moveF21

    iter = [0, 1, 2]

    counter1 = 0
    counter2 = 0
    counters1 = [0, 10, 20]
    counters2 = [21, 23, 25, 27]

    # Connect to the Modeller
    proj_dir = Path(__file__).resolve().parents[2]
    modeller = tbm.connect(proj_dir)
    emmebank = modeller.emmebank
    delete_scenario = modeller.tool("inro.emme.data.scenario.delete_scenario")
    delete_matrix = modeller.tool("inro.emme.data.matrix.delete_matrix")
    create_matrix = modeller.tool("inro.emme.data.matrix.create_matrix")
    change_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")

    #First clean up folders 
    if os.path.isfile("data/punchlink.csv"): os.remove("data/punchlink.csv")

    #temporarily move reports to save them from deletion 
    if os.path.isfile(moveF12): shutil.move(moveF12, moveF11) 
    if os.path.isfile(moveF22): shutil.move(moveF22, moveF21) 
    if os.path.isfile("report/transit_skim.rpt"): shutil.move("report/transit_skim.rpt", "transit_skim.rpt")
    if os.path.isfile("report/transit_skim_stats.txt"): shutil.move("report/transit_skim_stats.txt", "transit_skim_stats.txt")
    if os.path.isfile("report/truck.access.rpt"): shutil.move("report/truck.access.rpt", "truck.access.rpt")

    #delete contents of report\iter_X
    for i in iter:
        newpath = "report/iter_" + str(i)
        if not os.path.exists(newpath):os.makedirs(newpath)
        files = newpath + "/*"
        filesG = glob.glob(str(files))
        for f in filesG: os.remove(f)

    #same files as above but move from where it is back to report folder
    if os.path.isfile(moveF11): shutil.move(moveF11, moveF12) 
    if os.path.isfile(moveF21): shutil.move(moveF21, moveF22) 
    if os.path.isfile("transit_skim.rpt"): shutil.move("transit_skim.rpt", "report/transit_skim.rpt")
    if os.path.isfile("transit_skim_stats.txt"): shutil.move("transit_skim_stats.txt", "report/transit_skim_stats.txt")
    if os.path.isfile("truck.access.rpt"): shutil.move("truck.access.rpt", "report/truck.access.rpt")
   
    #Set primary scenario
    change_scenario(scenario=cScenMD)
    #delete scenarios and matrices
    for c in counters1:
        if c == 0:
            while counter1 <= c +9:
                scenDel = emmebank.scenario(counter1)
                if scenDel: delete_scenario(scenDel)    
                counter1 = counter1+1
        else:
            while counter1 <= c +9:
                scen = str(currentScen) + str(counter1)
                scenDel = emmebank.scenario(scen)
                if scenDel: delete_scenario(scenDel)
                counter1 = int(counter1)
                counter1 = counter1+1

    #delete transit overnight and PM peak skim scenarios if exist
    trSkims= (currentScen+1, currentScen+7)
    for t in trSkims:
        scenDel = emmebank.scenario(t)
        if scenDel: delete_scenario(scenDel)

    #delete matrices
    matpairs = {}
    matpairs['index'] = [0, 1, 2, 3, 4, 5, 6, 7]
    matpairs['begin'] = [1, 11, 48, 80, 1100, 1300, 1500, 1700]
    matpairs['end'] = [3, 43, 75, 800, 1153, 1353, 1553, 1753]

    i = 0
    for i in matpairs['index']:
        begVal = matpairs['begin'][i]
        endVal = matpairs['end'][i]
        while begVal <= endVal:
            matrix = emmebank.matrix("mf" + str(begVal))
            begVal = begVal + 1
            if matrix: delete_matrix(matrix)

    #set ms97, ms98, & ms99 to proper values for rerun (create if necessary)
    new_mat97 = create_matrix(matrix_id="ms97",
                            matrix_name="one",
                            matrix_description="dummy for assignment",
                            default_value=1,
                            overwrite ="True") 
    new_mat98 = create_matrix(matrix_id="ms98",
                            matrix_name="count",
                            matrix_description="iteration counter",
                            default_value=0,
                            overwrite ="True") 
    new_mat99 = create_matrix(matrix_id="ms99",
                            matrix_name="dmy9",
                            matrix_description="0 value for convolution opportunity calc",
                            default_value=0,
                            overwrite ="True") 
    
    #delete set of matrices no longer used in model
    unneeded = ("ms1","mf835","mf836","mf839","mf935","mf936","mf939")
    for u in unneeded:
        matrix = emmebank.matrix(u)
        if matrix: delete_matrix(matrix)
        
if __name__ == '__main__':
    main()