#filename: input_data_mac.py
#description: export congested travel times for use in new model run
#author: Karly Cazzato, 1/9/2024
#TO RUN IN VSCODE
#1. In File Explorer, open the script in the model setup by right-clicking and selecting Open with Code
#2. In VS Code, select Run -> Run without Debugging

#from asyncio.windows_events import NULL
import os
import os.path
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))
from tbmtools import project as tbm


def main():
    # print("WARNING: DO NOT RUN ON AN OFFICIAL ARCHIVED CONFORMITY RUN")
    # print("please make sure that you are using a temporary local copy of the model run.")
    # copyModel = input("are you using a temporary copy of the model? type y or n: ")
    # if not "y" in copyModel: quit
    # print("running script")

    #set paths
    proj_dir = Path(__file__).resolve().parents[2]
    outdir = proj_dir.joinpath('Database', 'defaults_base_year')
    if not os.path.exists(outdir): os.makedirs(outdir)
    
    # Connect to the Modeller
    modeller = tbm.connect(proj_dir)

    #define tools
    change_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")
    export_mat = modeller.tool("inro.emme.data.matrix.export_matrices")
    net_calc = modeller.tool("inro.emme.network_calculation.network_calculator")
    create_extra = modeller.tool("inro.emme.data.extra_attribute.create_extra_attribute")
    export_att = modeller.tool("inro.emme.data.extra_attribute.export_extra_attributes")
  
    timau_spec = {
        "expression": "timau",
        "selections": {
            "link": "all",
        },
        "type": "NETWORK_CALCULATION",
        "result": "@temptimau"
    }
    
    #create per3_timau.txt
    change_scenario(scenario = "3")
    create_extra(extra_attribute_type="LINK",
                 extra_attribute_name="@temptimau",
                 extra_attribute_description="store timau for export",
                 overwrite=True) 
    net_calc(timau_spec, full_report = True)
    export_att(extra_attributes = "@temptimau",
               export_path = outdir)

    #create per5_timau.txt
    change_scenario(scenario = "5") 
    create_extra(extra_attribute_type="LINK",
                 extra_attribute_name="@temptimau",
                 extra_attribute_description="store timau for export",
                 overwrite=True) 
    net_calc(timau_spec, full_report = True)
    export_att(extra_attributes = "@temptimau",
               export_path = outdir)

    os.chdir(outdir)
    if os.path.isfile("per3_timau.txt"): os.remove("per3_timau.txt")
    if os.path.isfile("per5_timau.txt"): os.remove("per5_timau.txt")

    os.rename("extra_links_3.txt", "per3_timau.txt")
    os.rename("extra_links_5.txt", "per5_timau.txt")

    #matrix numbers
    mats = [44, 45, 46, 47, 76, 77]
    #write matrices 
    for matrix in mats:
        matrix_file =  str(outdir.joinpath(f"preload_mf{matrix}.txt"))
        print(f"Exporting {matrix_file}...")
        export_mat(matrices = "mf" + str(matrix),
                   export_file = matrix_file)


if __name__ == '__main__':
    main()