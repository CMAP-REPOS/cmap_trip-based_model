#filename: input_data_mac.py
#description: export congested travel times for use in new model run
#author: Karly Cazzato, 1/9/2024
#TO RUN IN VSCODE
#1. In File Explorer, open the script in the model setup by right-clicking and selecting Open with Code
#2. In VS Code, select Run -> Run without Debugging

from asyncio.windows_events import NULL
import os
import os.path
import sys
   
def main():
    import inro.emme.desktop.app as _app
    import inro.modeller as _m
    print("WARNING: DO NOT RUN ON AN OFFICIAL ARCHIVED CONFORMITY RUN")
    print("please make sure that you are using a temporary local copy of the model run.")
    copyModel = input("are you using a temporary copy of the model? type y or n: ")
    if not "y" in copyModel: quit
    print("running script")

    #set paths
    DBdir = os.path.realpath(__file__).replace('\\useful_macros\\input_data_mac.py', "")
    EmpDir = DBdir.replace('\\Database','')
    outdir = DBdir + '\\defaults_base_year'
    if not os.path.exists(outdir): os.makedirs(outdir)

    # Define the path to the Emme project (.emp file)
    project = input("enter your project number followed by an underscore and then the scenario number (ex: c23q4_100): ")
    #check below
    currentScen = int(project[-3:]) 
    scenario = str(currentScen)
    cScenMD = currentScen + 5
    print("current scenario = " + str(scenario))
    print("midday scenario = " + str(cScenMD))

    empFl = project + ".emp"
    empFile = os.path.join(EmpDir,empFl)
    desktop = _app.start_dedicated(
        visible=True,
        user_initials='KCC',
        project= empFile
    )
    
    # Connect to the Modeller
    modeller = _m.Modeller(desktop=desktop)

    #define tools
    change_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")
    export_mat = modeller.tool("inro.emme.data.matrix.export_matrices")
    export_field = modeller.tool("inro.emme.data.network_field.export_network_fields")
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
    change_scenario(scenario = str(3))
    new_att = create_extra(extra_attribute_type="LINK",
                       extra_attribute_name="@temptimau",
                       extra_attribute_description="store timau for export",
                       overwrite=True) 
    timau_file =  outdir + "\\per3_timau.txt" 
    net_calc(timau_spec, full_report = True)
    export_att(extra_attributes = "@temptimau",
               export_path = outdir)

    #create per5_timau.txt
    if os.path.isfile("report\\defaults_base_year\\per5_timau.txt"): os.remove("report\\defaults_base_year\\per5_timau.txt")
    change_scenario(scenario = str(5)) 
    new_att = create_extra(extra_attribute_type="LINK",
                       extra_attribute_name="@temptimau",
                       extra_attribute_description="store timau for export",
                       overwrite=True) 
    timau_file = outdir + "\\per5_timau.txt"
    net_calc(timau_spec, full_report = True)
    export_att(extra_attributes = "@temptimau",
               export_path = outdir)

    os.chdir(outdir)
    if os.path.isfile("per3_timau.txt"): os.remove("per3_timau.txt")
    if os.path.isfile("per5_timau.txt"): os.remove("per5_timau.txt")

    os.rename("extra_links_3.txt", "per3_timau.txt")
    os.rename("extra_links_5.txt", "per5_timau.txt")

    #dictionary for matrix numbers
    matpairs = {44, 45, 46, 47, 76, 77}
    #loop write matrices 
    for matrix in matpairs:
        matrices_file =  outdir + "\\preload_mf" + str(matrix) + ".txt"
        print("matrix file = " + matrices_file)
        print("matrix ID = " + str(matrix))
        export_mat(matrices = "mf" + str(matrix),
                   export_file = matrices_file)
        print("processed mf_" + str(matrix))

    #end 
                
if __name__ == '__main__':
    main()