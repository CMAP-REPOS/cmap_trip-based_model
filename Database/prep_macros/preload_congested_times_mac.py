#filename: free.skim.mac.py
#description: creates time and distance skims from am-peak and midday scenarios to start distribution and mode choice
#author: Karly Cazzato, 12/29/2023

from asyncio.windows_events import NULL
import os
import os.path
import sys
import inro.emme.desktop.app as _app
import inro.modeller as _m

   
def main():

    #set variables
    currentScen = int(sys.argv[2])
    scenario = str(currentScen)
    cScenMD = currentScen + 5
    directory1 = os.getcwd()
    directory = os.getcwd().replace('\\Database','')

    # Define the path to the Emme project (.emp file)
    empFl = sys.argv[1]
    empFile = os.path.join(directory,empFl)
    desktop = _app.start_dedicated(
        visible=True,
        user_initials='KCC',
        project= empFile
    )
    
    # Connect to the Modeller
    modeller = _m.Modeller(desktop=desktop)
    eb = _m.Modeller().emmebank

    #define tools
    batchin_data = modeller.tool("inro.emme.data.network_field.import_network_fields")
    change_scenario = modeller.tool("inro.emme.data.scenario.change_primary_scenario")
    process_mat = modeller.tool("inro.emme.data.matrix.matrix_transaction")
    net_calc = modeller.tool("inro.emme.network_calculation.network_calculator")
    delete_mat = modeller.tool("inro.emme.data.matrix.delete_matrix")

    #delete report\iter_0\preload_times.rpt if it exists
    if os.path.isfile("report\\iter_0\\preload_times.rpt"): os.remove("report\\iter_0\\preload_times.rpt")

    hwytm_spec = {
        "result": "@hwytm",
        "expression": "ul2",
        "selections": {
            "link": "all",
            "transit_line": "all"
        },
        "type": "NETWORK_CALCULATION"
    }
    ul2_spec = {
        "result": "ul2",
        "expression": "0",
        "selections": {
            "link": "all",
        },
        "type": "NETWORK_CALCULATION"
    }
    #set scenario to transit AM _00, batchin and store per3_timau.txt
    change_scenario(scenario = str(scenario))
    batchin = directory1 + "\\defaults_base_year\\per3_timau.txt" 
    batchin_data(batchin,
                     column_labels = {
                         0 : 'i_node',
                         1: 'j_node',
                         2: 'ul2'
                     }, 
                     revert_on_error=False) 
    
    #add net calc to store ul2 in @hwytm 
    net_calc(hwytm_spec)
    #add net calc to make all ul2 back to 0
    net_calc(ul2_spec)

    #set scenario to transit MD _00, batchin and store per5_timau.txt
    change_scenario(scenario = str(cScenMD)) 
    batchin = directory1 + "\\defaults_base_year\\per5_timau.txt" 
    batchin_data(batchin, 
                     column_labels = {
                         0: 'i_node',
                         1: 'j_node',
                         2: 'ul2'
                     },
                    revert_on_error = False)
    #add net calc to store ul2 in @hwytm 
    net_calc(hwytm_spec)
    #add net calc to make all ul2 back to 0
    net_calc(ul2_spec)

    #change to scenario 100000
    scen_00 = scenario + str(0) + str(0)

    change_scenario(scenario = str(scen_00))

    matpairs = (44, 45, 46, 47, 76, 77)
    for mat in matpairs:
        name = "mf" + str(mat)
        mfName = eb.matrix(name)
        if mfName: delete_mat(matrix = mfName)
        batchin =  directory1 + "\\defaults_base_year\\preload_mf" + str(mat) + ".txt"
        process_mat(batchin)
        print("processed mf_" + str(mat))


        
if __name__ == '__main__':
    main()