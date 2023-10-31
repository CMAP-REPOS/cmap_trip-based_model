## select_line_boardings.py
## 
## Calculate boardings for a select line analysis and write the results to file.
##
## Heither 08-21-2023
## ==========================================================================================

import os, sys
import pandas as pd
import inro.modeller as _m
import inro.emme.desktop.app as _app

empFl = sys.argv[1]
currentScen = int(sys.argv[2])
RspFlag = sys.argv[3]
outfilePart = sys.argv[4]

## -- Output file -- ##
if RspFlag == "T":
    output = "\\rsp_evaluation\\results\\"
else:
    output = "\\transit_asmt_macros\\report\\"
outFile = os.getcwd() + output + "{0}_select_line_boardings.csv".format(outfilePart.partition("_")[0])

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=True, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)
my_emmebank = my_modeller.emmebank
scens = (currentScen+21, currentScen+23, currentScen+25, currentScen+27)

x = 0
brd = 0
for s in scens:
    ## -- Set primary scenario -- ##
    scen = int(scens[x])
    scenario = my_emmebank.scenario(scen)
    ## -- Iterate over network -- ##
    network = scenario.get_network()
    for line in network.transit_lines():
        for seg in line.segments():
            brd = brd + (line['@sline'] * seg.transit_boardings)

    x += 1
    print("  --> {0:.0f} cumulative select line boardings through scenario {1}".format(brd, scen))

with open(outFile,'w') as f:
    f.write("{0}".format(str(int(brd))))

print("Boardings written to {0}".format(outFile))