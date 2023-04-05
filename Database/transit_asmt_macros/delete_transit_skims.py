## delete_transit_skims.py
##
## Delete transit skim matrices to reduce storage space.
##
## Heither 11-26-2022
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

delete_matrix = my_modeller.tool("inro.emme.data.matrix.delete_matrix")
my_emmebank = my_modeller.emmebank

## Create list of matrices to delete
a = list(range(1100, 1154))       
b = list(range(1300, 1354))
c = list(range(1500, 1554))
d = list(range(1700, 1754))
mtxListing = a + b + c + d

matrices = []
for item in mtxListing:
    matrices.append("mf"+str(item))

for m in matrices:
    delete_matrix(matrix=my_emmebank.matrix(m))

