'''
 verify_select_link.py

 Script reads the list of select link analysis files and verifies each exists. 
 Script updates Emmebank to support additional extra attribute values if needed.
 Script updates Emmebank to support additional full matrices for transit assignment if needed.  
 
 Craig Heither, 02-05-2025
## ==========================================================================================
'''
import os
import sys
import inro.modeller as _m
import inro.emme.desktop.app as _app
import inro.emme.database.emmebank as _emmebank

empFl = sys.argv[1]
selLink = sys.argv[2]
rspFlag = sys.argv[3]
trnAsmtFlag = int(sys.argv[4])

directory = os.getcwd().replace('\\Database','')
empFile = os.path.join(directory,empFl)
my_app = _app.start_dedicated(project=empFile, visible=False, user_initials="CMAP")
my_modeller = _m.Modeller(my_app)
change_db_dim = my_modeller.tool("inro.emme.data.database.change_database_dimensions")

newSpace = 6000000      ## -- extra attribute storage
fullMat = 1999          ## -- full matrices for transit assignment
fileIssues = 0

s = selLink.split(',')
sl = [e for e in s if e != 'None']
files = len(sl)

if files>0:
    for i in range(0,files):
        if not os.path.isfile(os.getcwd() +"\\Select_Link\\{0}".format(sl[i])):
            print("---> ERROR -- {0} does not exist!! DO NOT CONTINUE <---\n".format(sl[i]))
            fileIssues += 1
            sys.exit(1)

if files==0 and rspFlag=="T" and trnAsmtFlag==0:
    print("---> ERROR: No select link file has been specified for the RSP evaluation!")
    sys.exit(1)
elif files>0 and fileIssues==0:
    print("---> Select Link File(s) verified!")
elif files==0:
    print("---> Verified: No Select Link File(s) submitted.")


## -- Ensure emmebank has enough space for extra attributes and full matrices -- ##
with _emmebank.Emmebank() as ebank:
    a = ebank.dimensions['extra_attribute_values']
    b = ebank.dimensions['full_matrices']
    if a<newSpace and (files>0 or trnAsmtFlag==1):
        print("===> Updating Emmebank extra attributes values to support analysis! <===")
        new_dimensions = ebank.dimensions
        new_dimensions["extra_attribute_values"] = newSpace
        change_db_dim(emmebank_dimensions=new_dimensions, keep_backup=False)
        print("----> Emmebank extra attributes values: {0}! <----".format(ebank.dimensions['extra_attribute_values']))
    if b<fullMat and trnAsmtFlag==1:
        print("===> Updating Emmebank full matrices to support transit assignment! <===")
        new_dimensions = ebank.dimensions
        new_dimensions["full_matrices"] = fullMat
        change_db_dim(emmebank_dimensions=new_dimensions, keep_backup=False)
        print("----> Emmebank full matrices: {0}! <----".format(ebank.dimensions['full_matrices']))