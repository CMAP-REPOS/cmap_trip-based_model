'''
#####################################################################################
CREATE_HHVYTPE_FILE.PY
  Craig Heither, rev. 10-30-2014

    Script reads enumerated household file created by TG (HI_HHENUM_TRIP_OUT.TXT)
    and geography correspondence file (GEOG_IN.TXT) to create the file
    Database\HI_HHENUM_IN.TXT required by the non-work vehicle occupancy
    procedures. The file has the following fields:
        subzone, zone, household type (vehicle definition)


	Revisions: 10-30-2014 - renamed output file HI_HHENUM_IN.TXT to 
				TG_HHENUM_OUTPUT.TXT to avoid confusion with another
				file using the same name.

#####################################################################################
'''

# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import sys, os, csv

geog_input = os.getcwd() + "\\tg\\fortran\\GEOG_IN.TXT"                     ### subzone-zone correspondence file
hh_input = os.getcwd() + "\\tg\\fortran\\HI_HHENUM_TRIP_OUT.TXT"            ### enumerated HH file
hh_output = os.getcwd() + "\\tg\\fortran\\TG_HHENUM_OUTPUT.TXT"

if os.path.exists(hh_output):
    os.remove(hh_output)


# ----------------------------------------------------------------------------
#  Read Geography file, create dictionary with key [subzone] & value [zone]. 
# ----------------------------------------------------------------------------  
print("Creating File of Household Types by Subzone")
subz={}
reader = csv.reader(open(geog_input), delimiter=',')
for row in reader:
    subz[eval(row[0])]=eval(row[6])                     ### assigns key (first object in row [0]) & value (2nd object in row [6]) pair
                                                        ### eval function converts from string to integers 


# ----------------------------------------------------------------------------
#  Read Enumerated HH File, extract subzone & hhvtype fields, write file. 
# ----------------------------------------------------------------------------
outFile = open(hh_output, 'w')
with open(hh_input,'r+b') as hh:
    data = hh.readlines()
    for a_line in data:
        subzn = eval(a_line[:5])
        hhvtype = eval(a_line[17:20])
        outFile.write("{0},{1},{2}\n".format(subzn, subz[subzn], hhvtype))

outFile.close()
print("Done")
