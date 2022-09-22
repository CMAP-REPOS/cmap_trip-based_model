'''
6a_write_final_file.py
   Craig Heither, 03-03-2016
   
   Script is called by 6_write_tredis_data.mac to write the final version
   of the TREDIS data input file.  This script also annualizes the VMT values
   to get around the issue of Emme registers not supporting values of 
   10,000,000,000 or higher.
'''
# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import sys, os, csv

read_in = os.getcwd() + "\\" + sys.argv[1]            	### temporary file written by Emme macro
write_out = os.getcwd() + "\\" + sys.argv[2]          	### final file for TREDIS
annfactor = eval(sys.argv[3])							### highway annualization factor


data = list(csv.reader(open(read_in)))

## Apply Annualization Factor to Auto VMT & Update List Values
commute_vmt = eval(data[1][3])*annfactor
personal_vmt = eval(data[2][3])*annfactor
business_vmt = eval(data[3][3])*annfactor
medtruck_vmt = eval(data[4][3])*annfactor
hevtruck_vmt = eval(data[5][3])*annfactor
data[1][3] = commute_vmt
data[2][3] = personal_vmt
data[3][3] = business_vmt
data[4][3] = medtruck_vmt
data[5][3] = hevtruck_vmt

with open(write_out,'w') as stats:
	writer = csv.writer(stats, lineterminator='\n')
	writer.writerows(data)

if os.path.exists(read_in):
    os.remove(read_in)
print("Done")
