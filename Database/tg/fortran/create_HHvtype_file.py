'''
#####################################################################################
CREATE_HHVYTPE_FILE.PY
  Craig Heither, rev. 04-15-2021

    Script reads enumerated household file created by TG (HI_HHENUM_TRIP_OUT.TXT)
    and geography correspondence file (GEOG_IN.TXT) to create the file
    Database\TG_HHENUM_OUTPUT.TXT required by the non-work vehicle occupancy
    procedures. The file has the following fields:
        subzone, zone, household type (vehicle definition)

#####################################################################################
'''
import os
import pandas as pd

geog_input = os.getcwd() + "\\tg\\fortran\\GEOG_IN.TXT"                     ### subzone-zone correspondence file
hh_input = os.getcwd() + "\\tg\\fortran\\HI_HHENUM_TRIP_OUT.TXT"            ### enumerated HH file
hh_output = os.getcwd() + "\\tg\\fortran\\TG_HHENUM_OUTPUT.TXT"

if os.path.exists(hh_output):
    os.remove(hh_output)

print("Creating File of Household Types by Subzone ...")

# ----------------------------------------------------------------------------
#  Read in Geography file. 
# ----------------------------------------------------------------------------  
geo=pd.read_csv(geog_input, sep=',', header=None)	
geo.columns=['subzone','county','name','state','puma','zone','chicago','cbd','rc','area','cmap']
geo.drop(columns=['county','name','state','puma','chicago','cbd','rc','area','cmap'], axis=1, inplace=True)	### drop unnecessary columns

# ----------------------------------------------------------------------------
#  Read in Trip Enumeration file. 
# ----------------------------------------------------------------------------  
hh=pd.read_fwf(hh_input, widths=[5,7,4,4], header=None)	
hh.columns=['subzone','puma','hhtype','hhvtype']
hh.drop(columns=['puma','hhtype'], axis=1, inplace=True)					### drop unnecessary columns

# ----------------------------------------------------------------------------
#  Merge dataframes, sort data and write file.
# ---------------------------------------------------------------------------- 
hh1 = hh.merge(geo, how='left', on='subzone', copy=False)
hh1.sort_values(by=['zone', 'subzone'], inplace=True)
hh1.to_csv(hh_output, columns=['subzone','zone','hhvtype'], header=False, index=False)
