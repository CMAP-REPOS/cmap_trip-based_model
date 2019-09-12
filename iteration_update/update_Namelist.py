'''
update_Namelist.py
   Craig Heither, 09-08-2016
   
   Script is called by Submit_Full_Regional_Model.bat to update the ITER values
   in all of the Pre-Distribution and Mode Choice NAMELIST files: one value is used
   for model global iterations 0-3 and a different value is used for global iteration 4. 
'''
# ----------------------------------------------------------------------------
# Import System Modules and Set Variables.
# ----------------------------------------------------------------------------
import sys, os, shutil, string
	
sims = eval(sys.argv[1])             				## number of simulations	
repl_text = ("  ITER={0}  &END".format(sims,))		## replacement text	

 # -- File Locations
temp_a = os.getcwd() + "\\temp_a.txt"
pdho = os.getcwd() + "\\PDHO_NAMELIST.TXT"
pdhw = os.getcwd() + "\\PDHW_NAMELIST.TXT"
pdnh = os.getcwd() + "\\PDNH_NAMELIST.TXT"
mcho = os.getcwd() + "\\MC_HO_NAMELIST.TXT"
mcwh = os.getcwd() + "\\MC_HWhigh_NAMELIST.TXT"
mcwl = os.getcwd() + "\\MC_HWlow_NAMELIST.TXT"
mcnh = os.getcwd() + "\\MC_NH_NAMELIST.TXT"


# ----------------------------------------------------------------------------
# Process Files.
# ----------------------------------------------------------------------------
filelist = (pdho, pdhw, pdnh, mcho, mcwh, mcwl, mcnh)

for f in filelist:
	shutil.copy(f, temp_a)
	with open(temp_a,'r+b') as f1:
		with open(f, "w") as f2:
			for line in iter(f1):
				if len(string.split(line, 'ITER=')) > 1:
					f2.write(repl_text + "\n")
				else:
					f2.write(line.rstrip() + "\n")	## rstrip to remove trailing newlines to enforce file integrity

if os.path.exists(temp_a):
    os.remove(temp_a)					
print("NAMELIST File Updates Completed")	
	
