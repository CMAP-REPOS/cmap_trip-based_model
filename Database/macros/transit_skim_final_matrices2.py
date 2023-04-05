#############################################################################
# TRANSIT_SKIM_FINAL_MATRICES2.PY                                           #
#  Craig Heither, last revised 04-04-2019                                   #
#                                                                           #
#    This program performs the matrix convolution portion of the transit    #
#    skim procedures (much more efficiently than Emme) for the midday.      #
#                                                                           #
#    Written to work with Emme 4 structure.                                 #
#    Rev Bozic to work with series 900 midday matrices 11/2/2017            #
#              for integration with global iterations                       #
#    04-04-2019: Heither - implement vectorized calculations using NumPy.   #
# 07/23/2020 Ferguson: Explicity cast kzone values as integers for          #
#            compatibility with np.add() in NumPy 1.16.                     #
#############################################################################

import os, string, array, numpy as np
from array import *

# ----------------------------------------------------------------------------------------
# Set Variables.
#    Note: these should only be changed if the transit skim procedures are altered.
# ----------------------------------------------------------------------------------------
embank = os.getcwd() + "\\emmebank"
mtxpath = os.getcwd() + "\\emmemat"                         ### path to Emme matrix storage

 #   -- Input Matrix Numbers --
inputmtx = (46, 903, 904, 905, 908, 909, 910, 911, 918, 919, 920, 921)
 #   -- Output Matrix Numbers --
outputmtx = (922, 923, 924, 925, 928, 929, 930, 931, 932, 933, 934)

#   -- Input Matrices --
mfauto = mtxpath + "\\mf" + str(inputmtx[0]) + ".emx"        ### midday hwy time matrix (mf46)
mffmode = mtxpath + "\\mf" + str(inputmtx[1]) + ".emx"       ### skimmed first mode (mf903)
mfpmode = mtxpath + "\\mf" + str(inputmtx[2]) + ".emx"       ### skimmed priority mode (mf904)
mflmode = mtxpath + "\\mf" + str(inputmtx[3]) + ".emx"       ### skimmed last mode (mf905)
mfinveh = mtxpath + "\\mf" + str(inputmtx[4]) + ".emx"       ### skimmed in-vehicle minutes (mf908)
mftrnfr = mtxpath + "\\mf" + str(inputmtx[5]) + ".emx"       ### skimmed transfer link minutes (mf909)
mftwait = mtxpath + "\\mf" + str(inputmtx[6]) + ".emx"       ### skimmed total wait minutes (mf910)
mffwait = mtxpath + "\\mf" + str(inputmtx[7]) + ".emx"       ### skimmed first wait minutes (mf911)
mfafare = mtxpath + "\\mf" + str(inputmtx[8]) + ".emx"       ### skimmed final average fare (mf918)
mfcghwy = mtxpath + "\\mf" + str(inputmtx[9]) + ".emx"       ### congested hwy generalized cost matrix (mf919)
mftcost = mtxpath + "\\mf" + str(inputmtx[10]) + ".emx"      ### indexed transit generalized cost (mf920)
mfkzone = mtxpath + "\\mf" + str(inputmtx[11]) + ".emx"      ### intermediate zone matrix (mf921)
#   -- Output Matrices --
mfinvehi = mtxpath + "\\mf" + str(outputmtx[0]) + ".emx"     ### indexed in-vehicle minutes (mf922)
mftrnfri = mtxpath + "\\mf" + str(outputmtx[1]) + ".emx"     ### indexed walk transfer minutes (mf923)
mftwaiti = mtxpath + "\\mf" + str(outputmtx[2]) + ".emx"     ### indexed total wait minutes (mf924)
mffwaiti = mtxpath + "\\mf" + str(outputmtx[3]) + ".emx"     ### indexed first wait minutes (mf925)
mfafarei = mtxpath + "\\mf" + str(outputmtx[4]) + ".emx"     ### indexed final average fare (mf928)
mffmodei = mtxpath + "\\mf" + str(outputmtx[5]) + ".emx"     ### indexed first mode (mf929)
mfpmodei = mtxpath + "\\mf" + str(outputmtx[6]) + ".emx"     ### indexed priority mode (mf930)
mflmodei = mtxpath + "\\mf" + str(outputmtx[7]) + ".emx"     ### indexed last mode (mf931)
mfacosti = mtxpath + "\\mf" + str(outputmtx[8]) + ".emx"     ### indexed auto generalized cost (mf932)
mfautrni = mtxpath + "\\mf" + str(outputmtx[9]) + ".emx"     ### indexed auto min. to transit (mf933)
mfratioi = mtxpath + "\\mf" + str(outputmtx[10]) + ".emx"    ### indexed transit/auto only (mf934)

#   -- Others --
cutoff = 0.4                         ### cutoff value for indexed transit cost/auto only trip cost
stats = os.getcwd() + "\\report\\transit_skim_stats9.txt"

if os.path.exists(stats):
    os.remove(stats)

# ---------------------------------------------------------------
# Open emmebank and read metadata.
# ---------------------------------------------------------------
a0 = array('i')
a1 = array('i')
with open(embank,'r+b') as f:
    # ## Read first 512 items: this is File 0 - metadata about files ##
    a0.fromfile(f,512)            ## grabs file offset, # of records, words/record & file type: 1-integer, 2-real, 3-text

    # ## Read Global parameters from File 1: array index starts with zero not 1, so is -1 from EMME documentation ##
    offst = a0[102] * 4           ## File 1 offset: elements * 4 bytes
    f.seek(offst, 0)
    a1.fromfile(f,80)
    mcent = a1[51]              ## maximum number of centroids defined in emmebank
    print("\tREADING EMMEBANK: {0} Centroids".format(str(mcent),))

    # ## Read Project title from File 2 ##
    offst = a0[104] * 4
    f.seek(offst, 0)

    # ## Write Emmebank parameters to File to review ##
    outFile = open(stats, 'w')
    outFile.write("Max. scenarios: {0} \n".format((a1[50]),))
    outFile.write("Max. centroids: {0} \n".format((a1[51]),))
    outFile.write("Max. nodes: {0} \n".format((a1[52]),))
    outFile.write("Max. links: {0} \n".format((a1[53]),))
    outFile.write("Max. length turn penalty table: {0} \n".format((a1[54]),))
    outFile.write("Max. transit lines: {0} \n".format((a1[55]),))
    outFile.write("Max. line segments: {0} \n".format((a1[56]),))
    outFile.write("Max. number of matrices: {0} \n".format((a1[57]),))
    outFile.write("Max. number of functions/class: {0} \n".format((a1[58]),))
    outFile.write("Max. number of operators/function class: {0} \n".format((a1[59]),))
    outFile.write("\n")
    for z in range(1, 100):
        outFile.write("File {0:>3} -  Type {1}  Offset {2:>10}  Word/Rec {3:>8}  Records {4:>4} \n".format(z, (a0[z+400]), (a0[2*z+100]), (a0[z+300]), (a0[z])))

    outFile.close()


# ---------------------------------------------------------------
# Store matrix values in arrays.
# ---------------------------------------------------------------
 #   -- Input Matrices --
dt = np.dtype('float32') 
auto = np.fromfile(mfauto, dtype=dt)						
kzone = np.fromfile(mfkzone, dtype=dt)
tcost = np.fromfile(mftcost, dtype=dt)
inveh = np.fromfile(mfinveh, dtype=dt)
trnfr = np.fromfile(mftrnfr, dtype=dt)
twait = np.fromfile(mftwait, dtype=dt)
fwait = np.fromfile(mffwait, dtype=dt)
afare = np.fromfile(mfafare, dtype=dt)
fmode = np.fromfile(mffmode, dtype=dt)
pmode = np.fromfile(mfpmode, dtype=dt)
lmode = np.fromfile(mflmode, dtype=dt)
cghwy = np.fromfile(mfcghwy, dtype=dt)

## -- create leg1 (p-k) indices
indxloc = np.arange(mcent*mcent)							## -- array of consecutive numbers representing element index values
leg1pt1 = np.divide(indxloc,mcent)
leg1pt1 = np.multiply(leg1pt1.astype('i4'),mcent,dtype='i4')	## -- portion of element index defining origin zone 
leg1indx = np.add(leg1pt1,kzone.astype('i4')-1,dtype='i4')	## -- add portion of element index defining destination zone
print("Kzone 1-1: {0}, Index 1-1: {1}, Kzone 121-2: {2}, Index 121-2: {3} \n".format(kzone[0], leg1indx[0], kzone[437882], leg1indx[437882]))

## -- create leg2 (k-q) indices
leg2pt1 = np.multiply(kzone.astype('i4')-1,mcent)
leg2pt2 = np.mod(indxloc,mcent)
leg2indx = np.add(leg2pt1,leg2pt2,dtype='i4')
print("Kzone 1-1: {0}, Index 1-1: {1}, Kzone 121-2: {2}, Index 121-2: {3} \n".format(kzone[0], leg2indx[0], kzone[437882], leg2indx[437882]))

# ---------------------------------------------------------------
# Create indexed matrices.
# ---------------------------------------------------------------
autoval = np.where(kzone>0, auto[leg1indx], kzone)					## -- hwy time matrix
tcostval = np.where(kzone>0, tcost[leg1indx], kzone)				## -- indexed transit generalized cost
invehval = np.where(kzone>0, inveh[leg2indx], kzone)				## -- skimmed in-vehicle minutes
trnfrval = np.where(kzone>0, trnfr[leg2indx], kzone)				## -- skimmed transfer link minutes
twaitval = np.where(kzone>0, twait[leg2indx], kzone)				## -- skimmed total wait minutes
fwaitval = np.where(kzone>0, fwait[leg2indx], kzone)				## -- skimmed first wait minutes
afareval = np.where(kzone>0, afare[leg2indx], kzone)				## -- skimmed final average fare
fmodeval = np.where(kzone>0, fmode[leg2indx], kzone)				## -- skimmed first mode
pmodeval = np.where(kzone>0, pmode[leg2indx], kzone)				## -- skimmed priority mode
lmodeval = np.where(kzone>0, lmode[leg2indx], kzone)				## -- skimmed last mode
threshold = np.where(cghwy>0, np.divide(tcostval,cghwy), cghwy)		## -- ratio of indexed transit cost to auto only cost

## -- Swap original matrix value back in if the threshold exceeds the cutoff value
autoval = np.where(threshold>cutoff, 0, autoval).astype(dt)
tcostval = np.where(threshold>cutoff, 0, tcostval).astype(dt)
invehval = np.where(threshold>cutoff, inveh, invehval).astype(dt)
trnfrval = np.where(threshold>cutoff, trnfr, trnfrval).astype(dt)
twaitval = np.where(threshold>cutoff, twait, twaitval).astype(dt)
fwaitval = np.where(threshold>cutoff, fwait, fwaitval).astype(dt)
afareval = np.where(threshold>cutoff, afare, afareval).astype(dt)
fmodeval = np.where(threshold>cutoff, fmode, fmodeval).astype(dt)
pmodeval = np.where(threshold>cutoff, pmode, pmodeval).astype(dt)
lmodeval = np.where(threshold>cutoff, lmode, lmodeval).astype(dt)


# ---------------------------------------------------------------
# Write final matrix values into files.
# ---------------------------------------------------------------
 # -- Arrays to write out
mtxlist = (invehval, trnfrval, twaitval, fwaitval, afareval, fmodeval, pmodeval, lmodeval, tcostval, autoval, threshold)
 # -- Files to write to
outmtx = (mfinvehi, mftrnfri, mftwaiti, mffwaiti, mfafarei, mffmodei, mfpmodei, mflmodei, mfacosti, mfautrni, mfratioi)
x = 0
outFl = open(stats, 'a')
outFl.write("\n\n {0:=^100}\n\n".format('=',))

for m in outmtx:
	mtxlist[x].tofile(outmtx[x])
	(fpath, fname) = os.path.split(outmtx[x])
	outFl.write("{0} Written Successfully.\n".format(fname, ))
	outFl.write("\t-- Minimum = {0:.4f}\n\t-- Maximum = {1:0.4f}\n\t-- Mean = {2:0.4f}\n\t-- Sum = {3:0.4f}\n\n".format(min(mtxlist[x]), max(mtxlist[x]), sum(mtxlist[x])/len(mtxlist[x]), sum(mtxlist[x])))
	x += 1

outFl.close()

print("-- TRANSIT SKIM MATRICES CREATED --")
