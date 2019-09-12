#############################################################################
# TRANSIT_SKIM_FINAL_MATRICES2.PY                                            #
#  Craig Heither, last revised 10-10-2013                                   #
#                                                                           #
#    This program performs the matrix convolution portion of the transit    #
#    skim procedures (much more efficiently than Emme).                     #
#                                                                           #
#    Written to work with Emme 4 structure.                                 #
#    Rev Bozic to work with serices 900 midday matrices 11/2/2017           #
#              for integration with global iterations                       #
#############################################################################

import os, array, string
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
# Declare empty arrays for storing values.
# ---------------------------------------------------------------
a0 = array('i')
a1 = array('i')
a2 = array('c')
auto = array('f')
fin_auto = array('f')
kzone = array('f')
tcost = array('f')
fin_tcost = array('f')
inveh = array('f')
fin_inveh = array('f')
trnfr = array('f')
fin_trnfr = array('f')
twait = array('f')
fin_twait = array('f')
fwait = array('f')
fin_fwait = array('f')
afare = array('f')
fin_afare = array('f')
fmode = array('f')
fin_fmode = array('f')
pmode = array('f')
fin_pmode = array('f')
lmode = array('f')
fin_lmode = array('f')
cghwy = array('f')
ratio = array('f')

# ---------------------------------------------------------------
# Open emmebank and read metadata.
# ---------------------------------------------------------------
with open(embank,'r+b') as f:
    # ## Read first 512 items: this is File 0 - metadata about files ##
    a0.fromfile(f,512)            ## grabs file offset, # of records, words/record & file type: 1-integer, 2-real, 3-text

    # ## Read Global parameters from File 1: array index starts with zero not 1, so is -1 from EMME documentation ##
    offst = a0[102] * 4           ## File 1 offset: elements * 4 bytes
    f.seek(offst, 0)
    a1.fromfile(f,80)
    mcent = a1[51]              ## maximum number of centroids defined in emmebank
    print "\tREADING EMMEBANK: {0} Centroids".format(str(mcent),)

    # ## Read Project title from File 2 ##
    offst = a0[104] * 4
    f.seek(offst, 0)
    a2.fromfile(f,80)
    proj = ''
    for z in range(0,80):
        proj = string.replace(proj+a2[z], "  ", "")

    # ## Write Emmebank parameters to File to review ##
    outFile = open(stats, 'w')
    outFile.write("Project: {0} \n".format(proj,))
    outFile.write("\n")
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
inputmat = (mfauto, mfkzone, mftcost, mfinveh, mftrnfr, mftwait, mffwait, mfafare, mffmode, mfpmode, mflmode, mfcghwy)
 #   -- Storage Arrays --
storage = (auto, kzone, tcost, inveh, trnfr, twait, fwait, afare, fmode, pmode, lmode, cghwy)
x = 0
for m in inputmat:
    with open(inputmat[x],'r+b') as m1:
        storage[x].fromfile(m1,mcent*mcent)                 ### Write matrix values into array
        x += 1


# ---------------------------------------------------------------
# Perform matrix convolution.
# ---------------------------------------------------------------
for x in range(0,mcent*mcent):
    pos = int(kzone[x])                                     ### intermediate zone value
    cghwyval = cghwy[x]                                     ### congested hwy generalized cost cell value

    if pos == 0:                                            ### Final matrix values are 0 if there is no intermediate zone
        autoval = 0                                         ### hwy time matrix value
        tcostval = 0                                        ### indexed transit gen. cost cell value
        invehval = 0                                        ### skimmed in-vehicle minutes cell value
        trnfrval = 0                                        ### skimmed transfer link minutes cell value
        twaitval = 0                                        ### skimmed total wait minutes cell value
        fwaitval = 0                                        ### skimmed first wait minutes cell value
        afareval = 0                                        ### skimmed final average fare cell value
        fmodeval = 0                                        ### skimmed first mode cell value
        pmodeval = 0                                        ### skimmed priority mode cell value
        lmodeval = 0                                        ### skimmed last mode cell value
    else:                                                   ### perform convolution
        leg1 = int(x/mcent)*mcent+(pos-1)                   ### first leg index value
        leg2 = (pos-1)*mcent+(x % mcent)                    ### second leg index value
        autoval = auto[leg1]
        tcostval = tcost[leg1]
        invehval = inveh[leg2]
        trnfrval = trnfr[leg2]
        twaitval = twait[leg2]
        fwaitval = fwait[leg2]
        afareval = afare[leg2]
        fmodeval = fmode[leg2]
        pmodeval = pmode[leg2]
        lmodeval = lmode[leg2]

    if cghwyval == 0:                                       ### Calculate ratio of indexed transit cost to auto only cost
        threshold = 0
    else:
        threshold = tcostval/cghwyval

    if threshold > cutoff:                                  ### use original matrix value (or zero) in final matrix if threshold exceeded
        autoval = 0
        tcostval = 0
        invehval = inveh[x]
        trnfrval = trnfr[x]
        twaitval = twait[x]
        fwaitval = fwait[x]
        afareval = afare[x]
        fmodeval = fmode[x]
        pmodeval = pmode[x]
        lmodeval = lmode[x]

    fin_auto.append(autoval)
    fin_tcost.append(tcostval)
    fin_inveh.append(invehval)
    fin_trnfr.append(trnfrval)
    fin_twait.append(twaitval)
    fin_fwait.append(fwaitval)
    fin_afare.append(afareval)
    fin_fmode.append(fmodeval)
    fin_pmode.append(pmodeval)
    fin_lmode.append(lmodeval)
    ratio.append(threshold)


# ---------------------------------------------------------------
# Write final matrix values into files.
# ---------------------------------------------------------------
 # -- Arrays to write out
mtxlist = (fin_inveh, fin_trnfr, fin_twait, fin_fwait, fin_afare, fin_fmode, fin_pmode, fin_lmode, fin_tcost, fin_auto, ratio)
 # -- Files to write to
outmtx = (mfinvehi, mftrnfri, mftwaiti, mffwaiti, mfafarei, mffmodei, mfpmodei, mflmodei, mfacosti, mfautrni, mfratioi)
x = 0
outFl = open(stats, 'a')
outFl.write("\n\n {0:=^100}\n\n".format('=',))

for m in outmtx:
    with open(outmtx[x],'r+b') as m1:
        mtxlist[x].tofile(m1)                                    ### Write matrix values
        (fpath, fname) = os.path.split(outmtx[x])
        outFl.write("{0} Written Successfully.\n".format(fname, ))
        outFl.write("\t-- Minimum = {0:.4f}\n\t-- Maximum = {1:0.4f}\n\t-- Mean = {2:0.4f}\n\t-- Sum = {3:0.4f}\n\n".format(min(mtxlist[x]), max(mtxlist[x]), sum(mtxlist[x])/len(mtxlist[x]), sum(mtxlist[x])))
        x += 1

outFl.close()
print "-- TRANSIT SKIM MATRICES CREATED --"
