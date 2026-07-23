import os
import numpy as np
import xarray as xr
# from array import *
from .addict import Dict

import logging
log = logging.getLogger("CMAP")

def skim_convol(
		dh,
		peak=True,
		cutoff=0.4,
		zone_types=None,
):
	"""
	Perform matrix convolution of transit skims.

	This function is based on the original TRANSIT_SKIM_FINAL_MATRICES1.PY

	Parameters
	----------
	dh : DataHandler
	peak : bool, default True
		Whether to process the peak or offpeak skims.
	cutoff : float, default 0.4
		Park-and-ride skim routing is ignored if the transit generalized
		cost exceeds this fraction of the congested drive-to-destination
		cost.  This prevents, for example, transit paths where the traveler
		drives a long distance to a park-and-ride lot, then boards transit
		for a short hop to the final destination.
		New for the 2020 model update: When the final destination is in the
		Chicago CBD, this cutoff is ignored when there is no valid non-PnR
		path, so the traveler is permitted to drive unlimited distances to
		access outlying transit (generally outlying Metra stations).
	zone_types : array-like, optional
		The zone type codes for zones in the model, used to filter for
		Chicago CBD zones to apply the new cutoff rules.  If not given, the
		zone types are loaded from the package-default M01 HW file (see
		m01_handler for details).

	"""
	report_dir = dh.filenames.emme_database_dir / "report"
	os.makedirs(report_dir, exist_ok=True)

	if peak:
		#   -- Input Matrix Numbers --
		inputmtx = Dict(
			auto =44,    ##0 AM peak hwy time matrix (mf44)
			fmode=803,   ##1 skimmed first mode (mf803)
			pmode=804,   ##2 skimmed priority mode (mf804)
			lmode=805,   ##3 skimmed last mode (mf805)
			inveh=808,   ##4 skimmed in-vehicle minutes (mf808)
			trnfr=809,   ##5 skimmed transfer link minutes (mf809)
			twait=810,   ##6 skimmed total wait minutes (mf810)
			fwait=811,   ##7 skimmed first wait minutes (mf811)
			afare=818,   ##8 skimmed final average fare (mf818)
			cghwy=819,   ##9 congested hwy generalized cost matrix (mf819)
			tcost=820,   #10 indexed transit generalized cost (mf820)
			kzone=821,   #11 intermediate zone matrix (mf821)
		)
		#   -- Output Matrix Numbers --
		outputmtx = Dict(
			mfinvehi=822,    ###0 indexed in-vehicle minutes (mf822)
			mftrnfri=823,    ###1 indexed walk transfer minutes (mf823)
			mftwaiti=824,    ###2 indexed total wait minutes (mf824)
			mffwaiti=825,    ###3 indexed first wait minutes (mf825)
			mfafarei=828,    ###4 indexed final average fare (mf828)
			mffmodei=829,    ###5 indexed first mode (mf829)
			mfpmodei=830,    ###6 indexed priority mode (mf830)
			mflmodei=831,    ###7 indexed last mode (mf831)
			mfacosti=832,    ###8 indexed auto generalized cost (mf832)
			mfautrni=833,    ###9 indexed auto min. to transit (mf833)
			mfratioi=834,    ##10 indexed transit/auto only (mf834)
		)
	else:
		#   -- Input Matrix Numbers --
		inputmtx = Dict(
			auto =46,    ### midday hwy time matrix (mf46)
			fmode=903,   ### skimmed first mode (mf903)
			pmode=904,   ### skimmed priority mode (mf904)
			lmode=905,   ### skimmed last mode (mf905)
			inveh=908,   ### skimmed in-vehicle minutes (mf908)
			trnfr=909,   ### skimmed transfer link minutes (mf909)
			twait=910,   ### skimmed total wait minutes (mf910)
			fwait=911,   ### skimmed first wait minutes (mf911)
			afare=918,   ### skimmed final average fare (mf918)
			cghwy=919,   ### congested hwy generalized cost matrix (mf919)
			tcost=920,   ### indexed transit generalized cost (mf920)
			kzone=921,   ### intermediate zone matrix (mf921)
		)
		#   -- Output Matrix Numbers --
		outputmtx = Dict(
			mfinvehi=922,    ### indexed in-vehicle minutes (mf922)
			mftrnfri=923,    ### indexed walk transfer minutes (mf923)
			mftwaiti=924,    ### indexed total wait minutes (mf924)
			mffwaiti=925,    ### indexed first wait minutes (mf925)
			mfafarei=928,    ### indexed final average fare (mf928)
			mffmodei=929,    ### indexed first mode (mf929)
			mfpmodei=930,    ### indexed priority mode (mf930)
			mflmodei=931,    ### indexed last mode (mf931)
			mfacosti=932,    ### indexed auto generalized cost (mf932)
			mfautrni=933,    ### indexed auto min. to transit (mf933)
			mfratioi=934,    ### indexed transit/auto only (mf934)
		)

	#   -- Input Matrices --
	# mfauto  = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[0]) + ".emx"  )
	# mffmode = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[1]) + ".emx"  )
	# mfpmode = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[2]) + ".emx"  )
	# mflmode = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[3]) + ".emx"  )
	# mfinveh = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[4]) + ".emx"  )
	# mftrnfr = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[5]) + ".emx"  )
	# mftwait = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[6]) + ".emx"  )
	# mffwait = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[7]) + ".emx"  )
	# mfafare = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[8]) + ".emx"  )
	# mfcghwy = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[9]) + ".emx"  )
	# mftcost = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[10]) + ".emx" )
	# mfkzone = os.path.join(emmemat_in_dir, "mf" + str(inputmtx[11]) + ".emx" )
	# #   -- Output Matrices --
	# if emmemat_out_dir:
	# 	mfinvehi = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[0]) + ".emx"  )
	# 	mftrnfri = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[1]) + ".emx"  )
	# 	mftwaiti = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[2]) + ".emx"  )
	# 	mffwaiti = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[3]) + ".emx"  )
	# 	mfafarei = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[4]) + ".emx"  )
	# 	mffmodei = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[5]) + ".emx"  )
	# 	mfpmodei = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[6]) + ".emx"  )
	# 	mflmodei = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[7]) + ".emx"  )
	# 	mfacosti = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[8]) + ".emx"  )
	# 	mfautrni = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[9]) + ".emx"  )
	# 	mfratioi = os.path.join(emmemat_out_dir, "mf" + str(outputmtx[10]) + ".emx" )

	#   -- Others --
	stats = os.path.join(report_dir, f"transit_skim_stats{8 if peak else 9}.txt")

	if os.path.exists(stats):
		os.remove(stats)

	# ---------------------------------------------------------------
	# Store matrix values in arrays.
	# ---------------------------------------------------------------
	#   -- Input Matrices --
	auto  = dh.skims.raw[f"mf{inputmtx.auto }"].values.astype(np.float32).reshape(-1) # np.fromfile(mfauto, dtype='f4') ## -- float, 4 bytes
	kzone = dh.skims.raw[f"mf{inputmtx.kzone}"].values.astype(np.float32).reshape(-1)
	tcost = dh.skims.raw[f"mf{inputmtx.tcost}"].values.astype(np.float32).reshape(-1)
	inveh = dh.skims.raw[f"mf{inputmtx.inveh}"].values.astype(np.float32).reshape(-1)
	trnfr = dh.skims.raw[f"mf{inputmtx.trnfr}"].values.astype(np.float32).reshape(-1)
	twait = dh.skims.raw[f"mf{inputmtx.twait}"].values.astype(np.float32).reshape(-1)
	fwait = dh.skims.raw[f"mf{inputmtx.fwait}"].values.astype(np.float32).reshape(-1)
	afare = dh.skims.raw[f"mf{inputmtx.afare}"].values.astype(np.float32).reshape(-1)
	fmode = dh.skims.raw[f"mf{inputmtx.fmode}"].values.astype(np.float32).reshape(-1)
	pmode = dh.skims.raw[f"mf{inputmtx.pmode}"].values.astype(np.float32).reshape(-1)
	lmode = dh.skims.raw[f"mf{inputmtx.lmode}"].values.astype(np.float32).reshape(-1)
	cghwy = dh.skims.raw[f"mf{inputmtx.cghwy}"].values.astype(np.float32).reshape(-1)

	mcent = int(np.sqrt(auto.shape[0]))

	if zone_types is None:
		zone_types = dh.m01.zone_type.values
	assert zone_types.size <= mcent
	cbd_zone_indexes = np.where(zone_types == 1)[0]
	dzone_inside_cbd = np.zeros([mcent,mcent], dtype=bool)
	dzone_inside_cbd[:,cbd_zone_indexes] = True
	dzone_inside_cbd = dzone_inside_cbd.reshape(-1)

	## -- create leg1 (p-k) indices
	indxloc = np.arange(mcent*mcent)							## -- array of consecutive numbers representing element index values
	leg1pt1 = np.floor_divide(indxloc,mcent) * mcent					## -- portion of element index defining origin zone (division results in integer value)
	leg1indx = np.add(leg1pt1,kzone.astype('i4')-1,dtype='i4')				## -- add portion of element index defining destination zone
	log.info(
		f"Kzone 1-1: {kzone[0]}, Index 1-1: {leg1indx[0]}, "
		f"Kzone 121-2: {kzone[437882]}, Index 121-2: {leg1indx[437882]} \n"
	)

	## -- create leg2 (k-q) indices
	leg2pt1 = np.multiply(kzone.astype('i4')-1,mcent)
	leg2pt2 = np.mod(indxloc,mcent)
	leg2indx = np.add(leg2pt1,leg2pt2,dtype='i4')
	log.info(
		f"Kzone 1-1: {kzone[0]}, Index 1-1: {leg2indx[0]}, "
		f"Kzone 121-2: {kzone[437882]}, Index 121-2: {leg2indx[437882]} \n"
	)

	# ---------------------------------------------------------------
	# Create indexed matrices.
	# ---------------------------------------------------------------
	log.debug(f"Create indexed matrices")
	autoval  = np.where(kzone>0, auto [leg1indx], kzone)			    ## -- hwy time matrix
	tcostval = np.where(kzone>0, tcost[leg1indx], kzone)				## -- indexed transit generalized cost
	invehval = np.where(kzone>0, inveh[leg2indx], kzone)				## -- skimmed in-vehicle minutes
	trnfrval = np.where(kzone>0, trnfr[leg2indx], kzone)				## -- skimmed transfer link minutes
	twaitval = np.where(kzone>0, twait[leg2indx], kzone)				## -- skimmed total wait minutes
	fwaitval = np.where(kzone>0, fwait[leg2indx], kzone)				## -- skimmed first wait minutes
	afareval = np.where(kzone>0, afare[leg2indx], kzone)				## -- skimmed final average fare
	fmodeval = np.where(kzone>0, fmode[leg2indx], kzone)				## -- skimmed first mode
	pmodeval = np.where(kzone>0, pmode[leg2indx], kzone)				## -- skimmed priority mode
	lmodeval = np.where(kzone>0, lmode[leg2indx], kzone)				## -- skimmed last mode
	threshold= np.where(cghwy>0, np.divide(tcostval,cghwy), cghwy)		## -- ratio of indexed transit cost to auto only cost

	log.debug(f"Swap original matrix value back in if the threshold exceeds the cutoff value")
	# -- Swap original matrix value back in if the threshold exceeds the cutoff value
	swap = (threshold>cutoff)
	# -- NEW 12/2020: but keep the park-and-ride indexed values if the destination zone
	#                 is inside the CBD and the original values are invalid
	prevent_swap = (dzone_inside_cbd) & (inveh > 999)
	swap &= ~prevent_swap
	autoval = np.where(swap, 0, autoval)
	tcostval = np.where(swap, 0, tcostval)
	invehval = np.where(swap, inveh, invehval)
	trnfrval = np.where(swap, trnfr, trnfrval)
	twaitval = np.where(swap, twait, twaitval)
	fwaitval = np.where(swap, fwait, fwaitval)
	afareval = np.where(swap, afare, afareval)
	fmodeval = np.where(swap, fmode, fmodeval)
	pmodeval = np.where(swap, pmode, pmodeval)
	lmodeval = np.where(swap, lmode, lmodeval)


	# ---------------------------------------------------------------
	# Write final matrix values into files.
	# ---------------------------------------------------------------
	# -- Arrays to write out
	mtxlist = {
		outputmtx.mfinvehi: invehval,   ###0
		outputmtx.mftrnfri: trnfrval,   ###1
		outputmtx.mftwaiti: twaitval,   ###2
		outputmtx.mffwaiti: fwaitval,   ###3
		outputmtx.mfafarei: afareval,   ###4
		outputmtx.mffmodei: fmodeval,   ###5
		outputmtx.mfpmodei: pmodeval,   ###6
		outputmtx.mflmodei: lmodeval,   ###7
		outputmtx.mfacosti: tcostval,   ###8
		outputmtx.mfautrni: autoval,    ###9
		outputmtx.mfratioi: threshold,  ##10
	}
	with open(stats, 'a') as outFl:
		for mf_num, vals in mtxlist.items():
			arr = dh.skims.raw[f"mf{mf_num}"] = xr.DataArray(
				vals.reshape([mcent, mcent]),
				dims=['otaz', 'dtaz'],
				coords=dh.skims.raw.indexes,
			)
			log.info(f"mf{mf_num} computed")
			outFl.write(f"mf{mf_num} computed.\n")
			# arr = arr.values.reshape(-1)
			# arr_statistics = (
			# 	f"\t-- Minimum = {min(arr):.4f}\n"
			# 	f"\t-- Maximum = {max(arr):0.4f}\n"
			# 	f"\t-- Mean = {sum(arr) / len(arr):0.4f}\n"
			# 	f"\t-- Sum = {sum(arr):0.4f}\n\n"
			# )
			# outFl.write(arr_statistics)
			# log.info(arr_statistics)

	revised_skims = dh.skims.raw[[f"mf{mf_num}" for mf_num in mtxlist.keys()]]
	revised_skims.to_zarr(
		dh.skims.filename
		if os.path.splitext(dh.skims.filename) == ".zarr" else
		dh.skims.filename.with_suffix(".zarr"),
		mode='a',
	)


	log.info("-- TRANSIT SKIM MATRICES CREATED --")
