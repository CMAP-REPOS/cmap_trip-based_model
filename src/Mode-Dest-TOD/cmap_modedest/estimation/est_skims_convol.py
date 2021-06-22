import os

from .est_logging import L
from .est_data import dh
filenames = dh.filenames

L("###### Skims Convolution ######")
from cmap_modedest.transit_skim_convolution import skim_convol
emmemat_in_dir = os.path.dirname(filenames.mf822)

filenames.pk_transit_skims = filenames.cache_dir / "peak.omx"
if not os.path.exists(filenames.pk_transit_skims):
    skim_convol(
        dh,
        peak=True,
    )
else:
    L("# using cached peak skims")

filenames.op_transit_skims = filenames.cache_dir / "offpeak.omx"
if not os.path.exists(filenames.op_transit_skims):
    skim_convol(
        dh,
        peak=False,
    )
else:
    L("# using cached offpeak skims")


