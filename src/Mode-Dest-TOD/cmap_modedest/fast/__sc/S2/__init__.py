import numba
import numpy as np

from .f_transit_approach_drivetime_PEAK_auto_dist_PEAK_SVETJXGBSVYEYM7OVBAQW2PX import transit_approach_drivetime_PEAK_auto_dist_PEAK_SVETJXGBSVYEYM7OVBAQW2PX
from .f_transit_approach_waittime_PEAK_auto_dist_PEAK_YHJNF2B3VEICSZL64Y2XLV4S import transit_approach_waittime_PEAK_auto_dist_PEAK_YHJNF2B3VEICSZL64Y2XLV4S
from .f_transit_approach_walktime_PEAK_auto_dist_PEAK_T52CC4SUBMFSLGDNMCSO7A7J import transit_approach_walktime_PEAK_auto_dist_PEAK_T52CC4SUBMFSLGDNMCSO7A7J
from .f_transit_ovtt_PEAK_auto_dist_PEAK_UIAXX4VIE5PP53PJMCSMEFOP import transit_ovtt_PEAK_auto_dist_PEAK_UIAXX4VIE5PP53PJMCSMEFOP
from .f_transit_approach_drivetime_OFFPEAK_auto_dist_OFFPEAK_U2242PIA3ZL4MJS726HCCG73 import transit_approach_drivetime_OFFPEAK_auto_dist_OFFPEAK_U2242PIA3ZL4MJS726HCCG73
from .f_transit_approach_waittime_OFFPEAK_auto_dist_OFFPEAK_22UIGWVENT7RTALQKKOHEPXM import transit_approach_waittime_OFFPEAK_auto_dist_OFFPEAK_22UIGWVENT7RTALQKKOHEPXM
from .f_transit_approach_walktime_OFFPEAK_auto_dist_OFFPEAK_ITAE2YOSBIY76DBTPIPT6QD2 import transit_approach_walktime_OFFPEAK_auto_dist_OFFPEAK_ITAE2YOSBIY76DBTPIPT6QD2
from .f_transit_ovtt_OFFPEAK_auto_dist_OFFPEAK_IY4UAVBMS2HPHNYZJLSMWG3V import transit_ovtt_OFFPEAK_auto_dist_OFFPEAK_IY4UAVBMS2HPHNYZJLSMWG3V
from .f_hard_sigmoid_transit_approach_walktime_PEAK__4_0__2_0__AJ26DYAVZX7MOBOG5SOJABST import hard_sigmoid_transit_approach_walktime_PEAK__4_0__2_0__AJ26DYAVZX7MOBOG5SOJABST
from .f_hard_sigmoid_transit_approach_walktime_OFFPEAK__4_0__2_0__FPYQ4GNCVUNJNUMJTUIBMLKO import hard_sigmoid_transit_approach_walktime_OFFPEAK__4_0__2_0__FPYQ4GNCVUNJNUMJTUIBMLKO
from .f_auto_parking_cost_HBWH import auto_parking_cost_HBWH
from .f_auto_parking_cost_HBWL import auto_parking_cost_HBWL
from .f_auto_parking_cost_HBO import auto_parking_cost_HBO
from .f_auto_parking_cost_HBS import auto_parking_cost_HBS
from .f_auto_parking_cost_NHB import auto_parking_cost_NHB
from .f_auto_parking_cost_HBOR import auto_parking_cost_HBOR
from .f_auto_parking_cost_NHBR import auto_parking_cost_NHBR
from .f_auto_parking_cost_NHBS import auto_parking_cost_NHBS
from .f_samp_wgt import samp_wgt
from .f_log_1_samp_wgt__PLSMOODNF77UOPHIVH6I2WBL import log_1_samp_wgt__PLSMOODNF77UOPHIVH6I2WBL
from .f_transit_avail_HBWH import transit_avail_HBWH
from .f_transit_avail_HBWL import transit_avail_HBWL
from .f_transit_avail_HBS import transit_avail_HBS
from .f_transit_avail_HBO import transit_avail_HBO
from .f_transit_avail_NHB import transit_avail_NHB
from .f_auto_avail_HBWH import auto_avail_HBWH
from .f__1_auto_avail_HBWH_RSSCN3EFDQ2CM4RPVZVAP7K3 import _1_auto_avail_HBWH_RSSCN3EFDQ2CM4RPVZVAP7K3
from .f__1_transit_avail_HBWH_NMEWQX35KIWZJMZ5IZZTZHOE import _1_transit_avail_HBWH_NMEWQX35KIWZJMZ5IZZTZHOE
from .f_auto_avail_HBWL import auto_avail_HBWL
from .f__1_auto_avail_HBWL_VWN3Q4WQRAVVJ5ZTYY4OHM63 import _1_auto_avail_HBWL_VWN3Q4WQRAVVJ5ZTYY4OHM63
from .f__1_transit_avail_HBWL_MPZ7IOPMILS7SZEG5T2D6HG2 import _1_transit_avail_HBWL_MPZ7IOPMILS7SZEG5T2D6HG2
from .f_auto_avail_HBS import auto_avail_HBS
from .f__1_auto_avail_HBS_JA6SY3LPJJDVZQF7IFNL4EWX import _1_auto_avail_HBS_JA6SY3LPJJDVZQF7IFNL4EWX
from .f__1_transit_avail_HBS_5CZV3HKDPQJDWZME2XMN7NW6 import _1_transit_avail_HBS_5CZV3HKDPQJDWZME2XMN7NW6
from .f_auto_avail_HBO import auto_avail_HBO
from .f__1_auto_avail_HBO_EKWWH33ZEA3VXLR4UYBVLRUX import _1_auto_avail_HBO_EKWWH33ZEA3VXLR4UYBVLRUX
from .f__1_transit_avail_HBO_K3RWOEPC3IXNAPRPQQJCHORY import _1_transit_avail_HBO_K3RWOEPC3IXNAPRPQQJCHORY
from .f_auto_avail_NHB import auto_avail_NHB
from .f__1_auto_avail_NHB_XHHWLREMVWOEBM6NUQF7EFH6 import _1_auto_avail_NHB_XHHWLREMVWOEBM6NUQF7EFH6
from .f__1_transit_avail_NHB_WN7OPNQOSVQ544PG5Q2ZGTNF import _1_transit_avail_NHB_WN7OPNQOSVQ544PG5Q2ZGTNF


@numba.jit(cache=True, parallel=True, error_model='numpy', boundscheck=False, nopython=True, fastmath=True)
def runner(argarray, inputarray, dtype=np.float64, min_shape_0=0):
    out_size = max(argarray.shape[0], min_shape_0)
    if out_size != argarray.shape[0]:
        result = np.zeros((out_size, 40), dtype=dtype)
    else:
        result = np.empty((out_size, 40), dtype=dtype)
    if out_size > 1000:
        for j in numba.prange(out_size):
            result[j, 0] = transit_approach_drivetime_PEAK_auto_dist_PEAK_SVETJXGBSVYEYM7OVBAQW2PX(argarray[j], inputarray[j], result[j], )
            result[j, 1] = transit_approach_waittime_PEAK_auto_dist_PEAK_YHJNF2B3VEICSZL64Y2XLV4S(argarray[j], inputarray[j], result[j], )
            result[j, 2] = transit_approach_walktime_PEAK_auto_dist_PEAK_T52CC4SUBMFSLGDNMCSO7A7J(argarray[j], inputarray[j], result[j], )
            result[j, 3] = transit_ovtt_PEAK_auto_dist_PEAK_UIAXX4VIE5PP53PJMCSMEFOP(argarray[j], inputarray[j], result[j], )
            result[j, 4] = transit_approach_drivetime_OFFPEAK_auto_dist_OFFPEAK_U2242PIA3ZL4MJS726HCCG73(argarray[j], inputarray[j], result[j], )
            result[j, 5] = transit_approach_waittime_OFFPEAK_auto_dist_OFFPEAK_22UIGWVENT7RTALQKKOHEPXM(argarray[j], inputarray[j], result[j], )
            result[j, 6] = transit_approach_walktime_OFFPEAK_auto_dist_OFFPEAK_ITAE2YOSBIY76DBTPIPT6QD2(argarray[j], inputarray[j], result[j], )
            result[j, 7] = transit_ovtt_OFFPEAK_auto_dist_OFFPEAK_IY4UAVBMS2HPHNYZJLSMWG3V(argarray[j], inputarray[j], result[j], )
            result[j, 8] = hard_sigmoid_transit_approach_walktime_PEAK__4_0__2_0__AJ26DYAVZX7MOBOG5SOJABST(argarray[j], inputarray[j], result[j], )
            result[j, 9] = hard_sigmoid_transit_approach_walktime_OFFPEAK__4_0__2_0__FPYQ4GNCVUNJNUMJTUIBMLKO(argarray[j], inputarray[j], result[j], )
            result[j, 10] = auto_parking_cost_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 11] = auto_parking_cost_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 12] = auto_parking_cost_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 13] = auto_parking_cost_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 14] = auto_parking_cost_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 15] = auto_parking_cost_HBOR(argarray[j], inputarray[j], result[j], )
            result[j, 16] = auto_parking_cost_NHBR(argarray[j], inputarray[j], result[j], )
            result[j, 17] = auto_parking_cost_NHBS(argarray[j], inputarray[j], result[j], )
            result[j, 18] = samp_wgt(argarray[j], inputarray[j], result[j], )
            result[j, 19] = log_1_samp_wgt__PLSMOODNF77UOPHIVH6I2WBL(argarray[j], inputarray[j], result[j], )
            result[j, 20] = transit_avail_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 21] = transit_avail_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 22] = transit_avail_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 23] = transit_avail_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 24] = transit_avail_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 25] = auto_avail_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 26] = _1_auto_avail_HBWH_RSSCN3EFDQ2CM4RPVZVAP7K3(argarray[j], inputarray[j], result[j], )
            result[j, 27] = _1_transit_avail_HBWH_NMEWQX35KIWZJMZ5IZZTZHOE(argarray[j], inputarray[j], result[j], )
            result[j, 28] = auto_avail_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 29] = _1_auto_avail_HBWL_VWN3Q4WQRAVVJ5ZTYY4OHM63(argarray[j], inputarray[j], result[j], )
            result[j, 30] = _1_transit_avail_HBWL_MPZ7IOPMILS7SZEG5T2D6HG2(argarray[j], inputarray[j], result[j], )
            result[j, 31] = auto_avail_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 32] = _1_auto_avail_HBS_JA6SY3LPJJDVZQF7IFNL4EWX(argarray[j], inputarray[j], result[j], )
            result[j, 33] = _1_transit_avail_HBS_5CZV3HKDPQJDWZME2XMN7NW6(argarray[j], inputarray[j], result[j], )
            result[j, 34] = auto_avail_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 35] = _1_auto_avail_HBO_EKWWH33ZEA3VXLR4UYBVLRUX(argarray[j], inputarray[j], result[j], )
            result[j, 36] = _1_transit_avail_HBO_K3RWOEPC3IXNAPRPQQJCHORY(argarray[j], inputarray[j], result[j], )
            result[j, 37] = auto_avail_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 38] = _1_auto_avail_NHB_XHHWLREMVWOEBM6NUQF7EFH6(argarray[j], inputarray[j], result[j], )
            result[j, 39] = _1_transit_avail_NHB_WN7OPNQOSVQ544PG5Q2ZGTNF(argarray[j], inputarray[j], result[j], )
    else:
        for j in range(out_size):
            result[j, 0] = transit_approach_drivetime_PEAK_auto_dist_PEAK_SVETJXGBSVYEYM7OVBAQW2PX(argarray[j], inputarray[j], result[j], )
            result[j, 1] = transit_approach_waittime_PEAK_auto_dist_PEAK_YHJNF2B3VEICSZL64Y2XLV4S(argarray[j], inputarray[j], result[j], )
            result[j, 2] = transit_approach_walktime_PEAK_auto_dist_PEAK_T52CC4SUBMFSLGDNMCSO7A7J(argarray[j], inputarray[j], result[j], )
            result[j, 3] = transit_ovtt_PEAK_auto_dist_PEAK_UIAXX4VIE5PP53PJMCSMEFOP(argarray[j], inputarray[j], result[j], )
            result[j, 4] = transit_approach_drivetime_OFFPEAK_auto_dist_OFFPEAK_U2242PIA3ZL4MJS726HCCG73(argarray[j], inputarray[j], result[j], )
            result[j, 5] = transit_approach_waittime_OFFPEAK_auto_dist_OFFPEAK_22UIGWVENT7RTALQKKOHEPXM(argarray[j], inputarray[j], result[j], )
            result[j, 6] = transit_approach_walktime_OFFPEAK_auto_dist_OFFPEAK_ITAE2YOSBIY76DBTPIPT6QD2(argarray[j], inputarray[j], result[j], )
            result[j, 7] = transit_ovtt_OFFPEAK_auto_dist_OFFPEAK_IY4UAVBMS2HPHNYZJLSMWG3V(argarray[j], inputarray[j], result[j], )
            result[j, 8] = hard_sigmoid_transit_approach_walktime_PEAK__4_0__2_0__AJ26DYAVZX7MOBOG5SOJABST(argarray[j], inputarray[j], result[j], )
            result[j, 9] = hard_sigmoid_transit_approach_walktime_OFFPEAK__4_0__2_0__FPYQ4GNCVUNJNUMJTUIBMLKO(argarray[j], inputarray[j], result[j], )
            result[j, 10] = auto_parking_cost_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 11] = auto_parking_cost_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 12] = auto_parking_cost_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 13] = auto_parking_cost_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 14] = auto_parking_cost_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 15] = auto_parking_cost_HBOR(argarray[j], inputarray[j], result[j], )
            result[j, 16] = auto_parking_cost_NHBR(argarray[j], inputarray[j], result[j], )
            result[j, 17] = auto_parking_cost_NHBS(argarray[j], inputarray[j], result[j], )
            result[j, 18] = samp_wgt(argarray[j], inputarray[j], result[j], )
            result[j, 19] = log_1_samp_wgt__PLSMOODNF77UOPHIVH6I2WBL(argarray[j], inputarray[j], result[j], )
            result[j, 20] = transit_avail_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 21] = transit_avail_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 22] = transit_avail_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 23] = transit_avail_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 24] = transit_avail_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 25] = auto_avail_HBWH(argarray[j], inputarray[j], result[j], )
            result[j, 26] = _1_auto_avail_HBWH_RSSCN3EFDQ2CM4RPVZVAP7K3(argarray[j], inputarray[j], result[j], )
            result[j, 27] = _1_transit_avail_HBWH_NMEWQX35KIWZJMZ5IZZTZHOE(argarray[j], inputarray[j], result[j], )
            result[j, 28] = auto_avail_HBWL(argarray[j], inputarray[j], result[j], )
            result[j, 29] = _1_auto_avail_HBWL_VWN3Q4WQRAVVJ5ZTYY4OHM63(argarray[j], inputarray[j], result[j], )
            result[j, 30] = _1_transit_avail_HBWL_MPZ7IOPMILS7SZEG5T2D6HG2(argarray[j], inputarray[j], result[j], )
            result[j, 31] = auto_avail_HBS(argarray[j], inputarray[j], result[j], )
            result[j, 32] = _1_auto_avail_HBS_JA6SY3LPJJDVZQF7IFNL4EWX(argarray[j], inputarray[j], result[j], )
            result[j, 33] = _1_transit_avail_HBS_5CZV3HKDPQJDWZME2XMN7NW6(argarray[j], inputarray[j], result[j], )
            result[j, 34] = auto_avail_HBO(argarray[j], inputarray[j], result[j], )
            result[j, 35] = _1_auto_avail_HBO_EKWWH33ZEA3VXLR4UYBVLRUX(argarray[j], inputarray[j], result[j], )
            result[j, 36] = _1_transit_avail_HBO_K3RWOEPC3IXNAPRPQQJCHORY(argarray[j], inputarray[j], result[j], )
            result[j, 37] = auto_avail_NHB(argarray[j], inputarray[j], result[j], )
            result[j, 38] = _1_auto_avail_NHB_XHHWLREMVWOEBM6NUQF7EFH6(argarray[j], inputarray[j], result[j], )
            result[j, 39] = _1_transit_avail_NHB_WN7OPNQOSVQ544PG5Q2ZGTNF(argarray[j], inputarray[j], result[j], )
    return result

def _map_index(dim_name, values):
    raise KeyError(dim_name)


def set_shared_data():
    global name_space
    name_space = {}


meta_match_names_idx = {}


import numpy as np
import numba as nb
import pandas as pd
import pyarrow as pa
import xarray as xr
import sharrow as sh
import inspect
import warnings
from contextlib import suppress




def load_raw(args, inputs, name_space, runner, dtype=None):
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', category=nb.NumbaExperimentalFeatureWarning)
        if len(args):
            assembled_args = np.stack(args).T
        else:
            assembled_args = None
        if len(inputs):
            assembled_inputs = np.stack(inputs).T
        else:
            assembled_inputs = np.empty([assembled_args.shape[0], 0], dtype=np.float32)
        if assembled_args is None:
            assembled_args = np.empty([assembled_inputs.shape[0], 0], dtype=np.float32)
        try:
            named_args = []
            arguments = []
            for arg in named_args:
                next_arg = name_space[arg]
                if isinstance(next_arg, xr.DataArray):
                    next_arg = next_arg.load()
                arguments.append(np.asarray(next_arg))
            if dtype is not None:
                return runner(assembled_args, assembled_inputs, *arguments, dtype=dtype)
            else:
                return runner(assembled_args, assembled_inputs, *arguments)
        except nb.TypingError:
            raise
        except KeyError as err:
            # raise the inner key error which is more helpful
            context = getattr(err, "__context__", None)
            if context:
                raise context
            else:
                raise err


def load(
        source,
        as_dataframe=False,
        as_table=False,
        dtype=None,
):
    if source is None:
        raise ValueError("no base table or source table")
    indexes = _get_indexes(source)[0]
    inputs = _get_inputs(source)
    result = load_raw(indexes, inputs, name_space, runner=runner, dtype=dtype)
    if as_dataframe:
        index = getattr(source, 'index', None)
        return pd.DataFrame(result, index=index, columns=function_names)
    elif as_table:
        return pa.table({k: result[:, n] for n, k in enumerate(function_names)})
    return result


def merge(source, dtype=None):
    """
    Merge the data created by this flow into the source.

    Parameters
    ----------
    source : Dataset or Table or DataFrame
    dtype : str or dtype
        The loaded data will be generated with this dtype.

    Returns
    -------
    merged : Dataset or Table or DataFrame
        The same data type as `source` is returned.
    """
    assert isinstance(source, (xr.Dataset, pa.Table, pd.DataFrame, sh.Table))
    new_cols = load(source, dtype=dtype)
    if isinstance(source, (pa.Table, sh.Table)):
        for n, k in enumerate(function_names):
            source = source.append_column(k, [new_cols[:, n]])
    else:
        for n, k in enumerate(function_names):
            source[k] = new_cols[:, n]
    return source


def _get_indexes(source_table, cache_dynamic_indexes=None):
    if cache_dynamic_indexes is None:
        cache_dynamic_indexes = True

    def geti(i):
        nonlocal source_table
        if i is not None:
            try:
                return np.asarray(source_table[i])
            except KeyError:
                if i[:10] == '__dynamic_':
                    if "@" in i:
                        i_complete = i
                        i, i_from = i.split("@", 1)
                    else:
                        i_complete = i
                        i_from = i[10:]
                    index_to_map = None
                    with suppress(KeyError):
                        index_to_map = source_table[i_from]
                    if index_to_map is None:
                        if isinstance(source_table, pd.DataFrame):
                            if i_from == "index" and source_table.index.names == [None]:
                                index_to_map = source_table.index
                            elif i_from in source_table.index.names:
                                index_to_map = source_table.index.get_level_values(i_from)
                    if index_to_map is None:
                        raise KeyError(i_complete)
                    indexed_vals = _map_index(i[10:], index_to_map)
                    if cache_dynamic_indexes:
                        if isinstance(source_table, (pa.Table)):
                            source_table = source_table.append_column(i_complete, [indexed_vals])
                        else:
                            source_table[i_complete] = indexed_vals
                    return indexed_vals
                else:
                    idx = getattr(source_table, 'index', None)
                    if idx is not None:
                        if i in idx.names:
                            return np.asarray(idx.get_level_values(i))
                        if i == 'index' and len(idx.names)==1:
                            return np.asarray(idx)
                    raise
        else:
            return np.arange(source_table.shape[0])
    index_keys = meta_match_names_idx.keys()
    return tuple(geti(i) for i in index_keys), source_table


def _get_inputs(source_table, dtype=np.float32):
    input_keys = input_name_positions.keys()
    return tuple(
        np.asarray(source_table[i], dtype=dtype)
        for i in input_keys
    )



input_name_positions = {'auto_dist_OFFPEAK': 0, 'auto_dist_PEAK': 1, 'log_attractions_HBO': 2, 'log_attractions_HBS': 3, 'log_attractions_HBWH': 4, 'log_attractions_HBWL': 5, 'log_attractions_NHB': 6, 'transit_approach_drivetime_OFFPEAK': 7, 'transit_approach_drivetime_PEAK': 8, 'transit_approach_waittime_OFFPEAK': 9, 'transit_approach_waittime_PEAK': 10, 'transit_approach_walktime_OFFPEAK': 11, 'transit_approach_walktime_PEAK': 12, 'transit_ivtt_OFFPEAK': 13, 'transit_ivtt_PEAK': 14, 'transit_ovtt_OFFPEAK': 15, 'transit_ovtt_PEAK': 16}
function_names = ['transit_approach_drivetime_PEAK/auto_dist_PEAK', 'transit_approach_waittime_PEAK/auto_dist_PEAK', 'transit_approach_walktime_PEAK/auto_dist_PEAK', 'transit_ovtt_PEAK/auto_dist_PEAK', 'transit_approach_drivetime_OFFPEAK/auto_dist_OFFPEAK', 'transit_approach_waittime_OFFPEAK/auto_dist_OFFPEAK', 'transit_approach_walktime_OFFPEAK/auto_dist_OFFPEAK', 'transit_ovtt_OFFPEAK/auto_dist_OFFPEAK', 'hard_sigmoid(transit_approach_walktime_PEAK, 4.0, 2.0)', 'hard_sigmoid(transit_approach_walktime_OFFPEAK, 4.0, 2.0)', 'auto_parking_cost_HBWH', 'auto_parking_cost_HBWL', 'auto_parking_cost_HBO', 'auto_parking_cost_HBS', 'auto_parking_cost_NHB', 'auto_parking_cost_HBOR', 'auto_parking_cost_NHBR', 'auto_parking_cost_NHBS', 'samp_wgt', 'log(1/samp_wgt)', 'transit_avail_HBWH', 'transit_avail_HBWL', 'transit_avail_HBS', 'transit_avail_HBO', 'transit_avail_NHB', 'auto_avail_HBWH', '1-auto_avail_HBWH', '1-transit_avail_HBWH', 'auto_avail_HBWL', '1-auto_avail_HBWL', '1-transit_avail_HBWL', 'auto_avail_HBS', '1-auto_avail_HBS', '1-transit_avail_HBS', 'auto_avail_HBO', '1-auto_avail_HBO', '1-transit_avail_HBO', 'auto_avail_NHB', '1-auto_avail_NHB', '1-transit_avail_NHB']

# Greetings, tinkerer!  The `defs_hash` included here is a safety 
# measure to prevent unknowing users creating a mess by modifying 
# the code in this module so that it no longer matches the expected 
# variable definitions. If you want to modify this code, you should 
# delete this hash to allow the code to run without any checks, but 
# you do so at your own risk. 
defs_hash = 'JLORNT6V2GU5GKGOGY25DSK2SA======'
