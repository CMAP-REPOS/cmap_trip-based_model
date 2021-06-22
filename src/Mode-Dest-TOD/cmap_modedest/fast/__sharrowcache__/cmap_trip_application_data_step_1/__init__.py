import numba
import numpy as np

from .f_o_zone____dtaz_HXCH5HUPIDPV3EWONWFXBZHM import o_zone____dtaz_HXCH5HUPIDPV3EWONWFXBZHM
from .f_transit_ivtt_PEAK import transit_ivtt_PEAK
from .f_transit_ivtt_OFFPEAK import transit_ivtt_OFFPEAK
from .f_transit_ovtt_PEAK import transit_ovtt_PEAK
from .f_transit_ovtt_OFFPEAK import transit_ovtt_OFFPEAK
from .f_transit_fare_PEAK import transit_fare_PEAK
from .f_transit_fare_OFFPEAK import transit_fare_OFFPEAK
from .f_piece_transit_ivtt_OFFPEAK__None__20__54N5BOFSYCN6SMRBKXM4V4TW import piece_transit_ivtt_OFFPEAK__None__20__54N5BOFSYCN6SMRBKXM4V4TW
from .f_piece_transit_ivtt_OFFPEAK__20__None__2SYXPZCTOY3PKAO2L4ALLHLI import piece_transit_ivtt_OFFPEAK__20__None__2SYXPZCTOY3PKAO2L4ALLHLI
from .f_autopropensity import autopropensity
from .f_auto_time_PEAK import auto_time_PEAK
from .f_auto_dist_PEAK import auto_dist_PEAK
from .f_auto_time_OFFPEAK import auto_time_OFFPEAK
from .f_auto_dist_OFFPEAK import auto_dist_OFFPEAK
from .f_auto_opcost_PEAK import auto_opcost_PEAK
from .f_auto_opcost_hov_PEAK import auto_opcost_hov_PEAK
from .f_auto_opcost_OFFPEAK import auto_opcost_OFFPEAK
from .f_auto_toll_hiinc_PEAK import auto_toll_hiinc_PEAK
from .f_auto_toll_loinc_PEAK import auto_toll_loinc_PEAK
from .f_auto_toll_hov_hiinc_PEAK import auto_toll_hov_hiinc_PEAK
from .f_auto_toll_hov_loinc_PEAK import auto_toll_hov_loinc_PEAK
from .f_auto_toll_OFFPEAK import auto_toll_OFFPEAK
from .f_piece_auto_dist_PEAK_None_0_5__NZ6HG4EJYEENT2OAOBDF3W5W import piece_auto_dist_PEAK_None_0_5__NZ6HG4EJYEENT2OAOBDF3W5W
from .f_piece_auto_dist_PEAK_0_5_1_0__4K5ZPM55FQJVFMWKUPZIQ2ET import piece_auto_dist_PEAK_0_5_1_0__4K5ZPM55FQJVFMWKUPZIQ2ET
from .f_piece_auto_dist_PEAK_1_0_None__FMBHM5APW4TIH6B5EKSKSXYD import piece_auto_dist_PEAK_1_0_None__FMBHM5APW4TIH6B5EKSKSXYD
from .f_piece_auto_dist_PEAK_None_5__7D72ALZRBNSP634JYUY7IV5P import piece_auto_dist_PEAK_None_5__7D72ALZRBNSP634JYUY7IV5P
from .f_piece_auto_dist_PEAK_5_10__DSXTFACXD5HSWU7YKUPQEGAK import piece_auto_dist_PEAK_5_10__DSXTFACXD5HSWU7YKUPQEGAK
from .f_piece_auto_dist_PEAK_10_None__L3GKJNRUTTFCDUYV65K7KFQD import piece_auto_dist_PEAK_10_None__L3GKJNRUTTFCDUYV65K7KFQD
from .f_piece_auto_dist_OFFPEAK_None_0_5__537AWC5AWFXLM3TRB3PPSGYZ import piece_auto_dist_OFFPEAK_None_0_5__537AWC5AWFXLM3TRB3PPSGYZ
from .f_piece_auto_dist_OFFPEAK_0_5_1_0__HZLL3YOGUR763JRQHG7SM6TX import piece_auto_dist_OFFPEAK_0_5_1_0__HZLL3YOGUR763JRQHG7SM6TX
from .f_piece_auto_dist_OFFPEAK_1_0_None__FBK6GF2KEWWDN7AL3ENVH3VS import piece_auto_dist_OFFPEAK_1_0_None__FBK6GF2KEWWDN7AL3ENVH3VS
from .f_piece_auto_dist_OFFPEAK_None_5__YWXA4HRZAVEZKC7EV5M2AX3B import piece_auto_dist_OFFPEAK_None_5__YWXA4HRZAVEZKC7EV5M2AX3B
from .f_piece_auto_dist_OFFPEAK_5_10__5RQTWWGPPCPP6EQU6ZV5MX2R import piece_auto_dist_OFFPEAK_5_10__5RQTWWGPPCPP6EQU6ZV5MX2R
from .f_piece_auto_dist_OFFPEAK_10_None__6EE7X5PWAQKBS6WQUONWVI63 import piece_auto_dist_OFFPEAK_10_None__6EE7X5PWAQKBS6WQUONWVI63
from .f_taxi_fare_PEAK import taxi_fare_PEAK
from .f_taxi_fare_OFFPEAK import taxi_fare_OFFPEAK
from .f_tnc_solo_fare_PEAK import tnc_solo_fare_PEAK
from .f_tnc_solo_fare_OFFPEAK import tnc_solo_fare_OFFPEAK
from .f_tnc_pool_fare_PEAK import tnc_pool_fare_PEAK
from .f_tnc_pool_fare_OFFPEAK import tnc_pool_fare_OFFPEAK
from .f_taxi_wait_time_PEAK import taxi_wait_time_PEAK
from .f_taxi_wait_time_OFFPEAK import taxi_wait_time_OFFPEAK
from .f_tnc_solo_wait_time_PEAK import tnc_solo_wait_time_PEAK
from .f_tnc_solo_wait_time_OFFPEAK import tnc_solo_wait_time_OFFPEAK
from .f_tnc_pool_wait_time_PEAK import tnc_pool_wait_time_PEAK
from .f_tnc_pool_wait_time_OFFPEAK import tnc_pool_wait_time_OFFPEAK
from .f_taxi_wait_time_PEAK_auto_dist_PEAK_CBYSKDURSGAJ4RHX2ASS76Q2 import taxi_wait_time_PEAK_auto_dist_PEAK_CBYSKDURSGAJ4RHX2ASS76Q2
from .f_taxi_wait_time_OFFPEAK_auto_dist_OFFPEAK_HC62HOLRZIWJJDAOIFX5XHV6 import taxi_wait_time_OFFPEAK_auto_dist_OFFPEAK_HC62HOLRZIWJJDAOIFX5XHV6
from .f_tnc_solo_wait_time_PEAK_auto_dist_PEAK_2EZZEATSM4EAHST74DMGSFSS import tnc_solo_wait_time_PEAK_auto_dist_PEAK_2EZZEATSM4EAHST74DMGSFSS
from .f_tnc_solo_wait_time_OFFPEAK_auto_dist_OFFPEAK_CA5RZWRWZJWFNZATH2HHD3TO import tnc_solo_wait_time_OFFPEAK_auto_dist_OFFPEAK_CA5RZWRWZJWFNZATH2HHD3TO
from .f_tnc_pool_wait_time_PEAK_auto_dist_PEAK_NHA453KUTMUKTUCSMA2XQQZV import tnc_pool_wait_time_PEAK_auto_dist_PEAK_NHA453KUTMUKTUCSMA2XQQZV
from .f_tnc_pool_wait_time_OFFPEAK_auto_dist_OFFPEAK_FNBOP5IL57Q6SQM4VB7EXQBB import tnc_pool_wait_time_OFFPEAK_auto_dist_OFFPEAK_FNBOP5IL57Q6SQM4VB7EXQBB
from .f_fmax_ozone_areatype__areatype___1_OWXSNW5EFKHSRO2IUCB6DHTR import fmax_ozone_areatype__areatype___1_OWXSNW5EFKHSRO2IUCB6DHTR
from .f_fmax_ozone_areatype__areatype___2_FIT25FYRGELBEONPX2MTPZFN import fmax_ozone_areatype__areatype___2_FIT25FYRGELBEONPX2MTPZFN
from .f_fmax_ozone_areatype__areatype___3_LIPIKNXA6ZB3PUNRJBVIHATL import fmax_ozone_areatype__areatype___3_LIPIKNXA6ZB3PUNRJBVIHATL
from .f_fmax_ozone_areatype__areatype___4_J3D56DXM2MR6NEMIQ6ALSFID import fmax_ozone_areatype__areatype___4_J3D56DXM2MR6NEMIQ6ALSFID
from .f_fmin_ozone_areatype__areatype___1_OTNZJFEFZT7N3JG6YKEUWGTF import fmin_ozone_areatype__areatype___1_OTNZJFEFZT7N3JG6YKEUWGTF
from .f_fmin_ozone_areatype__areatype___2_FY4D7BMCO2N4QRIA6QIIWTZI import fmin_ozone_areatype__areatype___2_FY4D7BMCO2N4QRIA6QIIWTZI
from .f_fmin_ozone_areatype__areatype___3_34BD5WGYZOPLALWPB2MCWKA7 import fmin_ozone_areatype__areatype___3_34BD5WGYZOPLALWPB2MCWKA7
from .f_fmin_ozone_areatype__areatype___4_64S6AODYPVZP57LTAC522OHX import fmin_ozone_areatype__areatype___4_64S6AODYPVZP57LTAC522OHX
from .f_log_attractions_HBWH import log_attractions_HBWH
from .f_log_attractions_HBWL import log_attractions_HBWL
from .f_log_attractions_HBS import log_attractions_HBS
from .f_log_attractions_HBO import log_attractions_HBO
from .f_log_attractions_NHB import log_attractions_NHB
from .f_log_attractions_HBWH____666_GW3RKVJQOVET3XE7MYE2MA7V import log_attractions_HBWH____666_GW3RKVJQOVET3XE7MYE2MA7V
from .f_log_attractions_HBWL____666_IQGLCTAX7EZOXKEFCIBK33BD import log_attractions_HBWL____666_IQGLCTAX7EZOXKEFCIBK33BD
from .f_log_attractions_HBS____666_L3ZMVRT365KFSBLEDVABFEGL import log_attractions_HBS____666_L3ZMVRT365KFSBLEDVABFEGL
from .f_log_attractions_HBO____666_5VORECHSLO2A6MKP5I3SGJVQ import log_attractions_HBO____666_5VORECHSLO2A6MKP5I3SGJVQ
from .f_log_attractions_NHB____666_3GFB7KMY3JN5TSJE4MRRR2HM import log_attractions_NHB____666_3GFB7KMY3JN5TSJE4MRRR2HM


@numba.njit(cache=True, parallel=True, error_model='numpy', boundscheck=False)
def runner(argarray, inputarray, __attractions__HBO, __attractions__HBS, __attractions__HBWH, __attractions__HBWL, __attractions__NHB, __auto_skims__am_dist, __auto_skims__am_opcost, __auto_skims__am_opcost_hov, __auto_skims__am_time, __auto_skims__am_toll_hiinc, __auto_skims__am_toll_hov_hiinc, __auto_skims__am_toll_hov_loinc, __auto_skims__am_toll_loinc, __auto_skims__md_dist, __auto_skims__md_opcost, __auto_skims__md_time, __auto_skims__md_toll, __d_autopropensity__autopropensity, __dzone__zone_type, __ozone__taxi_wait_op, __ozone__taxi_wait_pk, __ozone__tnc_pool_wait_op, __ozone__tnc_pool_wait_pk, __ozone__tnc_solo_wait_op, __ozone__tnc_solo_wait_pk, __ozone__zone_type, __transit_op__fare, __transit_op__ivtt, __transit_op__ovtt, __transit_pk__fare, __transit_pk__ivtt, __transit_pk__ovtt, dtype=np.float64, min_shape_0=0):
    out_size = max(argarray.shape[0], min_shape_0)
    if out_size != argarray.shape[0]:
        result = np.zeros((out_size, 70), dtype=dtype)
    else:
        result = np.empty((out_size, 70), dtype=dtype)
    if out_size > 1000:
        for j in numba.prange(out_size):
            result[j, 0] = o_zone____dtaz_HXCH5HUPIDPV3EWONWFXBZHM(argarray[j], inputarray[j], result[j], )
            result[j, 1] = transit_ivtt_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__ivtt)
            result[j, 2] = transit_ivtt_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__ivtt)
            result[j, 3] = transit_ovtt_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__ovtt)
            result[j, 4] = transit_ovtt_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__ovtt)
            result[j, 5] = transit_fare_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__fare)
            result[j, 6] = transit_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__fare)
            result[j, 7] = piece_transit_ivtt_OFFPEAK__None__20__54N5BOFSYCN6SMRBKXM4V4TW(argarray[j], inputarray[j], result[j], )
            result[j, 8] = piece_transit_ivtt_OFFPEAK__20__None__2SYXPZCTOY3PKAO2L4ALLHLI(argarray[j], inputarray[j], result[j], )
            result[j, 9] = autopropensity(argarray[j], inputarray[j], result[j], __d_autopropensity__autopropensity)
            result[j, 10] = auto_time_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_time)
            result[j, 11] = auto_dist_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 12] = auto_time_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_time)
            result[j, 13] = auto_dist_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 14] = auto_opcost_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_opcost)
            result[j, 15] = auto_opcost_hov_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_opcost_hov)
            result[j, 16] = auto_opcost_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_opcost)
            result[j, 17] = auto_toll_hiinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hiinc)
            result[j, 18] = auto_toll_loinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_loinc)
            result[j, 19] = auto_toll_hov_hiinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hov_hiinc)
            result[j, 20] = auto_toll_hov_loinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hov_loinc)
            result[j, 21] = auto_toll_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_toll)
            result[j, 22] = piece_auto_dist_PEAK_None_0_5__NZ6HG4EJYEENT2OAOBDF3W5W(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 23] = piece_auto_dist_PEAK_0_5_1_0__4K5ZPM55FQJVFMWKUPZIQ2ET(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 24] = piece_auto_dist_PEAK_1_0_None__FMBHM5APW4TIH6B5EKSKSXYD(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 25] = piece_auto_dist_PEAK_None_5__7D72ALZRBNSP634JYUY7IV5P(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 26] = piece_auto_dist_PEAK_5_10__DSXTFACXD5HSWU7YKUPQEGAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 27] = piece_auto_dist_PEAK_10_None__L3GKJNRUTTFCDUYV65K7KFQD(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 28] = piece_auto_dist_OFFPEAK_None_0_5__537AWC5AWFXLM3TRB3PPSGYZ(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 29] = piece_auto_dist_OFFPEAK_0_5_1_0__HZLL3YOGUR763JRQHG7SM6TX(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 30] = piece_auto_dist_OFFPEAK_1_0_None__FBK6GF2KEWWDN7AL3ENVH3VS(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 31] = piece_auto_dist_OFFPEAK_None_5__YWXA4HRZAVEZKC7EV5M2AX3B(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 32] = piece_auto_dist_OFFPEAK_5_10__5RQTWWGPPCPP6EQU6ZV5MX2R(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 33] = piece_auto_dist_OFFPEAK_10_None__6EE7X5PWAQKBS6WQUONWVI63(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 34] = taxi_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 35] = taxi_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 36] = tnc_solo_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 37] = tnc_solo_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 38] = tnc_pool_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 39] = tnc_pool_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 40] = taxi_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__taxi_wait_pk)
            result[j, 41] = taxi_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__taxi_wait_op)
            result[j, 42] = tnc_solo_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_solo_wait_pk)
            result[j, 43] = tnc_solo_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_solo_wait_op)
            result[j, 44] = tnc_pool_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_pool_wait_pk)
            result[j, 45] = tnc_pool_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_pool_wait_op)
            result[j, 46] = taxi_wait_time_PEAK_auto_dist_PEAK_CBYSKDURSGAJ4RHX2ASS76Q2(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__taxi_wait_pk)
            result[j, 47] = taxi_wait_time_OFFPEAK_auto_dist_OFFPEAK_HC62HOLRZIWJJDAOIFX5XHV6(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__taxi_wait_op)
            result[j, 48] = tnc_solo_wait_time_PEAK_auto_dist_PEAK_2EZZEATSM4EAHST74DMGSFSS(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__tnc_solo_wait_pk)
            result[j, 49] = tnc_solo_wait_time_OFFPEAK_auto_dist_OFFPEAK_CA5RZWRWZJWFNZATH2HHD3TO(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__tnc_solo_wait_op)
            result[j, 50] = tnc_pool_wait_time_PEAK_auto_dist_PEAK_NHA453KUTMUKTUCSMA2XQQZV(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__tnc_pool_wait_pk)
            result[j, 51] = tnc_pool_wait_time_OFFPEAK_auto_dist_OFFPEAK_FNBOP5IL57Q6SQM4VB7EXQBB(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__tnc_pool_wait_op)
            result[j, 52] = fmax_ozone_areatype__areatype___1_OWXSNW5EFKHSRO2IUCB6DHTR(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 53] = fmax_ozone_areatype__areatype___2_FIT25FYRGELBEONPX2MTPZFN(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 54] = fmax_ozone_areatype__areatype___3_LIPIKNXA6ZB3PUNRJBVIHATL(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 55] = fmax_ozone_areatype__areatype___4_J3D56DXM2MR6NEMIQ6ALSFID(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 56] = fmin_ozone_areatype__areatype___1_OTNZJFEFZT7N3JG6YKEUWGTF(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 57] = fmin_ozone_areatype__areatype___2_FY4D7BMCO2N4QRIA6QIIWTZI(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 58] = fmin_ozone_areatype__areatype___3_34BD5WGYZOPLALWPB2MCWKA7(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 59] = fmin_ozone_areatype__areatype___4_64S6AODYPVZP57LTAC522OHX(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 60] = log_attractions_HBWH(argarray[j], inputarray[j], result[j], __attractions__HBWH)
            result[j, 61] = log_attractions_HBWL(argarray[j], inputarray[j], result[j], __attractions__HBWL)
            result[j, 62] = log_attractions_HBS(argarray[j], inputarray[j], result[j], __attractions__HBS)
            result[j, 63] = log_attractions_HBO(argarray[j], inputarray[j], result[j], __attractions__HBO)
            result[j, 64] = log_attractions_NHB(argarray[j], inputarray[j], result[j], __attractions__NHB)
            result[j, 65] = log_attractions_HBWH____666_GW3RKVJQOVET3XE7MYE2MA7V(argarray[j], inputarray[j], result[j], __attractions__HBWH)
            result[j, 66] = log_attractions_HBWL____666_IQGLCTAX7EZOXKEFCIBK33BD(argarray[j], inputarray[j], result[j], __attractions__HBWL)
            result[j, 67] = log_attractions_HBS____666_L3ZMVRT365KFSBLEDVABFEGL(argarray[j], inputarray[j], result[j], __attractions__HBS)
            result[j, 68] = log_attractions_HBO____666_5VORECHSLO2A6MKP5I3SGJVQ(argarray[j], inputarray[j], result[j], __attractions__HBO)
            result[j, 69] = log_attractions_NHB____666_3GFB7KMY3JN5TSJE4MRRR2HM(argarray[j], inputarray[j], result[j], __attractions__NHB)
    else:
        for j in range(out_size):
            result[j, 0] = o_zone____dtaz_HXCH5HUPIDPV3EWONWFXBZHM(argarray[j], inputarray[j], result[j], )
            result[j, 1] = transit_ivtt_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__ivtt)
            result[j, 2] = transit_ivtt_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__ivtt)
            result[j, 3] = transit_ovtt_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__ovtt)
            result[j, 4] = transit_ovtt_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__ovtt)
            result[j, 5] = transit_fare_PEAK(argarray[j], inputarray[j], result[j], __transit_pk__fare)
            result[j, 6] = transit_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __transit_op__fare)
            result[j, 7] = piece_transit_ivtt_OFFPEAK__None__20__54N5BOFSYCN6SMRBKXM4V4TW(argarray[j], inputarray[j], result[j], )
            result[j, 8] = piece_transit_ivtt_OFFPEAK__20__None__2SYXPZCTOY3PKAO2L4ALLHLI(argarray[j], inputarray[j], result[j], )
            result[j, 9] = autopropensity(argarray[j], inputarray[j], result[j], __d_autopropensity__autopropensity)
            result[j, 10] = auto_time_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_time)
            result[j, 11] = auto_dist_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 12] = auto_time_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_time)
            result[j, 13] = auto_dist_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 14] = auto_opcost_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_opcost)
            result[j, 15] = auto_opcost_hov_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_opcost_hov)
            result[j, 16] = auto_opcost_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_opcost)
            result[j, 17] = auto_toll_hiinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hiinc)
            result[j, 18] = auto_toll_loinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_loinc)
            result[j, 19] = auto_toll_hov_hiinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hov_hiinc)
            result[j, 20] = auto_toll_hov_loinc_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_toll_hov_loinc)
            result[j, 21] = auto_toll_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_toll)
            result[j, 22] = piece_auto_dist_PEAK_None_0_5__NZ6HG4EJYEENT2OAOBDF3W5W(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 23] = piece_auto_dist_PEAK_0_5_1_0__4K5ZPM55FQJVFMWKUPZIQ2ET(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 24] = piece_auto_dist_PEAK_1_0_None__FMBHM5APW4TIH6B5EKSKSXYD(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 25] = piece_auto_dist_PEAK_None_5__7D72ALZRBNSP634JYUY7IV5P(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 26] = piece_auto_dist_PEAK_5_10__DSXTFACXD5HSWU7YKUPQEGAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 27] = piece_auto_dist_PEAK_10_None__L3GKJNRUTTFCDUYV65K7KFQD(argarray[j], inputarray[j], result[j], __auto_skims__am_dist)
            result[j, 28] = piece_auto_dist_OFFPEAK_None_0_5__537AWC5AWFXLM3TRB3PPSGYZ(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 29] = piece_auto_dist_OFFPEAK_0_5_1_0__HZLL3YOGUR763JRQHG7SM6TX(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 30] = piece_auto_dist_OFFPEAK_1_0_None__FBK6GF2KEWWDN7AL3ENVH3VS(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 31] = piece_auto_dist_OFFPEAK_None_5__YWXA4HRZAVEZKC7EV5M2AX3B(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 32] = piece_auto_dist_OFFPEAK_5_10__5RQTWWGPPCPP6EQU6ZV5MX2R(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 33] = piece_auto_dist_OFFPEAK_10_None__6EE7X5PWAQKBS6WQUONWVI63(argarray[j], inputarray[j], result[j], __auto_skims__md_dist)
            result[j, 34] = taxi_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 35] = taxi_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 36] = tnc_solo_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 37] = tnc_solo_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 38] = tnc_pool_fare_PEAK(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __auto_skims__am_time)
            result[j, 39] = tnc_pool_fare_OFFPEAK(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __auto_skims__md_time)
            result[j, 40] = taxi_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__taxi_wait_pk)
            result[j, 41] = taxi_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__taxi_wait_op)
            result[j, 42] = tnc_solo_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_solo_wait_pk)
            result[j, 43] = tnc_solo_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_solo_wait_op)
            result[j, 44] = tnc_pool_wait_time_PEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_pool_wait_pk)
            result[j, 45] = tnc_pool_wait_time_OFFPEAK(argarray[j], inputarray[j], result[j], __ozone__tnc_pool_wait_op)
            result[j, 46] = taxi_wait_time_PEAK_auto_dist_PEAK_CBYSKDURSGAJ4RHX2ASS76Q2(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__taxi_wait_pk)
            result[j, 47] = taxi_wait_time_OFFPEAK_auto_dist_OFFPEAK_HC62HOLRZIWJJDAOIFX5XHV6(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__taxi_wait_op)
            result[j, 48] = tnc_solo_wait_time_PEAK_auto_dist_PEAK_2EZZEATSM4EAHST74DMGSFSS(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__tnc_solo_wait_pk)
            result[j, 49] = tnc_solo_wait_time_OFFPEAK_auto_dist_OFFPEAK_CA5RZWRWZJWFNZATH2HHD3TO(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__tnc_solo_wait_op)
            result[j, 50] = tnc_pool_wait_time_PEAK_auto_dist_PEAK_NHA453KUTMUKTUCSMA2XQQZV(argarray[j], inputarray[j], result[j], __auto_skims__am_dist, __ozone__tnc_pool_wait_pk)
            result[j, 51] = tnc_pool_wait_time_OFFPEAK_auto_dist_OFFPEAK_FNBOP5IL57Q6SQM4VB7EXQBB(argarray[j], inputarray[j], result[j], __auto_skims__md_dist, __ozone__tnc_pool_wait_op)
            result[j, 52] = fmax_ozone_areatype__areatype___1_OWXSNW5EFKHSRO2IUCB6DHTR(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 53] = fmax_ozone_areatype__areatype___2_FIT25FYRGELBEONPX2MTPZFN(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 54] = fmax_ozone_areatype__areatype___3_LIPIKNXA6ZB3PUNRJBVIHATL(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 55] = fmax_ozone_areatype__areatype___4_J3D56DXM2MR6NEMIQ6ALSFID(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 56] = fmin_ozone_areatype__areatype___1_OTNZJFEFZT7N3JG6YKEUWGTF(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 57] = fmin_ozone_areatype__areatype___2_FY4D7BMCO2N4QRIA6QIIWTZI(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 58] = fmin_ozone_areatype__areatype___3_34BD5WGYZOPLALWPB2MCWKA7(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 59] = fmin_ozone_areatype__areatype___4_64S6AODYPVZP57LTAC522OHX(argarray[j], inputarray[j], result[j], __dzone__zone_type, __ozone__zone_type)
            result[j, 60] = log_attractions_HBWH(argarray[j], inputarray[j], result[j], __attractions__HBWH)
            result[j, 61] = log_attractions_HBWL(argarray[j], inputarray[j], result[j], __attractions__HBWL)
            result[j, 62] = log_attractions_HBS(argarray[j], inputarray[j], result[j], __attractions__HBS)
            result[j, 63] = log_attractions_HBO(argarray[j], inputarray[j], result[j], __attractions__HBO)
            result[j, 64] = log_attractions_NHB(argarray[j], inputarray[j], result[j], __attractions__NHB)
            result[j, 65] = log_attractions_HBWH____666_GW3RKVJQOVET3XE7MYE2MA7V(argarray[j], inputarray[j], result[j], __attractions__HBWH)
            result[j, 66] = log_attractions_HBWL____666_IQGLCTAX7EZOXKEFCIBK33BD(argarray[j], inputarray[j], result[j], __attractions__HBWL)
            result[j, 67] = log_attractions_HBS____666_L3ZMVRT365KFSBLEDVABFEGL(argarray[j], inputarray[j], result[j], __attractions__HBS)
            result[j, 68] = log_attractions_HBO____666_5VORECHSLO2A6MKP5I3SGJVQ(argarray[j], inputarray[j], result[j], __attractions__HBO)
            result[j, 69] = log_attractions_NHB____666_3GFB7KMY3JN5TSJE4MRRR2HM(argarray[j], inputarray[j], result[j], __attractions__NHB)
    return result

def _map_index(dim_name, values):
    raise KeyError(dim_name)


def set_shared_data(transit_pk, transit_op, auto_skims, ozone, dzone, attractions, o_autopropensity, d_autopropensity):
    global name_space
    name_space = {}
    name_space['__transit_pk__ivtt'] = transit_pk['ivtt']
    name_space['__transit_pk__ovtt'] = transit_pk['ovtt']
    name_space['__transit_pk__headway'] = transit_pk['headway']
    name_space['__transit_pk__fare'] = transit_pk['fare']
    name_space['__transit_pk__firstmode'] = transit_pk['firstmode']
    name_space['__transit_pk__prioritymode'] = transit_pk['prioritymode']
    name_space['__transit_pk__lastmode'] = transit_pk['lastmode']
    name_space['__transit_op__ivtt'] = transit_op['ivtt']
    name_space['__transit_op__ovtt'] = transit_op['ovtt']
    name_space['__transit_op__headway'] = transit_op['headway']
    name_space['__transit_op__fare'] = transit_op['fare']
    name_space['__transit_op__firstmode'] = transit_op['firstmode']
    name_space['__transit_op__prioritymode'] = transit_op['prioritymode']
    name_space['__transit_op__lastmode'] = transit_op['lastmode']
    name_space['__auto_skims__am_time'] = auto_skims['am_time']
    name_space['__auto_skims__am_dist'] = auto_skims['am_dist']
    name_space['__auto_skims__am_toll_loinc'] = auto_skims['am_toll_loinc']
    name_space['__auto_skims__am_toll_hiinc'] = auto_skims['am_toll_hiinc']
    name_space['__auto_skims__md_time'] = auto_skims['md_time']
    name_space['__auto_skims__md_dist'] = auto_skims['md_dist']
    name_space['__auto_skims__md_toll'] = auto_skims['md_toll']
    name_space['__auto_skims__am_time_hov'] = auto_skims['am_time_hov']
    name_space['__auto_skims__am_dist_hov'] = auto_skims['am_dist_hov']
    name_space['__auto_skims__am_toll_hov_loinc'] = auto_skims['am_toll_hov_loinc']
    name_space['__auto_skims__am_toll_hov_hiinc'] = auto_skims['am_toll_hov_hiinc']
    name_space['__auto_skims__am_opcost'] = auto_skims['am_opcost']
    name_space['__auto_skims__am_opcost_hov'] = auto_skims['am_opcost_hov']
    name_space['__auto_skims__md_opcost'] = auto_skims['md_opcost']
    name_space['__ozone__zone_type'] = ozone['zone_type']
    name_space['__ozone__pnr_parking_cost'] = ozone['pnr_parking_cost']
    name_space['__ozone__zone_income'] = ozone['zone_income']
    name_space['__ozone__pnr_flag'] = ozone['pnr_flag']
    name_space['__ozone__first_wait_bus_peak'] = ozone['first_wait_bus_peak']
    name_space['__ozone__first_wait_bus_offpeak'] = ozone['first_wait_bus_offpeak']
    name_space['__ozone__first_wait_feeder_peak'] = ozone['first_wait_feeder_peak']
    name_space['__ozone__first_wait_feeder_offpeak'] = ozone['first_wait_feeder_offpeak']
    name_space['__ozone__autocc'] = ozone['autocc']
    name_space['__ozone__taxi_wait_pk'] = ozone['taxi_wait_pk']
    name_space['__ozone__taxi_wait_op'] = ozone['taxi_wait_op']
    name_space['__ozone__tnc_solo_wait_pk'] = ozone['tnc_solo_wait_pk']
    name_space['__ozone__tnc_solo_wait_op'] = ozone['tnc_solo_wait_op']
    name_space['__ozone__tnc_pool_wait_pk'] = ozone['tnc_pool_wait_pk']
    name_space['__ozone__tnc_pool_wait_op'] = ozone['tnc_pool_wait_op']
    name_space['__dzone__zone_type'] = dzone['zone_type']
    name_space['__dzone__pnr_parking_cost'] = dzone['pnr_parking_cost']
    name_space['__dzone__zone_income'] = dzone['zone_income']
    name_space['__dzone__pnr_flag'] = dzone['pnr_flag']
    name_space['__dzone__first_wait_bus_peak'] = dzone['first_wait_bus_peak']
    name_space['__dzone__first_wait_bus_offpeak'] = dzone['first_wait_bus_offpeak']
    name_space['__dzone__first_wait_feeder_peak'] = dzone['first_wait_feeder_peak']
    name_space['__dzone__first_wait_feeder_offpeak'] = dzone['first_wait_feeder_offpeak']
    name_space['__dzone__autocc'] = dzone['autocc']
    name_space['__dzone__taxi_wait_pk'] = dzone['taxi_wait_pk']
    name_space['__dzone__taxi_wait_op'] = dzone['taxi_wait_op']
    name_space['__dzone__tnc_solo_wait_pk'] = dzone['tnc_solo_wait_pk']
    name_space['__dzone__tnc_solo_wait_op'] = dzone['tnc_solo_wait_op']
    name_space['__dzone__tnc_pool_wait_pk'] = dzone['tnc_pool_wait_pk']
    name_space['__dzone__tnc_pool_wait_op'] = dzone['tnc_pool_wait_op']
    name_space['__attractions__HBO'] = attractions['HBO']
    name_space['__attractions__HBS'] = attractions['HBS']
    name_space['__attractions__HBWH'] = attractions['HBWH']
    name_space['__attractions__HBWL'] = attractions['HBWL']
    name_space['__attractions__NHB'] = attractions['NHB']
    name_space['__o_autopropensity__autopropensity'] = o_autopropensity['autopropensity']
    name_space['__d_autopropensity__autopropensity'] = d_autopropensity['autopropensity']


meta_match_names_idx = {'otaz_idx': 0, 'dtaz_idx': 1}


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
            named_args = ['__attractions__HBO', '__attractions__HBS', '__attractions__HBWH', '__attractions__HBWL', '__attractions__NHB', '__auto_skims__am_dist', '__auto_skims__am_opcost', '__auto_skims__am_opcost_hov', '__auto_skims__am_time', '__auto_skims__am_toll_hiinc', '__auto_skims__am_toll_hov_hiinc', '__auto_skims__am_toll_hov_loinc', '__auto_skims__am_toll_loinc', '__auto_skims__md_dist', '__auto_skims__md_opcost', '__auto_skims__md_time', '__auto_skims__md_toll', '__d_autopropensity__autopropensity', '__dzone__zone_type', '__ozone__taxi_wait_op', '__ozone__taxi_wait_pk', '__ozone__tnc_pool_wait_op', '__ozone__tnc_pool_wait_pk', '__ozone__tnc_solo_wait_op', '__ozone__tnc_solo_wait_pk', '__ozone__zone_type', '__transit_op__fare', '__transit_op__ivtt', '__transit_op__ovtt', '__transit_pk__fare', '__transit_pk__ivtt', '__transit_pk__ovtt']
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



input_name_positions = {'dtaz': 0, 'otaz': 1}
function_names = ['o_zone == dtaz', 'transit_ivtt_PEAK', 'transit_ivtt_OFFPEAK', 'transit_ovtt_PEAK', 'transit_ovtt_OFFPEAK', 'transit_fare_PEAK', 'transit_fare_OFFPEAK', 'piece(transit_ivtt_OFFPEAK, None, 20)', 'piece(transit_ivtt_OFFPEAK, 20, None)', 'autopropensity', 'auto_time_PEAK', 'auto_dist_PEAK', 'auto_time_OFFPEAK', 'auto_dist_OFFPEAK', 'auto_opcost_PEAK', 'auto_opcost_hov_PEAK', 'auto_opcost_OFFPEAK', 'auto_toll_hiinc_PEAK', 'auto_toll_loinc_PEAK', 'auto_toll_hov_hiinc_PEAK', 'auto_toll_hov_loinc_PEAK', 'auto_toll_OFFPEAK', 'piece(auto_dist_PEAK,None,0.5)', 'piece(auto_dist_PEAK,0.5,1.0)', 'piece(auto_dist_PEAK,1.0,None)', 'piece(auto_dist_PEAK,None,5)', 'piece(auto_dist_PEAK,5,10)', 'piece(auto_dist_PEAK,10,None)', 'piece(auto_dist_OFFPEAK,None,0.5)', 'piece(auto_dist_OFFPEAK,0.5,1.0)', 'piece(auto_dist_OFFPEAK,1.0,None)', 'piece(auto_dist_OFFPEAK,None,5)', 'piece(auto_dist_OFFPEAK,5,10)', 'piece(auto_dist_OFFPEAK,10,None)', 'taxi_fare_PEAK', 'taxi_fare_OFFPEAK', 'tnc_solo_fare_PEAK', 'tnc_solo_fare_OFFPEAK', 'tnc_pool_fare_PEAK', 'tnc_pool_fare_OFFPEAK', 'taxi_wait_time_PEAK', 'taxi_wait_time_OFFPEAK', 'tnc_solo_wait_time_PEAK', 'tnc_solo_wait_time_OFFPEAK', 'tnc_pool_wait_time_PEAK', 'tnc_pool_wait_time_OFFPEAK', 'taxi_wait_time_PEAK/auto_dist_PEAK', 'taxi_wait_time_OFFPEAK/auto_dist_OFFPEAK', 'tnc_solo_wait_time_PEAK/auto_dist_PEAK', 'tnc_solo_wait_time_OFFPEAK/auto_dist_OFFPEAK', 'tnc_pool_wait_time_PEAK/auto_dist_PEAK', 'tnc_pool_wait_time_OFFPEAK/auto_dist_OFFPEAK', 'fmax(ozone_areatype, areatype)==1', 'fmax(ozone_areatype, areatype)==2', 'fmax(ozone_areatype, areatype)==3', 'fmax(ozone_areatype, areatype)==4', 'fmin(ozone_areatype, areatype)==1', 'fmin(ozone_areatype, areatype)==2', 'fmin(ozone_areatype, areatype)==3', 'fmin(ozone_areatype, areatype)==4', 'log_attractions_HBWH', 'log_attractions_HBWL', 'log_attractions_HBS', 'log_attractions_HBO', 'log_attractions_NHB', 'log_attractions_HBWH > -666', 'log_attractions_HBWL > -666', 'log_attractions_HBS > -666', 'log_attractions_HBO > -666', 'log_attractions_NHB > -666']

# Greetings, tinkerer!  The `defs_hash` included here is a safety 
# measure to prevent unknowing users creating a mess by modifying 
# the code in this module so that it no longer matches the expected 
# variable definitions. If you want to modify this code, you should 
# delete this hash to allow the code to run without any checks, but 
# you do so at your own risk. 
defs_hash = 'XUCQEOOTEO3UREF2YYT73XUJ24======'
