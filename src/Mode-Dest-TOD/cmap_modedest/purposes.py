
purposes5 = (
    'HBWH',  # Home-based Work, High Income
    'HBWL',  # Home-based Work, Low Income
    'HBS',   # Home-based Shopping
    'HBO',   # Home-based Other Purpose Not Enumerated
    'NHB',   # Non-home-based
)

purposes8 = (
    'HBWH',  # Home-based Work, High Income
    'HBWL',  # Home-based Work, Low Income
    'HBS',   # Home-based Shopping
    'HBO',   # Home-based Other Purpose Not Enumerated
    'HBOR',  # Home-based Other Purpose Not Enumerated, at a residence
    'NHB',   # Non-home-based not shopping
    'NHBS',  # Non-home-based shopping
    'NHBR',  # Non-home-based, at a residence
)

purposes3 = (
    'HW',   # Home-based Work
    'HO',   # Home-based Other Purpose Not Work
    'NH',   # Non-home-based
)

purposesA = purposes5

purposes_to_peaky = dict(
    HBWH=1,
    HBWL=1,
    HBS=0,
    HBO=0,
    NHB=0,
    HBOR=0,
    NHBR=0,
    NHBS=0,
)

purposes_to_3 = dict(
    HBWH='HW',
    HBWL='HW',
    HBS='HO',
    HBO='HO',
    NHB='NH',
    HBOR='HO',
    NHBR='NH',
    NHBS='NH',
)

purposes_to_5 = dict(
    HBWH='HBWH',
    HBWL='HBWL',
    HBS='HBS',
    HBO='HBO',
    NHB='NHB',
    HBOR='HBO',
    NHBR='NHB',
    NHBS='NHB',
)