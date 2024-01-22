from copy import deepcopy
from pathlib import Path

SKIM_MATRIX_IDS = {'transit': {'peak': {'in-vehicle minutes': 'mf822',
                                        'walk transfer minutes': 'mf823',
                                        'wait time': 'mf838',
                                        'headway': 'mf839',
                                        'priority mode': 'mf830',
                                        'average fare': 'mf828',
                                        'station zone': 'mf837'},
                               'off-peak': {'in-vehicle minutes': 'mf922',
                                            'walk transfer minutes': 'mf923',
                                            'wait time': 'mf938',
                                            'headway': 'mf939',
                                            'priority mode': 'mf930',
                                            'average fare': 'mf928',
                                            'station zone': 'mf937'}},
                   'highway': {'am': {'time': 'mf44',
                                      'distance': 'mf45'},
                               'md': {'time': 'mf46',
                                      'distance': 'mf47'}}}

def flag_transit_disconnects(modeller):
    """
    Flag peak and off-peak transit skim O-Ds that are not connected by
    transit. Use a flag value of 9999.
    """
    compute_matrices = modeller.tool('inro.emme.matrix_calculation.matrix_calculator')
    transit_skims = SKIM_MATRIX_IDS['transit']
    for transitnet, transitnet_skims in transit_skims.items():
        # Flag O-Ds with negative or impossibly large values for in-vehicle minutes.
        spec = {'type': 'MATRIX_CALCULATION',
                'expression': '9999',
                'result': transitnet_skims['in-vehicle minutes'],
                'constraint': {'by_value': {'od_values': transitnet_skims['in-vehicle minutes'],
                                            'interval_min': 0,
                                            'interval_max': 9999,
                                            'condition': 'EXCLUDE'}}}
        compute_matrices(spec)
        # Apply flag to other transit skim matrices.
        spec['constraint']['by_value']['interval_min'] = 9999
        spec['constraint']['by_value']['condition'] = 'INCLUDE'
        specs = []
        for desc, mtx_id in transitnet_skims.items():
            if desc not in ['in-vehicle minutes', 'station zone']:
                spec['result'] = mtx_id
                specs.append(deepcopy(spec))
        compute_matrices(specs)

def export_matrices(outdir, modeller):
    """
    Export peak and off-peak transit skims and highway time and distance
    skim matrices.
    """
    # Make output subdirectory.
    skimdir = outdir.joinpath('skims')
    skimdir.mkdir(exist_ok=True)
    # Construct Modeller tool.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    # Export peak transit skims.
    export_matrix_data(matrices=[i for i in list(SKIM_MATRIX_IDS['transit']['peak'].values())],
                       export_path=skimdir)
    # Export off-peak transit skims.
    export_matrix_data(matrices=[i for i in list(SKIM_MATRIX_IDS['transit']['off-peak'].values())],
                       export_path=skimdir)
    # Export am highway skims.
    export_matrix_data(matrices=[i for i in list(SKIM_MATRIX_IDS['highway']['am'].values())],
                       export_path=skimdir)
    # Export md highway skims.
    export_matrix_data(matrices=[i for i in list(SKIM_MATRIX_IDS['highway']['md'].values())],
                       export_path=skimdir)