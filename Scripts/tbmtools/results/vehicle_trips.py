from pathlib import Path

def export_matrices(outdir, modeller):
    """
    Export vehicle trip matrices from Emme.
    """
    # Make output subdirectories.
    tripdir = outdir.joinpath('trips')
    tripdir.mkdir(exist_ok=True)
    # Construct Modeller tool.
    export_matrix_data = modeller.tool('inro.emme.data.matrix.export_matrix_to_csv')
    # Export vehicle trip matrices.
    matrix_names = {'mf4': 'b_truck',
                    'mf5': 'l_truck',
                    'mf6': 'm_truck',
                    'mf7': 'h_truck',
                    'mf8': 'poe_auto',
                    'mf9': 'poe_truck',
                    'mf10': 'airport'}
    export_matrix_data(matrices=list(matrix_names.keys()),
                       export_path=tripdir)
    return tripdir