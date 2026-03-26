import os
import sys
import re
from pathlib import Path
import inro.modeller as _m
sys.path.append(str(Path(__file__).resolve().parents[2].joinpath('Scripts')))    # Assumes script is in Database/ANOTHERSUBFOLDER; [2] goes up two folders to find cmap_trip-based_model/Scripts
from tbmtools import project as tbm
    
def main():
    # Define the path to the Emme project (.emp file)
    proj_dir = Path(__file__).resolve().parents[2]
    my_modeller = tbm.connect(proj_dir)
    
    # Connect to the Modeller
    delete_matrix = my_modeller.tool('inro.emme.data.matrix.delete_matrix')
    matrix_init = my_modeller.tool('inro.emme.data.matrix.create_matrix')
    process = my_modeller.tool('inro.emme.data.matrix.matrix_transaction')
    my_emmebank = my_modeller.emmebank

    # proceed with all the tools you need

    directory = os.getcwd().replace('\\prep_macros','')
    tgdata_path = Path(directory + "\\tg\\data")
    for file in tgdata_path.glob("*.in"):
        with open(file, "r") as f:
            lines = f.readlines()

        third_row = lines[2]

        # Extract matrix_id
        match = re.search(r"matrix=(.*?) ", third_row)
        matrix_id = match.group(1).strip()

        # Parse the row
        parts = third_row.split()
        matrix_name = parts[2]
        matrix_description = " ".join(parts[4:])

        matrix_file = os.path.join(tgdata_path, file)

        # Delete old matrix
        delete_matrix(matrix=my_emmebank.matrix(matrix_id))

        # Dynamically name the variable: new_<matrix_id>
        var_name = f"new_{matrix_id}"
        globals()[var_name] = matrix_init(
            matrix_id=matrix_id,
            matrix_name=matrix_name,
            matrix_description=matrix_description,
            overwrite=True,
            default_value=0
        )

        # Process the transaction file
        process(
            transaction_file=matrix_file,
            throw_on_error=True,
            scenario=_m.Modeller().scenario
        )
        print(f'Successfully process {file}')


    # END


if __name__ == '__main__':
    main()