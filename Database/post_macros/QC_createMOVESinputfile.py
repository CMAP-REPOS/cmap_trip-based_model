import pandas as pd, numpy as np
from pathlib import Path
from functools import reduce

## This file runs two quality checks for createMOVESinputfile.py
## (1) check_im(): Checks if all tabs in two comparison workbooks match for a given imregion and scenario
## (2) check_im_county(): Checks if the VHT/VMT in initial_model_output for each county in a given imregion
## sum up to the correct aggregated values

### TO RUN: specify the scenario year and model ###################################################################
### This set-up assumes the files are in data/MOVES_cXXqX_scenXXX. The files you want to compare to must have
### the suffix "_old", for example MOVES_c26q2_scen100_IM_old.xlsx and MOVES_c26q2_scen100_nonIM_old.xlsx
SCEN = "600"
MODEL = "c26q2"
##################################################################################################################

DB_DIR = Path(__file__).resolve().parents[1]
FOLDER_PATH = DB_DIR.joinpath("data",f"MOVES_{MODEL}_scen{SCEN}")

def check_im(scen, imregion):
    """"
    Compares the new and old Excel workbooks for a specific IM region and scenario,
    ex. MOVES_c26q2_scen100_IM.xlsx and MOVES_c26q2_scen100_IM_old.xlsx
    Prints True if all cells match, False otherwise.
    """
    # Get paths of new and old files
    new_file_path = FOLDER_PATH.joinpath(f"MOVES_{MODEL}_scen{scen}_{imregion}.xlsx")
    old_file_path = FOLDER_PATH.joinpath(f"MOVES_{MODEL}_scen{scen}_{imregion}_old.xlsx")

    tab_names = ["initial_model_output","AvgSpeedDistribution", "RoadTypeDistribution","hourVMTFraction", "HPMSDailyVMT"]
    
    print("check_im()")
    print(f"... Comparing MOVES_{MODEL}_scen{scen}_{imregion}.xlsx and MOVES_{MODEL}_scen{scen}_{imregion}_old.xlsx")

    # Check equality of each tab
    for tab in tab_names:
        old_data = pd.read_excel(old_file_path,sheet_name=tab)
        new_data = pd.read_excel(new_file_path,sheet_name=tab)

        print(f"--> {tab} equality: {old_data.equals(new_data)}")


def check_im_county(scen, imregion):
    """
    Checks to see if the VMT/VHT in the initial_model_output tab from each county sum up to the aggregated VMT/VHT.
    For VMT and VHT, prints True if they match, False otherwise. 
    """
    # Specify counties for given IM region
    if imregion == "IM":
        counties = ['COOK', 'DUPAGE', 'KANE', 'KENDALL', 'LAKE', 'MCHENRY', 'WILL']
    if imregion == "nonIM":
        counties = ['KANE', 'KENDALL', 'LAKE', 'MCHENRY', 'WILL','GRUNDY']

    # Merge all county initial_model_output datasets together for given imregion
    dfs = []
    for county in counties:
        county_file_path = FOLDER_PATH.joinpath(f"MOVES_{MODEL}_scen{scen}_{imregion}_{county}.xlsx")

        # get initial_model_output tab from county file
        initial_model_output = pd.read_excel(county_file_path,sheet_name="initial_model_output")
        initial_model_output = initial_model_output.rename(columns={"vmt": f"vmt_{county}", "vht": f"vht_{county}"})

        dfs.append(initial_model_output)

    ids = ["sourceTypeID","roadTypeID","hourDayID","avgSpeedBinID","imarea"]
    merged_df = reduce(lambda left, right: pd.merge(left, right, on=ids, how='outer'), dfs)

    # Create totals for VMT/VHT
    merged_df["vmt_TOTAL"] = sum([merged_df[f"vmt_{county}"] for county in counties])
    merged_df["vht_TOTAL"] = sum([merged_df[f"vht_{county}"] for county in counties])

    # Use OLD aggregated file for given im region
    aggregated_file_path = FOLDER_PATH.joinpath(f"MOVES_{MODEL}_scen{scen}_{imregion}_old.xlsx")
    agg_initial_model_output = pd.read_excel(aggregated_file_path,sheet_name="initial_model_output")

    # Check for approximate equality (necessary because floats, tolerance = 1e-8) 
    print()
    print("check_im_county()")
    print(f"... Aggregating {counties}")
    print(f"... Comparing to MOVES_{MODEL}_scen{scen}_{imregion}_old.xlsx")
    print("--> VMT Equality:", np.all(np.isclose(merged_df["vmt_TOTAL"], agg_initial_model_output["vmt"])))
    print("--> VHT Equality:", np.all(np.isclose(merged_df["vht_TOTAL"], agg_initial_model_output["vht"])))
    print()


if __name__ == "__main__":
    for imregion in ["nonIM", "IM"]:
        print(f"** SCENARIO YEAR: {SCEN}, REGION: {imregion} **")
        check_im(SCEN, imregion)
        check_im_county(SCEN, imregion)