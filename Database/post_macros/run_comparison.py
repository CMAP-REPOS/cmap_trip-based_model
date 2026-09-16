"""
run_comparison.py

This script creates an excel spreadsheet with tables and charts to compare various statistics between two conformity runs.

Written by: David Wells
Last Update: 9/15/26
"""

import pandas as pd
import numpy as np
import os
import fnmatch
import yaml
from openpyxl import Workbook
from openpyxl.worksheet.worksheet import Worksheet
from openpyxl.utils import get_column_letter
from openpyxl.styles import PatternFill, Font, Border, Side, Alignment
from openpyxl.chart import BarChart, Reference
from collections.abc import Callable

####################################################################################################################################################
#
# variables that need to be updated
#
####################################################################################################################################################

# path to the conformity folder
run1_path = r"E:\djw\Fix_Final_Run_Stats\c25q4"
run2_path = r"E:\djw\Fix_Final_Run_Stats\c26q2"

# list of tuples of the years that should be compared between the runs, the script will identify the correct scenario numbers to use
comparison_years = [
    (2019, 2019),
    (2025, 2026),
    (2030, 2030),
    (2035, 2035),
    (2040, 2040),
    (2050, 2050)
]

# path to a folder where the excel sheet will be saved (must include the / or \\ at the end) or "" to save to the working directory
output_dir = ""

####################################################################################################################################################
#
# helper functions, excel aesthetic definitions, custom classes
#
####################################################################################################################################################

# attempt to match years to 5 year intervals to account for one year differences in scenarios
def remap_year(year):
    year = int(year)
    if year % 5 == 0:
        return year
    elif (year + 1) % 5 == 0:
        return year + 1
    elif (year - 1) % 5 == 0:
        return year - 1
    else: 
        return year

# flatten 2d list
def flatten(xss):
    return [x for xs in xss for x in xs]

# convert string representations of numbers to numbers
def str2num(n: str):
    n = str(n)
    n2 = n.replace(',', '')
    try:
        if '.' in n2:
            return float(n2)
        else:
            return int(n2)
    except ValueError:
        return n

# compare two lists of numbers returning percent different and absolute difference
def compare_values(l1: list, l2: list):
    prct_diff = []
    abs_diff = []
    for i in range(len(l1)):
        try:
            prct_diff.append((l2[i]-l1[i])/l1[i])
            abs_diff.append(l2[i]-l1[i])
        except TypeError:
            prct_diff.append(None)
            abs_diff.append(None)
    return prct_diff, abs_diff

# 
# define excel aesthetics
#

# fill colors
white_fill = PatternFill(start_color="ffffff", end_color = "ffffff", fill_type = "solid")
light_blue_fill = PatternFill(start_color="c5d9f1", end_color = "c5d9f1", fill_type = "solid")
light_yellow_fill = PatternFill(start_color="fde9d9", end_color = "fde9d9", fill_type = "solid")
light_green_fill = PatternFill(start_color="ebf1de", end_color = "ebf1de", fill_type = "solid")
light_red_fill = PatternFill(start_color="e6b8b7", end_color = "e6b8b7", fill_type = "solid")
light_gray_fill = PatternFill(start_color="d9d9d9", end_color = "d9d9d9", fill_type = "solid")
light_purple_fill = PatternFill(start_color="ccc0da", end_color = "ccc0da", fill_type = "solid")
dark_blue_fill = PatternFill(start_color="1f497d", end_color="1f497d", fill_type="solid")

# fonts
normal_font = Font(name = "Arial", size = 10, bold = False, color = '000000')
bold_font = Font(name = "Arial", size = 10, bold = True, color = '000000')
category_font = Font(name = "Arial", size = 10, bold = True, color = '000000', underline = "single")
red_font = Font(name = "Arial", size = 10, bold = False, color = 'FF0000')
red_bold_font = Font(name = "Arial", size = 10, bold = True, color = 'FF0000')
orange_font = Font(name = "Arial", size = 10, bold = False, color = "FF7B00")

# alignments
align_h_center = Alignment(horizontal="center")
align_h_left = Alignment(horizontal="left")

#
# custom classes
#

# custom object for storing data about a run
class ModelRun:
    def __init__(self, path):
        # path to run folder
        self.path = path
        # name of run
        self.name = self.path.replace("/", "\\").split("\\")[-1]

        # create a dictionary of all scenarios in the run, storing a label and the path to the Database folder of the scenario
        self.scenarios = {}

        scenario_folders = fnmatch.filter(os.listdir(self.path), pat = f"{self.name}_???_*")
        with open(os.path.join(self.path, scenario_folders[0], "cmap_trip-based_model", "Scripts", "prepare", "conformity_scenario", "hand", "config.yaml")) as f:
            scenario_years = yaml.safe_load(f)["scenario_years"]

        for scenario_name in scenario_folders:
            scenario_num = int(scenario_name.split("_")[1])
            scenario_year = scenario_years[scenario_num]
            # # originally tried to automatically match scenario between runs, replaced by explicitly defining match years in the comparison_years variable
            # match_year = remap_year(scenario_year)
            # self.scenarios[match_year] = {
            self.scenarios[scenario_year] = {
                "label": f"{'_'.join(scenario_name.split('_')[:2])}_{scenario_year}",
                "path": os.path.join(self.path, scenario_name, "cmap_trip-based_model", "Database"),
                "year": scenario_year
            }

####################################################################################################################################################
#
# statistics comparison helper function
#   
#################################################################################################################################################### 

def create_comparison_table(
    ws: Worksheet, # openpyxl Worksheet object to create the comparison table in
    labels: list[str], # list of labels to put in the first column only
    value_func: Callable[[str], [list[float], list[str]]], # a function that takes the path of a database folder and returns a list of values for that run (with any spacing needed), optionally also returns a matching list of number formats
    run1: ModelRun, # ModelRun object for run1
    run2: ModelRun, # ModelRun object for run2
    comparison_years: list[tuple[int]], # list of tuples of years to compare betweens run1 and run2
    label_fills: list[PatternFill] | None = None, # list of fills for each label
    label_fonts: list[Font] | None = None, # list of fills for each label
    label_alignments: list[Alignment] | None = None, # list of fills for each label,
    number_formats: list[str] | None = None, # list of excel number format strings for each row
    prct_diff_thresholds: list[float] | None = None, # list of floats to use as the threshold to flag percent differences for each row
    abs_diff_thresholds: list[float] | None = None # list of floats to use as the threshold to flag absolute differences for each row
):
    #
    # setup aesthetic defaults
    #

    if label_fills is None:
        label_fills = [white_fill]*len(labels)

    if label_fonts is None:
        label_fonts = [normal_font]*len(labels)
    
    if label_alignments is None:
        label_alignments = [align_h_left]*len(labels)

    if number_formats is None:
        number_formats = ["#,##0.00"]*len(labels)

    if prct_diff_thresholds is None:
        prct_diff_thresholds = [None]*len(labels)

    if abs_diff_thresholds is None:
        abs_diff_thresholds = [None]*len(labels)

    #
    # display labels
    #

    # set column width
    ws.column_dimensions['A'].width = 30

    # add all labels
    for i in range(len(labels)):
        cell = ws.cell(row=i+3, column=1)
        cell.value = labels[i]
        cell.fill = label_fills[i]
        cell.font = label_fonts[i]
        cell.alignment = label_alignments[i]

    # 
    # display values and comparison
    #

    for scen_i, scen_years in enumerate(comparison_years):
        scen_year1 = scen_years[0]
        scen_year2 = scen_years[1]
        # load scenario data from both runs
        run1_values = value_func(run1.scenarios[scen_year1]["path"])
        run2_values = value_func(run2.scenarios[scen_year2]["path"])
        # get percent and absolute difference
        prct_diff, abs_diff = compare_values(run1_values, run2_values)

        # 
        # setup year and scenario labels
        #

        # display the match year
        if run1.scenarios[scen_year1]["year"] == run2.scenarios[scen_year2]["year"]:
            year_label = run1.scenarios[scen_year1]["year"]
        else:
            year_label = f"{run1.scenarios[scen_year1]["year"]}/{run2.scenarios[scen_year2]["year"]}"
        cell = ws.cell(row=1, column=2+(5*scen_i))
        cell.value = year_label
        cell.alignment = align_h_center
        cell.font = bold_font
        ws.merge_cells(start_row=1, end_row=1, start_column=2+(5*scen_i), end_column=2+(5*scen_i)+3)

        # run1 scenario label
        ws.column_dimensions[get_column_letter(2+(5*scen_i))].width = 12
        cell = ws.cell(row=2, column=2+(5*scen_i))
        cell.value = run1.name
        cell.alignment = align_h_center
        cell.font = bold_font

        # run2 scenario label
        ws.column_dimensions[get_column_letter(2+(5*scen_i)+1)].width = 12
        cell = ws.cell(row=2, column=2+(5*scen_i)+1)
        cell.value = run2.name
        cell.alignment = align_h_center
        cell.font = bold_font

        # percent difference column
        ws.column_dimensions[get_column_letter(2+(5*scen_i)+2)].width = 8
        cell = ws.cell(row=2, column=2+(5*scen_i)+2)
        cell.value = "Diff%"
        cell.alignment = align_h_center
        cell.font = bold_font

        # actual difference column
        ws.column_dimensions[get_column_letter(2+(5*scen_i)+3)].width = 12
        cell = ws.cell(row=2, column=2+(5*scen_i)+3)
        cell.value = "Difference"
        cell.alignment = align_h_center
        cell.font = bold_font

        # add spacer between years
        ws.column_dimensions[get_column_letter(2+(5*scen_i)+4)].width = 2
        for i in range(len(run1_values)+2):
            cell = ws.cell(row=i+1, column=2+(5*scen_i)+4)
            cell.fill = dark_blue_fill

        for value_i in range(len(run1_values)):
            # run 1 values
            cell = ws.cell(row=value_i+3, column=2+(5*scen_i))
            cell.value = run1_values[value_i]
            cell.number_format = number_formats[value_i]
            cell.font = normal_font

            # run 2 values
            cell = ws.cell(row=value_i+3, column=2+(5*scen_i)+1)
            cell.value = run2_values[value_i]
            cell.number_format = number_formats[value_i]
            cell.font = normal_font

            # percent difference
            cell = ws.cell(row=value_i+3, column=2+(5*scen_i)+2)
            cell.value = prct_diff[value_i]
            cell.number_format = "0.00%"
            cell.font = normal_font
            if prct_diff[value_i] is not None:
                if abs(prct_diff[value_i]) > 0.05:
                    cell.font = red_font
                elif prct_diff_thresholds[value_i] is not None and abs(prct_diff[value_i]) > prct_diff_thresholds[value_i]:
                    cell.font = orange_font

            # absolute difference
            cell = ws.cell(row=value_i+3, column=2+(5*scen_i)+3)
            cell.value = abs_diff[value_i]
            cell.number_format = number_formats[value_i]
            cell.font = normal_font
            if abs_diff[value_i] is not None and abs_diff_thresholds[value_i] is not None and abs(abs_diff[value_i]) > abs_diff_thresholds[value_i]:
                cell.font = orange_font

####################################################################################################################################################
#
# add chart helper function
#
####################################################################################################################################################

def add_charts(
    ws: Worksheet, # openpyxl Worksheet object to create the charts in
    chart_defs: list[dict], # list of chart definition dictionaries with the following structure: {
    # "title": name of statistic being compared, title of chart
    # "stat_row": the excel row index of the stat in the worksheet to graph, can be a list of rows if statistics should be side by side
    # "split_labels": list of labels describing the split, only needed when stat_row is a list
    # }
    run1: ModelRun, # ModelRun object for run1
    run2: ModelRun, # ModelRun object for run2
    comparison_years: list[tuple[int]], # list of tuples of years to compare betweens run1 and run2
):
    # number of years of data to compare
    n_years = len(comparison_years)
    # keep track of how many rows of chart data have been added to this sheet
    start_row = 3
    # position charts to the right of the comparison tables
    start_column = 2 + (5*n_years) + 1

    # set column widths
    ws.column_dimensions[get_column_letter(start_column)].width = 10
    ws.column_dimensions[get_column_letter(start_column+1)].width = 12
    ws.column_dimensions[get_column_letter(start_column+2)].width = 12

    # add each chart in the list
    for chart_def in chart_defs:
        # add chart title above data
        cell = ws.cell(row = start_row, column = start_column)
        cell.value = chart_def["title"]
        cell.font = bold_font
        cell.alignment = align_h_center
        ws.merge_cells(start_row = start_row, end_row = start_row, start_column = start_column, end_column = start_column + 2)
        
        # add run labels
        cell = ws.cell(row = start_row+1, column = start_column+1)
        cell.value = run1.name
        cell.alignment = align_h_center
        cell = ws.cell(row = start_row+1, column = start_column+2)
        cell.value = run2.name
        cell.alignment = align_h_center

        # add values for each scenario
        row_i = 0
        for scen_i, scen_years in enumerate(comparison_years):
            scen_year1 = scen_years[0]
            scen_year2 = scen_years[1]
            
            # handle single vs multiple stats
            if type(chart_def["stat_row"]) is int:
                stat_rows = [chart_def["stat_row"]]
                split_labels = [""]
            else:
                stat_rows = chart_def["stat_row"]
                split_labels = chart_def["split_labels"]

            for split_i in range(len(stat_rows)):
                # add scenario year label
                cell = ws.cell(row = start_row+2+row_i, column = start_column)
                if run1.scenarios[scen_year1]["year"] == run2.scenarios[scen_year2]["year"]:
                    year = run1.scenarios[scen_year1]['year']
                else:
                    year = f"{run1.scenarios[scen_year1]['year']}/{run2.scenarios[scen_year2]['year']}"
                cell.value = f"{year} {split_labels[split_i]}"
                cell.alignment = align_h_left

                # add values for statistics
                cell = ws.cell(row = start_row+2+row_i, column = start_column+1)
                cell.value = ws.cell(row = stat_rows[split_i], column = 2+(5*scen_i)).value
                cell.number_format = "#,##0"
                cell = ws.cell(row = start_row+2+row_i, column = start_column+2)
                cell.value = ws.cell(row = stat_rows[split_i], column = 2+(5*scen_i)+1).value
                cell.number_format = "#,##0"

                row_i += 1

        # create chart
        chart = BarChart()
        chart.type = "col"
        chart.title = chart_def["title"]

        chart.add_data(
            Reference(ws, min_row = start_row+1, max_row = start_row+1+row_i, min_col = start_column+1, max_col = start_column+2),
            titles_from_data = True
        )
        chart.set_categories(
            Reference(ws, min_row = start_row+2, max_row = start_row+1+row_i, min_col = start_column, max_col = start_column)
        )

        chart.x_axis.delete = False
        chart.y_axis.delete = False
        chart.y_axis.number_format = "#,##0"

        chart.legend.position = "r"
        chart.legend.overlay = False

        chart.title.overlay = False

        ws.add_chart(chart, f"{get_column_letter(start_column + 4)}{start_row}")

        # add space for next chart
        start_row += max(15, row_i)


####################################################################################################################################################
#
# setup comparison
#
####################################################################################################################################################

run1 = ModelRun(run1_path)
run2 = ModelRun(run2_path)

# create new Excel workbook
wb = Workbook()

####################################################################################################################################################
#
# trip statistics sheet
#
####################################################################################################################################################

ws = wb.create_sheet("trip statistics")
ws.freeze_panes = 'B3'

# get the list of row labels from the output csv
trip_stat_labels = pd.read_csv(os.path.join(run1.scenarios[comparison_years[0][0]]["path"], "report", "final_run_statistics.csv"))["type"].tolist()
# create list of cell fills, fonts, and alignments to match each label
trip_stat_label_fills = []
trip_stat_label_fonts = []
trip_stat_label_alignments = []
trip_stat_number_formats = []
trip_stat_prct_diff_thresholds = []
trip_stat_abs_diff_thresholds = []
for label in trip_stat_labels:
    # fills, number formats, and difference thresholds based on category
    if label.endswith("Person Trips"):
        trip_stat_label_fills.append(light_blue_fill)
        trip_stat_number_formats.append("#,##0")
        trip_stat_prct_diff_thresholds.append(None)
        trip_stat_abs_diff_thresholds.append(None)
    elif label.endswith("Transit Share"):
        trip_stat_label_fills.append(light_yellow_fill)
        trip_stat_number_formats.append("0.00%")
        trip_stat_prct_diff_thresholds.append(None)
        trip_stat_abs_diff_thresholds.append(None)
    elif label.endswith("Miles") or label == "Trip Distance":
        trip_stat_label_fills.append(light_green_fill)
        trip_stat_number_formats.append("#,##0.00")
        trip_stat_prct_diff_thresholds.append(None)
        trip_stat_abs_diff_thresholds.append(None)
    elif label.endswith("Minutes") or label == "Trip Duration":
        trip_stat_label_fills.append(light_red_fill)
        trip_stat_number_formats.append("#,##0.00")
        trip_stat_prct_diff_thresholds.append(None)
        trip_stat_abs_diff_thresholds.append(None)
    elif label.endswith("Trips"):
        trip_stat_label_fills.append(light_gray_fill)
        trip_stat_number_formats.append("#,##0")
        trip_stat_prct_diff_thresholds.append(0.0001)
        trip_stat_abs_diff_thresholds.append(5)
    elif label.endswith("VMT"):
        trip_stat_label_fills.append(light_purple_fill)
        trip_stat_number_formats.append("#,##0")
        trip_stat_prct_diff_thresholds.append(0.002)
        trip_stat_abs_diff_thresholds.append(None)
    else:
        trip_stat_label_fills.append(white_fill)
        trip_stat_number_formats.append("#,##0.00")
        trip_stat_prct_diff_thresholds.append(None)
        trip_stat_abs_diff_thresholds.append(None)

    # red font for ENTIRE NETWORK vs. NON-ATTAINMENT AREA
    if label == label.upper():
        trip_stat_label_fonts.append(red_bold_font)
        trip_stat_label_alignments.append(align_h_left)
    # special font for categories (and centered)
    elif label in ["Person Trips", "Transit Share", "Trip Distance", "Trip Duration", "Other Trips", "Vehicle Class VMT"]:
        trip_stat_label_fonts.append(category_font)
        trip_stat_label_alignments.append(align_h_center)
    # normal font for everything else
    else:
        trip_stat_label_fonts.append(normal_font)
        trip_stat_label_alignments.append(align_h_left)

# function that takes the database folder of a scenario and returns a list of all of the trip statistics values with appropriate spacing
def trip_stat_value_func(database_folder):
    df = pd.read_csv(os.path.join(database_folder, "report", "final_run_statistics.csv"))

    # convert strings to numbers if needed
    values = df["value"].apply(str2num).to_list()

    return values

# call the comparison table helper function to create the trip statistics table
create_comparison_table(
    ws = ws, 
    labels = trip_stat_labels, 
    value_func = trip_stat_value_func, 
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years,
    label_fills = trip_stat_label_fills,
    label_fonts = trip_stat_label_fonts,
    label_alignments = trip_stat_label_alignments,
    number_formats = trip_stat_number_formats,
    prct_diff_thresholds = trip_stat_prct_diff_thresholds,
    abs_diff_thresholds = trip_stat_abs_diff_thresholds
)

# add charts to worksheet
trip_stat_charts = [
    {
        "title": stat,
        "stat_row": trip_stat_labels.index(stat)+3
    }
    for stat in ["Auto VMT", "Heavy Truck VMT", "All VMT"]
]
add_charts(
    ws = ws,
    chart_defs = trip_stat_charts,
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years
)

####################################################################################################################################################
#
# vmt statistics sheet
#
####################################################################################################################################################

ws = wb.create_sheet("VMT statistics")
ws.freeze_panes = 'B3'

# define labels and aethetics
vmt_stat_labels = []
vmt_stat_label_fills = []
vmt_stat_label_fonts = []
vmt_stat_label_alignments = []
# vmt statistics have a consistent pattern for each geography so add data in loop through geogs
geogs = pd.read_csv(os.path.join(run1.scenarios[comparison_years[0][0]]["path"], "report", "vmt_statistics.csv"))["Geography"].unique().tolist()
geogs.extend(["Entire Network", "CMAP 7-County"])
fills = [light_blue_fill, light_yellow_fill, light_green_fill, light_red_fill, light_gray_fill, light_purple_fill]
fill_i = 0
for geog in geogs:
    # add the geography name
    vmt_stat_labels.append(geog)
    # add the road types for each geogrphy
    vmt_stat_labels.extend(["Expressway VMT", "Arterial VMT", "Ramp/Toll VMT", "Centroid VMT", "Total District VMT"])
    # loop through the fills defined in the list above
    vmt_stat_label_fills.extend([fills[fill_i]]*6)
    fill_i += 1
    if fill_i == len(fills):
        fill_i = 0
    # use the category font for the geography name, normal font for the road types
    vmt_stat_label_fonts.append(category_font)
    vmt_stat_label_fonts.extend([normal_font]*5)
    vmt_stat_label_alignments.append(align_h_center)
    vmt_stat_label_alignments.extend([align_h_left]*5)
vmt_stat_number_formats = ["#,##0"]*len(vmt_stat_labels)
vmt_stat_prct_diff_thresholds = [0.002]*len(vmt_stat_labels)

# function that takes the database folder of a scenario and returns a list of all of the vmt statistic values with appropriate spacing
def vmt_stat_value_func(database_folder):
    df = pd.read_csv(os.path.join(database_folder, "report", "vmt_statistics.csv"))
    # convert strings to numbers if needed
    df["VMT"] = df["VMT"].apply(str2num)

    # add summaries of entire network and 7-county CMAP region
    entire_network_df = df.groupby("Road Type", sort = False, as_index = False).sum()
    entire_network_df["Geography"] = "Entire Network"
    cmap_county_df = df[df["Geography"].isin(['Chicago', 'Cook balance', 'DuPage', 'Kane', 'Kendall', 'Lake', 'McHenry', 'Will'])].groupby("Road Type", sort = False, as_index = False).sum()
    cmap_county_df["Geography"] = "CMAP 7-County"

    vmts = pd.concat([df, entire_network_df, cmap_county_df])["VMT"].tolist()

    # add spacing row for each geography label
    values = []
    i = 0
    for vmt in vmts:
        if i == 0:
            values.append("")
        values.append(vmt)
        i += 1
        if i == 5:
            i = 0

    return values

# call the comparison table helper function to create the VMT statistics table
create_comparison_table(
    ws = ws, 
    labels = vmt_stat_labels, 
    value_func = vmt_stat_value_func, 
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years,
    label_fills = vmt_stat_label_fills,
    label_fonts = vmt_stat_label_fonts,
    label_alignments = vmt_stat_label_alignments,
    number_formats = vmt_stat_number_formats,
    prct_diff_thresholds = vmt_stat_prct_diff_thresholds
)

####################################################################################################################################################
#
# transit statistics sheet
#
####################################################################################################################################################

ws = wb.create_sheet("transit statistics")
ws.freeze_panes = 'B3'

# define labels and aethetics
transit_stat_labels = []
transit_stat_label_fills = []
transit_stat_label_fonts = []
transit_stat_label_alignments = []
transit_stat_prct_diff_thresholds = []
transit_stat_abs_diff_thresholds = []
fills = [light_blue_fill, light_yellow_fill, light_green_fill, light_red_fill, light_gray_fill, light_purple_fill]
fill_i = 0
# add labels for each type of transit stat
for stat in ["Directional Miles", "Service Miles", "Service Hours"]:
    transit_stat_labels.append(stat)
    # add a row for each agency
    transit_stat_labels.extend(["CTA", "Pace", "CTA Rail", "Metra"])
    transit_stat_label_fills.extend([fills[fill_i]]*5)
    # loop through the fills defined in the list above
    fill_i += 1
    if fill_i == len(fills):
        fill_i = 0
    # use category font for transit stat type, normal font for agency name
    transit_stat_label_fonts.append(category_font)
    transit_stat_label_fonts.extend([normal_font]*4)
    transit_stat_label_alignments.append(align_h_center)
    transit_stat_label_alignments.extend([align_h_left]*4)
    # check for any differences in service miles
    if stat == "Service Miles": 
        transit_stat_prct_diff_thresholds.extend([0]*5)
        transit_stat_abs_diff_thresholds.extend([0]*5)
    else:
        transit_stat_prct_diff_thresholds.extend([None]*5)
        transit_stat_abs_diff_thresholds.extend([None]*5)


# function that takes the database folder of a scenario and returns a list of all of the transit statistic values with appropriate spacing
def transit_stat_value_func(database_folder):
    df = pd.read_csv(os.path.join(database_folder, "report", "transit_statistics.csv"))
    # convert strings to numbers if needed
    df["value"] = df["value"].apply(str2num)

    stats = df["value"].tolist()

    # add spacing
    values = []
    i = 0
    for stat in stats:
        if i == 0:
            values.append("")
        values.append(stat)
        i += 1
        if i == 4:
            i = 0

    return values

# call the comparison table helper function to create the transit statistics table
create_comparison_table(
    ws = ws, 
    labels = transit_stat_labels, 
    value_func = transit_stat_value_func, 
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years,
    label_fills = transit_stat_label_fills,
    label_fonts = transit_stat_label_fonts,
    label_alignments = transit_stat_label_alignments,
    prct_diff_thresholds = transit_stat_prct_diff_thresholds,
    abs_diff_thresholds = transit_stat_abs_diff_thresholds
)

# add charts to worksheet
transit_stat_charts = [
    {
        "title": f"{agency} Service Miles",
        "stat_row": 3+transit_stat_labels[transit_stat_labels.index("Service Miles"):].index(agency)+transit_stat_labels.index("Service Miles")
    }
    for agency in ["CTA", "Pace", "CTA Rail", "Metra"]
]
add_charts(
    ws = ws,
    chart_defs = transit_stat_charts,
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years
)

####################################################################################################################################################
#
# socec statistics sheet
#
####################################################################################################################################################

ws = wb.create_sheet("socec statistics")
ws.freeze_panes = 'B3'

# define labels and aesthetics
socec_stat_labels = ["households", "workers", "retail emp", "total emp", "population", "adults", "children", "HW prod", "HW attr", "HO prod", "HO attr", "NH prod", "NH attr"]
socec_stat_label_fills = [white_fill]*len(socec_stat_labels)
socec_stat_label_fonts = [normal_font]*len(socec_stat_labels)
socec_stat_label_alignments = [align_h_left]*len(socec_stat_labels)
socec_stat_number_formats = ["#,##0"]*len(socec_stat_labels)
socec_stat_prct_diff_thresholds = [0]*len(socec_stat_labels)
socec_stat_abs_diff_thresholds = [0]*len(socec_stat_labels)

# function that takes the database folder of a scenario and returns a list of all of the socec statistic values with appropriate spacing
def socec_stat_value_func(database_folder):
    # create list of values in the same order as the labels
    input_df = pd.read_csv(os.path.join(database_folder, "tg", "reports", "tg_rpt_input.csv"))
    values = input_df.loc[input_df["cmap"]=="All", ["households", "workers", "retail employment", "total employment", "population", "adults", "children"]].iloc[0].tolist()
    output_df = pd.read_csv(os.path.join(database_folder, "tg", "reports", "tg_rpt_output.csv"))
    values.extend(output_df.loc[output_df["cmap"]=="All", ["sum of home-based work productions", "sum of home-based work attractions", "sum of home-based other productions", "sum of home-based other attractions", "sum of non-home based productions", "sum of non-home based attractions"]].iloc[0].tolist())

    return values

# call the comparison table helper function to create the socec statistics table
create_comparison_table(
    ws = ws, 
    labels = socec_stat_labels, 
    value_func = socec_stat_value_func, 
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years,
    label_fills = socec_stat_label_fills,
    label_fonts = socec_stat_label_fonts,
    label_alignments = socec_stat_label_alignments,
    number_formats = socec_stat_number_formats,
    prct_diff_thresholds = socec_stat_prct_diff_thresholds,
    abs_diff_thresholds = socec_stat_abs_diff_thresholds
)

# add charts
socec_stat_charts = [
    {
        "title": stat,
        "stat_row": socec_stat_labels.index(stat)+3
    }
    for stat in ["households", "population", "workers"]
]
socec_stat_charts.extend([
    {
        "title": f"{purpose} P-A",
        "stat_row": [socec_stat_labels.index(f"{purpose} prod")+3, socec_stat_labels.index(f"{purpose} attr")+3],
        "split_labels": ["prod", "attr"]
    }
    for purpose in ["HW", "HO", "NH"]
])
add_charts(
    ws = ws,
    chart_defs = socec_stat_charts,
    run1 = run1,
    run2 = run2,
    comparison_years = comparison_years
)

####################################################################################################################################################
#
# save finished workbook
#
####################################################################################################################################################

# remove "Sheet" created by default
wb.remove(wb.worksheets[0])
# save
wb.save(f"{output_dir}test_comparison_{run1.name}_to_{run2.name}.xlsx")
