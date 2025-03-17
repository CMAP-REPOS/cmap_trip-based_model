import sys
import pandas as pd
import openpyxl

emission_rates_file = sys.argv[1]
year = 2050
out_pkl = sys.argv[2]


#input emission rate files
emission_data_cols = year-2023
GHG = pd.read_excel(emission_rates_file, sheet_name='All Veh GHG UA running', skiprows=[0,2], usecols=[1, emission_data_cols])
NOx = pd.read_excel(emission_rates_file, sheet_name='All Veh NOx UA running', skiprows=[0,2], usecols=[1, emission_data_cols])
PM = pd.read_excel(emission_rates_file, sheet_name='All Veh PM UA running', skiprows=[0,2], usecols=[1, emission_data_cols])
VOC = pd.read_excel(emission_rates_file, sheet_name='All Veh VOC UA running', skiprows=[0,2], usecols=[1, emission_data_cols])

GHG.columns = ['Speed', 'GHG_ER']
NOx.columns = ['Speed', 'NOx_ER']
PM.columns = ['Speed', 'PM_ER']
VOC.columns = ['Speed', 'VOC_ER']

pollutants = GHG.merge(NOx, on='Speed') \
            .merge(PM, on='Speed') \
            .merge(VOC, on='Speed')

pollutants.to_pickle(out_pkl)