## RSP_AM_truck_demand.py
# author: toleary
# creation date: 1/17/2025

# This script was written to output truck demand for the AM Peak 
# as part of the RCP evaluation process for the 2026 ON TO 2050 Update.
#   - For each time of day, truck trip demand is calculated and stored
#     in 4 temporary matrices: mf14-mf17. 
#   - This script copies these temporary matrices in a new location for RSP analysis: 
#        temp matrix  |  copied matrix
#           mf14           mf34: b plate truck trips for p3. 7am-9am
#           mf15           mf35: light truck trips for p3. 7am-9am
#           mf16           mf36: medium truck trips for p3. 7am-9am
#           mf17           mf37: heavy truck trips for p3. 7am-9am
#
#           mf14           mf30: b plate truck trips for p7. 4pm-6pm
#           mf15           mf31: light truck trips for p7. 4pm-6pm
#           mf16           mf32: medium truck trips for p7. 4pm-6pm
#           mf17           mf33: heavy truck trips for p7. 4pm-6pm

import os
import sys
from pathlib import Path

cmap_tbm_dir =  next(tbmdir for tbmdir in Path(__file__).parents if tbmdir.name.endswith('cmap_trip-based_model')).resolve()
sys.path.append(os.path.join(cmap_tbm_dir, 'Scripts'))
from tbmtools import project as tbm

modeller = tbm.connect(cmap_tbm_dir)
emmebank = modeller.emmebank

#import variables and define tools
yr = str(sys.argv[1])  #emme scenario number -- %val% in batch
tod = int(sys.argv[2])   #time of day -- %tod_cntr% in batch
glob_iter = int(sys.argv[3]) #global iteration -- %counter% in batch

copy_matrix = modeller.tool("inro.emme.data.matrix.copy_matrix")

scen = emmebank.scenario(f'{yr}{glob_iter}{tod}')

AMmatrices=[
    #[old_mf, new_mf, new_name, new_description],
    ['mf14', 'mf34', 'bplt_tod3', 'b plate truck trips for p3. 7am-9am'],
    ['mf15', 'mf35', 'ltrk_tod3', 'light truck trips for p3. 7am-9am'],
    ['mf16', 'mf36', 'mtrk_tod3', 'medium truck trips for p3. 7am-9am'],
    ['mf17', 'mf37', 'htrk_tod3', 'heavy truck trips for p3. 7am-9am']
]
PMmatrices=[
    #[old_mf, new_mf, new_name, new_description],
    ['mf14', 'mf30', 'bplt_tod7', 'b plate truck trips for p7. 4pm-6pm'],
    ['mf15', 'mf31', 'ltrk_tod7', 'light truck trips for p7. 4pm-6pm'],
    ['mf16', 'mf32', 'mtrk_tod7', 'medium truck trips for p7. 4pm-6pm'],
    ['mf17', 'mf33', 'htrk_tod7', 'heavy truck trips for p7. 4pm-6pm']
]

if int(tod)==3:
    matrices = AMmatrices
elif int(tod)==7:
    matrices = PMmatrices

for spec in matrices:
    copy_matrix(
        from_matrix = spec[0],
        matrix_id = spec[1],
        matrix_name = spec[2],
        matrix_description = spec[3],
        scenario=scen
    )
    print(spec[0], ' copied to ', spec[1])