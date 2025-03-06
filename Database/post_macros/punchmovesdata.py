# PUNCHMOVESDATA.PY
# Tim OLeary - 9/15/2023
# New punch moves python script. Does the same work as 
# the macro 'punch.moves.data.mac', with minor update 
# for c23q2 onward.
#
#########################################################

# Note: right now, this script is stored under the post_macros 
# folder. look at comments in 'inputs' section below for 
# places to change relative filepaths

#################################
## -- INPUTS/IMPORT MODULES -- ##
#################################
print('Starting Emme... ')

#libraries
import os, sys, yaml, textwrap
import pandas as pd
from pathlib import Path


#initialize emme
cmap_tbm_dir =  next(tbmdir for tbmdir in Path(__file__).parents if tbmdir.name.endswith('cmap_trip-based_model')).resolve()
sys.path.append(os.path.join(cmap_tbm_dir, 'Scripts'))
from tbmtools import project as tbm
modeller = tbm.connect(cmap_tbm_dir)
emmebank = modeller.emmebank

#setup output locations
db = next(dbdir for dbdir in Path(__file__).parents if dbdir.name == "Database").resolve()
punchlink_out = os.path.join(db, 'data/punchlink.csv')


#grab info from batch_file.yaml
with open(os.path.join(db, 'batch_file.yaml')) as f:
    lines_without_backslashes = ''.join([line.replace('\\','/') for line in f])
    config = yaml.safe_load(lines_without_backslashes)

rsp_flag = config['RSP'] #True or False

#emme tools
net_calc = modeller.tool('inro.emme.network_calculation.network_calculator')
create_attribute = modeller.tool('inro.emme.data.extra_attribute.create_extra_attribute')
copy_scenario = modeller.tool('inro.emme.data.scenario.copy_scenario')
delete_scenario = modeller.tool('inro.emme.data.scenario.delete_scenario')

# --

###############################
## -- EXECUTE PUNCH MOVES -- ##
###############################

print('EXTRACT ROADWAY LINK ATTRIBUTES')

## --
## -- DEFINE SPECS FOR FLAGGING TOLL LINKS
## --

#zero out ui1
ui1spec = {
    "result": "ui1",
    "expression": "0",
    "selections": {"node":"all"},
    "type": "NETWORK_CALCULATION"
}

#zero out ui2
ui2spec = {
    "result":"ui2",
    "expression":"0",
    "selections": {"node":"all"},
    "type": "NETWORK_CALCULATION"
}

#flag ramps (vdf 3,5,8) in ui1
ui1spec2 = {
    "result": "ui1",
    "expression": "vdf==3 .or. vdf==5 .or. vdf==8",
    "aggregation": ".max.",
    "selections": {"link": "all"},
    "type": "NETWORK_CALCULATION"
}

#flag ramps (vdf 3,5,8) in ui2
uj2spec2 = {
    "result": "uj2",
    "expression": "vdf==3 .or. vdf==5 .or. vdf==8",
    "aggregation": ".max.",
    "selections": {"link": "all"},
    "type": "NETWORK_CALCULATION"
}

#flag toll links (vdf=7) on ramps (vdf 3,5,8)
#based on whether a ramp exists on either side of the link
tmpl2spec = '''
{
    "result": "@tmpl2",
    "expression": "(ui2+uj1).ge.2",
    "selections": {"link": "vdf=7"},
    "type": "NETWORK_CALCULATION"
}
'''

#list of attributes to extract for punchlink
desired_links = '''\
    "length+lanes+vdf+\
    @zone+@emcap+timau+\
    @ftime+@avauv+@avh2v+\
    @avh3v+@avbqv+@avlqv+\
    @avmqv+@avhqv+@busveq+\
    @atype+@imarea+@tmpl2+\
    @speed+@m200+@h200"\
'''

if rsp_flag:
    desired_links = '''\
    "length+lanes+vdf+\
    @zone+@emcap+timau+\
    @ftime+@avauv+@avh2v+\
    @avh3v+@avbqv+@avlqv+\
    @avmqv+@avhqv+@busveq+\
    @atype+@imarea+@tmpl2+\
    @speed+@m200+@h200+\
    @ejauto+@ejtruck+\
    @ccrauto+@ccrtruck"\
'''

desired_links = ''.join(textwrap.dedent(desired_links).split()) #put in single line for formatting into emme tool

## --
## -- GRAB LINK DATA FOR EACH TIME PERIOD
## -- 

#container to hold dataframe for each timeperiod
df_list = []

#iterate through each time period (1-8)
for tp in range(1,9):
    print(f'  -- Obtaining link data for time period {tp}...')

    copy_scenario(
        from_scenario=tp,
        scenario_id=99998,
        scenario_title='temp copy for MOVES',
        overwrite=True
    )

    #flag ramp links
    net_calc(specification=[ui1spec, ui2spec], scenario=emmebank.scenario(99998))
    net_calc(specification=[ui1spec2,uj2spec2], scenario=emmebank.scenario(99998))
    
    #create attribute @tmpl2 to hold toll flag
    create_attribute(
        extra_attribute_type='LINK',
        extra_attribute_name='@tmpl2',
        extra_attribute_description='toll link on ramp',
        extra_attribute_default_value=0,
        overwrite=True,
        scenario=emmebank.scenario(99998)
    )

    #flag toll links on ramps
    net_calc(specification=tmpl2spec, scenario=emmebank.scenario(99998))

    #specification for extracting link data
    spec_linkdata = f'''
    {{
        "expression": {desired_links},
        "selections": {{"link":"all"}},
        "type": "NETWORK_CALCULATION"
    }}
    '''
    #network calculation to extract link data
    linkdata_tp = net_calc(
        specification=spec_linkdata, 
        scenario=emmebank.scenario(99998), 
        full_report=True
    )

    #convert table to pandas dataframe
    header = linkdata_tp['table'][0]
    data = linkdata_tp['table'][1:]

    linkdata_tp_df = pd.DataFrame(data=data, columns=header)
    linkdata_tp_df['timeperiod'] = tp
    
    #add dataframe for timeperiod 'tp' to df_list
    df_list.append(linkdata_tp_df)

## --
## -- COMBINE TABLES, CLEAN UP, AND EXPORT
## --

print('Final touches...')

delete_scenario(scenario=emmebank.scenario(99998))

#concatenate all tp dataframes together
linkdata = pd.concat(df_list, ignore_index=True)

#clean up column names (removing '@' sign)
columns=linkdata.columns.tolist()
rename={}
for c in columns:
    rename[c] = c.replace('@','')
linkdata.rename(columns=rename, inplace=True)

#export to csv
linkdata.to_csv(punchlink_out, index=False)
print(f'Done! Output csv located here: {punchlink_out}')