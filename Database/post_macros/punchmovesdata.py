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
import os
import numpy as np, pandas as pd

#output locations
workspace = os.getcwd()            # <<-- 'Database' folder
output_location = workspace + '\\data'              # <<-- 'data' folder, where punchmoves has been written previously
punchlink_out = output_location + '\\punchlink.csv' # <<-- output csv name

#import emme desktop and initialize emme
import inro.emme.desktop.app as _app

#find emp file - may have unexpected results if more than one .emp file exists
e = os.listdir(os.path.dirname(workspace))
empfile = [os.path.join(os.path.dirname(workspace), file) for file in e if file.endswith('.emp')][0]

#start desktop
desktop = _app.start_dedicated(
    visible=False,
    user_initials="cmap",
    project=empfile
)

#import modeller
import inro.modeller as _m
modeller = _m.Modeller(desktop=desktop)
emmebank = modeller.emmebank

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
    @atype+@imarea+@tmpl2\
    @speed+@m200+@h200"\
'''

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

    #list of attributes to extract from links
    desired_links = '''"length+lanes+vdf+@zone+@emcap+timau+@ftime+@avauv+@avh2v+@avh3v+@avbqv+@avlqv+@avmqv+@avhqv+@busveq+@atype+@imarea+@tmpl2+@speed+@m200+@h200"'''
    
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
    if c.startswith('@'):
        rename[c] = c[1:]
linkdata.rename(columns=rename, inplace=True)

#export to csv
linkdata.to_csv(punchlink_out, index=False)
print(f'Done! Output csv located here: {punchlink_out}')