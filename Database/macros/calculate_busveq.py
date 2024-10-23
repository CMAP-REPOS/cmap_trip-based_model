
def busveq(tod, empFile, scen, scen_txt, tod_mult):
    #import libraries
    import os
    import csv
    import pandas as pd
    import inro.modeller as _m
    modeller = _m.Modeller()
    emmebank = modeller.emmebank
    
    #modeller tools
    export_transit_lines = modeller.tool("inro.emme.data.network.transit.export_transit_lines")
    import_extra_attributes = modeller.tool("inro.emme.data.extra_attribute.import_extra_attributes")

    print('  --calculating @busveq')
    
    #TOLEARY - 08/2024 - busveq calculation
    # transit asmt networks matched to highway tod periods
    # 321: Night (6pm-6am)
    # 323: AM (6am-9am)
    # 325: Midday (9am-4pm)
    # 327: PM (4pm-6pm)

    # -- for # hours per period, use tod_mult from before; reminder below --
    # 1: 8pm-6am --> 321 (Night) --> 10 hr, but code as 5
    # 2: 6am-7am --> 323 (AM) --> 1 hr
    # 3: 7am-9am --> 323 (AM) --> 2 hr
    # 4: 9am-10am --> 325 (Midday) --> 1 hr
    # 5: 10am-2pm --> 325 (Midday) --> 4 hr
    # 6: 2pm-4pm --> 325 (Midday) --> 2 hr
    # 7: 4pm-6pm --> 327 (PM) --> 2 hr
    # 8: 6pm-8pm --> 321 (Night) --> 2 hr

    #get scenario year from scen_txt (e.g., '3' from '30002')
    yr = str(scen_txt)[0]

    #map hwy time-of-day period to transit scenario number
    hwytod_trntod = {
        '1': f'{yr}21',
        '2': f'{yr}23',
        '3': f'{yr}23',
        '4': f'{yr}25',
        '5': f'{yr}25',
        '6': f'{yr}25',
        '7': f'{yr}27',
        '8': f'{yr}21'
    }


    scen_to_export = hwytod_trntod[str(tod)]
    print(f'tod {tod}, trnt tod {scen_to_export}')
    
    trnt_network_scen = emmebank.scenario(scen_to_export)
    trnt_net_export = os.path.join(os.path.dirname(empFile), f'Database/cache/{scen_txt}_trntlines')

    export_transit_lines(
        selection='all',
        export_file=trnt_net_export,
        append_to_file=False,
        field_separator=';',
        export_format='PROMPT_DATA_FORMAT',
        scenario=trnt_network_scen,
        include_first_hidden_data=True
    )

    #grab the data from the transit network --
    #this looks like a list of dictionaries of the format:
    # {
    #     'Line': <Transit Line ID>,
    #     'Headwy': <Transit Line Headway>,
    #     'Links': [[node_a, node_b], [node_b, node_c], ..., [node_y, node_z]]
    # }

    with open(trnt_net_export, 'r') as file:
        reader = csv.reader(file, delimiter=';')
        
        line_data = []
        
        for row in reader:
            if not row:  # Skip empty lines
                continue
                
            #cleanup leading/trailing whitespaces
            row = [element.strip() for element in row]
            
            if row[0].startswith('c'):
                continue  # ignore comment lines
            
            index = []
            if row[0].startswith("a"):
                # Extract the line value (second element in row) and headway (fourth element)
                line = row[0].split("'")[1]
                headwy = row[3]
                data = {'Line': line, 'Headwy': headwy, 'Links': []}
                line_data.append(data)
                prev_node = None
            
            elif row[0].isdigit():
                # Extract the first number from the row
                if prev_node is None:
                    prev_node = row[0]
                else:
                    if line_data:
                        line_data[-1]['Links'].append([prev_node,row[0]])
                        prev_node=row[0]

    #now, create dataframe from `line_data`
    #data will be in format pandas can handle: 
    # lines_df = [[<inode>,<jnode>,<transit_line>,<headway>], ...]

    lines_df = []
    column_headers=['inode','jnode','line','hdwy']

    for line in line_data:
        for link in line['Links']:
            inode=link[0]
            jnode=link[1]
            line0=line['Line']
            hdwy=line['Headwy']
            row=[inode,jnode,line0,hdwy]
            lines_df.append(row)

    #create dataframe
    data = pd.DataFrame(lines_df, columns=column_headers)      
    data['hdwy'] = data['hdwy'].astype(float)  

    # calculate vehicles per hour (60/headway), 
    # then busveq (vehicles per hour * # hours * 3)
    data.eval('vehperhr = 60 / hdwy', inplace=True)
    data['@busveq'] = data['vehperhr'] * tod_mult * 3
    data.sort_values(['inode','jnode'], inplace=True)

    #add up all busveqs from all lines, by link
    busveq_df = data.groupby(['inode','jnode']).agg({'@busveq':'sum'}).reset_index()

    #create batchin file for highway scenario
    database_cache = os.path.join(os.path.dirname(empFile), 'Database/cache')
    batchin_file = os.path.join(database_cache, f'busveq_batchin_{scen_txt}')

    if os.path.exists(batchin_file):
        os.remove(batchin_file)
        
    # Write the DataFrame to an emme transaction file
    with open(batchin_file, 'w', newline='') as file:
        # Write the header
        file.write('t extra_attributes\n')
        file.write("@busveq LINK 0.0 'bus volume veh equivalent'\n")
        file.write('end extra_attributes\n')
        #column names    
        file.write(f'  inode  jnode   @busveq\n')
        # Write the DataFrame rows with the desired structure
        for index, row in busveq_df.iterrows():
            #row[0] => inode, row[1] => jnode, row[2] => busveq
            file.write(f'{row[0]:>7}{row[1]:>7}{round(row[2],2):>10}\n')

    #throw data into hwy scenario
    import_extra_attributes(
        file_path=batchin_file,
        revert_on_error=False,
        scenario=scen,
        import_definitions=True
    )

    os.remove(trnt_net_export)
    os.remove(batchin_file)
    
if __name__ == '__main__':
    busveq()