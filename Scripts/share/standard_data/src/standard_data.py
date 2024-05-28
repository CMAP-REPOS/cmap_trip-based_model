from pathlib import Path
import sys
import os
import logging
import shutil
import multiprocessing

from tqdm import tqdm
import yaml
from jinja2 import Environment, FileSystemLoader
import markdown

sys.path.append(str(Path(__file__).resolve().parents[3]))
import tbmtools.project as tbm
from tbmtools.results import vehicle_trips
from tbmtools.results import trip_roster
from tbmtools.results import person_trips
from tbmtools.results import skims
from tbmtools.results import transit_network
from tbmtools.results import highway_network
from tbmtools.results import sharing

src_dir = Path(__file__).resolve().parent
proj_dir = src_dir.parents[3]
out_dir = src_dir.parent.joinpath('output')

def add_tag(out_file_names, title, scenario_code):
    # Add tag and suffix to file names.
    tag = f'_{title}_{scenario_code}'
    tagged_file_names = {}
    for placeholder_variable, file_name in out_file_names.items():
        tagged_file_names[placeholder_variable] = file_name + tag
    return tagged_file_names

def load_config():
    with open(proj_dir.joinpath('Database/batch_file.yaml')) as f:
        batch_file_config = yaml.safe_load(f)
    with open(src_dir.parent.joinpath('hand/config.yaml')) as f:
        config = yaml.safe_load(f)
    config['scenario_code'] = batch_file_config['scenario_code']
    config['model_version'] = batch_file_config['model_version']
    return config

def export(project_file_name, out_file_names, scenario_code):
    # Make output directory.
    out_dir.mkdir(exist_ok=True)
    # Set up log file.
    logging.basicConfig(filename=out_dir.joinpath('export_data.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    
    print(f'Writing output to {out_dir}')

    # Start Modeller in the Emme project.
    modeller = tbm.connect(proj_dir.joinpath(project_file_name))
    logging.info(f'Connected to {modeller.desktop.project_file_name()}')
    # Display a progress bar while exporting data.
    with tqdm(desc='Exporting data', total=11) as pbar:
        # Export vehicle trips from emmebank.
        logging.info('Exporting vehicle trips')
        vehicle_trips.export_matrices(out_dir, modeller)
        pbar.update()
        # Export skims from emmebank.
        logging.info('Exporting skims')
        skims.flag_transit_disconnects(modeller)
        skims.export_matrices(out_dir, modeller)
        pbar.update()
        # Export trip roster from parquet files.
        logging.info('Exporting trip roster')
        trip_roster_path = trip_roster.export(proj_dir, out_dir, out_file_names['trip_roster'])
        out_file_names['trip_roster'] = trip_roster_path.name
        pbar.update()
        # Export auto person trips from trip roster and transit person trips
        # from emmebank.
        logging.info('Exporting person trips')
        person_trips.export_auto_matrices(proj_dir, out_dir, trip_roster_path)
        pbar.update()
        person_trips.export_transit_matrices(out_dir, modeller)
        pbar.update()
        # Export peak (AM peak) and off-peak (midday) transit network,
        # itineraries, and attributes as Emme transaction files and
        # shapefiles.
        logging.info('Exporting transit network')
        transit_network.export(out_dir,
                               scenario=int(scenario_code),
                               format='transaction',
                               modeller=modeller,
                               emmebank=modeller.emmebank)
        pbar.update()
        transit_network.export(out_dir,
                               scenario=int(scenario_code),
                               format='shape',
                               modeller=modeller,
                               emmebank=modeller.emmebank)
        pbar.update()
        # Export each time of day highway network and attributes as Emme
        # transaction files.
        logging.info('Exporting highway network')
        highway_network.export_by_tod(out_dir,
                                      scenario=int(scenario_code),
                                      format='transaction',
                                      modeller=modeller,
                                      emmebank=modeller.emmebank)
        pbar.update()
        # Export peak highway networks and attributes as shapefiles.
        highway_network.export_by_tod(out_dir,
                                    scenario=int(scenario_code),
                                    format='shape',
                                    modeller=modeller,
                                    emmebank=modeller.emmebank,
                                    period=[3, 7])
        pbar.update()
        # Export daily highway network and attributes as Emme transaction
        # files and shapefiles.
        highway_network.export_daily(out_dir,
                                     scenario=int(scenario_code),
                                     format='transaction',
                                     modeller=modeller,
                                     emmebank=modeller.emmebank)
        pbar.update()
        highway_network.export_daily(out_dir,
                                     scenario=int(scenario_code),
                                     format='shape',
                                     modeller=modeller,
                                     emmebank=modeller.emmebank)
        pbar.update()
    # Copy TG results to output directory.
    logging.info('Copying TG results')
    tg_dir = proj_dir.joinpath('Database/tg')
    file = sorted(tg_dir.joinpath('data').glob('tg_results*.csv'))[0]
    shutil.copy(file, out_dir)
    file_copy = out_dir.joinpath(file.name)
    out_file_names['tg_data'] += '.csv'
    renamed_copy = file_copy.with_name(out_file_names['tg_data'])
    if renamed_copy.exists(): 
        os.remove(renamed_copy)
    file_copy.rename(renamed_copy)
    # Copy productions and attractions to output subdirectory.
    logging.info('Copying productions and attractions')
    files = [tg_dir.joinpath('fortran/TRIP49_PA_OUT.TXT'),
             tg_dir.joinpath('fortran/TRIP49_PA_WFH_OUT.TXT')]
    out_dir.joinpath('prods_attrs').mkdir(exist_ok=True)
    for file in files:
        shutil.copy(file, out_dir.joinpath('prods_attrs'))
    logging.info('Finished.')
    return out_file_names

def compress(out_file_names, scenario_code, transit_dir):
    # Set up log file.
    logging.basicConfig(filename=out_dir.joinpath('compress_data.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    
    print(f'Writing compressed output to {out_dir}')

    # Relate destination paths to source paths.
    zip_sources = {'pa_tables': out_dir.joinpath('prods_attrs'),
                   'hov_trip_tables': out_dir.joinpath('trips/hov_trips'),
                   'trip_tables': out_dir.joinpath('trips'),
                   'highway_networks': out_dir.joinpath('networks/highway'),
                   'am_peak_highway_network': out_dir.joinpath(f'networks/highway/highway_ampk-{scenario_code}'),
                   'pm_peak_highway_network': out_dir.joinpath(f'networks/highway/highway_pmpk-{scenario_code}'),
                   'daily_highway_network': out_dir.joinpath(f'networks/highway/highway-{scenario_code}'),
                   'transit_networks': out_dir.joinpath('networks/transit'),
                   'tod_transit_networks': Path(transit_dir, str(scenario_code)),
                   'peak_transit_network': out_dir.joinpath(f'networks/transit/transit_pk-{scenario_code}'),
                   'offpeak_transit_network': out_dir.joinpath(f'networks/transit/transit_op-{scenario_code}'),
                   'skim_matrices': out_dir.joinpath('skims'),
                   'database': proj_dir.joinpath('Database/emmebank'),
                   'matrices': proj_dir.joinpath('Database/emmemat')}
    mp_compress_args = list()
    for placeholder_variable, source_path in zip_sources.items():
        out_file_names[placeholder_variable] += '.zip'
        mp_compress_args.append((out_file_names[placeholder_variable], source_path, out_dir))
    # Compress model data for sharing.
    logging.info('Compressing outputs')
    with multiprocessing.Pool() as pool:
        # Display a progress bar while processing the tasks.
        for i in tqdm(iterable=pool.imap_unordered(func=sharing.mp_compress,
                                                   iterable=mp_compress_args),
                      total=len(mp_compress_args),
                      desc='Compressing outputs'):
            pass
    logging.info('Finished.')
    return out_file_names

def document(context):
    # Load the Markdown template for data user guide.
    environment = Environment(loader=FileSystemLoader(src_dir.parent.joinpath('hand')))
    md_template = environment.get_template('data_user_guide_md.txt')
    # Render the Markdown template.
    md_file = out_dir.joinpath('data_user_guide.md')
    with open(md_file, mode='w', encoding='utf-8') as file:
        context.update(context.pop('out_file_names'))
        file.write(md_template.render(context))
    # Read Markdown from file.
    with open(md_file, encoding='utf-8') as file:
        md = file.read()
    # Convert Markdown to HTML.
    html = markdown.markdown(text=md, extensions=['tables'])
    # Load the HTML template for data user guide.
    html_template_file = src_dir.parent.joinpath('hand/data_user_guide_html.txt')
    with open(html_template_file, encoding='utf-8') as file:
        html_template = file.read()
    # Render the HTML template.
    with open(md_file.with_suffix('.html'), mode='w', encoding='utf-8') as file:
        file.write(html_template.replace('{{content}}', html))