from pathlib import Path
import sys
import os
import logging
import shutil
import multiprocessing

from tqdm import tqdm
from jinja2 import Environment, FileSystemLoader

sys.path.append(str(Path(__file__).resolve().parents[3]))
import tbmtools.project as tbm
from tbmtools.results import vehicle_trips
from tbmtools.results import trip_roster
from tbmtools.results import person_trips
from tbmtools.results import skims
from tbmtools.results import transit_network
from tbmtools.results import highway_network
from tbmtools.results import sharing


def export(project_file, title, scenario_code):
    # Set working directory and define relative paths to the Emme
    # project folder and output destination.
    src_dir = Path(sys.argv[0]).parent
    os.chdir(str(src_dir))
    proj_dir = Path('../../../..').resolve()
    out_dir = Path('../output').resolve()
    # Make output directory.
    out_dir.mkdir(exist_ok=True)
    # Set up log file.
    logging.basicConfig(filename=out_dir.joinpath('export_data.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    print(f'Writing output to {out_dir}')
    # Start Modeller in the Emme project.
    modeller = tbm.connect(project_file)
    logging.info(f'Connected to {modeller.desktop.project_file_name()}')
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
        trip_roster.export(proj_dir, out_dir)
        pbar.update()
        # Export auto person trips from trip roster and transit person trips
        # from emmebank.
        logging.info('Exporting person trips')
        person_trips.export_auto_matrices(proj_dir, out_dir)
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
    dir = proj_dir.joinpath('Database', 'tg', 'data')
    tg_data_filename = f'tg_results_{title}_{scenario_code}.csv'
    file = sorted(dir.glob('tg_results*.csv'))[0]
    shutil.copy(file, out_dir)
    file_copy = out_dir.joinpath(file.name)
    renamed_copy = file_copy.with_name(tg_data_filename)
    if renamed_copy.exists(): 
        os.remove(renamed_copy)
    file_copy.rename(renamed_copy)
    # Copy productions and attractions to output subdirectory.
    logging.info('Copying productions and attractions')
    files = sorted(dir.glob('*.in'))
    out_dir.joinpath('prods_attrs').mkdir(exist_ok=True)
    for file in files:
        shutil.copy(file, out_dir.joinpath('prods_attrs'))

    logging.info('Finished.')

def compress(title, scenario_code, transit_dir):
    # Set working directory and define relative paths to the Emme
    # project folder and output destination.
    src_dir = Path(sys.argv[0]).parent
    os.chdir(str(src_dir))
    proj_dir = Path('../../../..').resolve()
    out_dir = Path('../output').resolve()
    # Make output directory.
    out_dir.mkdir(exist_ok=True)
    # Set up log file.
    logging.basicConfig(filename=out_dir.joinpath('compress_data.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    print(f'Writing compressed output to {out_dir}')

    # Compress model data for sharing.
    tag = f'_{title}_{scenario_code}'
               # Productions and attractions.
    ziplist = [(out_dir.joinpath(f'prods_attrs{tag}.zip'),
                out_dir.joinpath('prods_attrs')),
               # Trips.
               (out_dir.joinpath(f'trips{tag}.zip'),
                out_dir.joinpath('trips')),
               # Work trips.
               (out_dir.joinpath(f'worktrips{tag}.zip'),
                out_dir.joinpath('trips', 'work_trips')),
               # HOV trips.
               (out_dir.joinpath(f'hovtrips{tag}.zip'),
                out_dir.joinpath('trips', 'hov_trips')),
               # Highway network transaction files.
               (out_dir.joinpath(f'emmenet_highway{tag}.zip'),
                out_dir.joinpath('networks', 'highway')),
               # AM peak highway network shapefile.
               (out_dir.joinpath(f'highwayshp_ampk{tag}.zip'),
                out_dir.joinpath('networks', 'highway', f'highway_ampk-{scenario_code}')),
               # PM peak highway network shapefile.
               (out_dir.joinpath(f'highwayshp_pmpk{tag}.zip'),
                out_dir.joinpath('networks', 'highway', f'highway_pmpk-{scenario_code}')),
               # Daily highway network shapefile.
               (out_dir.joinpath(f'highwayshp{tag}.zip'),
                out_dir.joinpath('networks', 'highway', f'highway-{scenario_code}')),
               # Modeled transit network transaction files.
               (out_dir.joinpath(f'emmenet_transit{tag}.zip'),
                out_dir.joinpath('networks', 'transit')),
               # Transit network transaction files by time of day period.
               (out_dir.joinpath(f'emmenet_transit_tod{tag}.zip'),
                Path(transit_dir).joinpath(str(scenario_code))),
               # Peak transit network shapefile.
               (out_dir.joinpath(f'transitshp_pk{tag}.zip'),
                out_dir.joinpath('networks', 'transit', f'transit_pk-{scenario_code}')),
               # Off-peak transit network shapefile.
               (out_dir.joinpath(f'transitshp_op{tag}.zip'),
                out_dir.joinpath('networks', 'transit', f'transit_op-{scenario_code}')),
               # Skims.
               (out_dir.joinpath(f'skims{tag}.zip'),
                out_dir.joinpath('skims')),
               # Emme matrix directory.
               (out_dir.joinpath(f'emmemat{tag}.zip'),
                proj_dir.joinpath('Database', 'emmemat')),
               # Emme databank.
               (out_dir.joinpath(f'emmebank{tag}.zip'),
                proj_dir.joinpath('Database', 'emmebank'))]
    logging.info('Compressing outputs')
    with multiprocessing.Pool() as pool:
        list(tqdm(pool.imap_unordered(sharing.compress, ziplist), total=len(ziplist), desc='Compressing outputs'))
    logging.info('Finished.')

def document(context):
    # Set anchor paths.
    src_dir = Path(__file__).resolve().parent
    out_dir = src_dir.parent.joinpath('output')
    # Add tag and suffix to file names.
    out_filenames = context.pop('out_filenames')
    tag = f"_{context['model_version']}_{context['scenario_code']}"
    for placeholder_variable, filename in out_filenames.items():
        if placeholder_variable == 'tg_data':
            suffix = '.csv'
        else:
            suffix = '.zip'
        out_filenames[placeholder_variable] = filename + tag + suffix
    context.update(out_filenames)
    # Load template for data user guide.
    environment = Environment(loader=FileSystemLoader(src_dir.parent.joinpath('hand')))
    user_guide_template = environment.get_template('data_user_guide.txt')
    # Render the template.
    user_guide_file = out_dir.joinpath('data_user_guide.md')
    with open(user_guide_file, mode='w', encoding='utf-8') as user_guide:
        user_guide.write(user_guide_template.render(context))