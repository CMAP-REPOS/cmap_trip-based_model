import argparse
from pathlib import Path
import sys
import os
import logging
import shutil
import multiprocessing
from tqdm import tqdm
import tbmtools.project as tbm
from tbmtools.results import vehicle_trips
from tbmtools.results import trip_roster
from tbmtools.results import person_trips
from tbmtools.results import skims
from tbmtools.results import transit_network
from tbmtools.results import highway_network
from tbmtools.results import sharing

def main():
    # Parse arguments and verify Emme project file.
    parser = argparse.ArgumentParser(description='export standard model data')
    parser.add_argument('--project_file', type=tbm.emme_project_file, help='path to Emme project file')
    parser.add_argument('--title', help='modeled scenario title')
    parser.add_argument('--year', help='modeled scenario year')
    parser.add_argument('--transit_dir', help='path to transit directory with TOD network transaction files')
    args = parser.parse_args()
    # Set working directory and define relative paths to the Emme
    # project folder and output destination.
    scriptdir = Path(sys.argv[0]).parent
    os.chdir(str(scriptdir))
    projdir = Path('../..').resolve()
    outdir = Path('../output').resolve()
    # Make output directory.
    outdir.mkdir(exist_ok=True)
    # Set up log file.
    logging.basicConfig(filename=outdir.joinpath('export_model_data.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    print(f'Writing output to {outdir}')
    # Start Modeller in the Emme project.
    modeller = tbm.connect(args.project_file)
    logging.info(f'Connected to {modeller.desktop.project_file_name()}')
    with tqdm(desc='Exporting data', total=11) as pbar:
        # Export vehicle trips from emmebank.
        logging.info('Exporting vehicle trips')
        vehicle_trips.export_matrices(outdir, modeller)
        pbar.update()
        # Export skims from emmebank.
        logging.info('Exporting skims')
        skims.flag_transit_disconnects(modeller)
        skims.export_matrices(outdir, modeller)
        pbar.update()
        # Export trip roster from parquet files.
        logging.info('Exporting trip roster')
        trip_roster.export(projdir, outdir)
        pbar.update()
        # Export auto person trips from trip roster and transit person trips
        # from emmebank.
        logging.info('Exporting person trips')
        person_trips.export_auto_matrices(projdir, outdir)
        pbar.update()
        person_trips.export_transit_matrices(outdir, modeller)
        pbar.update()
        # Export peak (AM peak) and off-peak (midday) transit network,
        # itineraries, and attributes as Emme transaction files and
        # shapefiles.
        logging.info('Exporting transit network')
        transit_network.export(outdir,
                            scenario=int(args.year),
                            format='transaction',
                            modeller=modeller,
                            emmebank=modeller.emmebank)
        pbar.update()
        transit_network.export(outdir,
                            scenario=int(args.year),
                            format='shape',
                            modeller=modeller,
                            emmebank=modeller.emmebank)
        pbar.update()
        # Export each time of day highway network and attributes as Emme
        # transaction files.
        logging.info('Exporting highway network')
        highway_network.export_by_tod(outdir,
                                    scenario=int(args.year),
                                    format='transaction',
                                    modeller=modeller,
                                    emmebank=modeller.emmebank)
        pbar.update()
        # Export peak highway networks and attributes as shapefiles.
        highway_network.export_by_tod(outdir,
                                    scenario=int(args.year),
                                    format='shape',
                                    modeller=modeller,
                                    emmebank=modeller.emmebank,
                                    period=[3, 7])
        pbar.update()
        # Export daily highway network and attributes as Emme transaction
        # files and shapefiles.
        highway_network.export_daily(outdir,
                                    scenario=int(args.year),
                                    format='transaction',
                                    modeller=modeller,
                                    emmebank=modeller.emmebank)
        pbar.update()
        highway_network.export_daily(outdir,
                                    scenario=int(args.year),
                                    format='shape',
                                    modeller=modeller,
                                    emmebank=modeller.emmebank)
        pbar.update()
    
    # Copy TG results to output directory.
    logging.info('Copying TG results')
    dir = projdir.joinpath('Database', 'tg', 'data')
    file = sorted(dir.glob('tg_results*.csv'))[0]
    shutil.copy(file, outdir)
    file_copy = outdir.joinpath(file.name)
    renamed_copy = file_copy.with_name(f'tg_results_{args.title}_{args.year}.csv')
    if renamed_copy.exists(): 
        os.remove(renamed_copy)
    file_copy.rename(renamed_copy)
    # Copy productions and attractions to output subdirectory.
    logging.info('Copying productions and attractions')
    files = sorted(dir.glob('*.in'))
    outdir.joinpath('prods_attrs').mkdir(exist_ok=True)
    for file in files:
        shutil.copy(file, outdir.joinpath('prods_attrs'))

    # Compress model data for sharing.
               # Productions and attractions.
    ziplist = [(outdir.joinpath(f'prods_attrs_{args.title}_{args.year}.zip'),
                outdir.joinpath('prods_attrs')),
               # Trips.
               (outdir.joinpath(f'trips_{args.title}_{args.year}.zip'),
                outdir.joinpath('trips')),
               # Work trips.
               (outdir.joinpath(f'worktrips_{args.title}_{args.year}.zip'),
                outdir.joinpath('trips', 'work_trips')),
               # HOV trips.
               (outdir.joinpath(f'hovtrips_{args.title}_{args.year}.zip'),
                outdir.joinpath('trips', 'hov_trips')),
               # Highway network transaction files.
               (outdir.joinpath(f'emmenet_highway_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'highway')),
               # AM peak highway network shapefile.
               (outdir.joinpath(f'highwayshp_ampk_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'highway', f'highway_ampk-{args.year}')),
               # PM peak highway network shapefile.
               (outdir.joinpath(f'highwayshp_pmpk_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'highway', f'highway_pmpk-{args.year}')),
               # Daily highway network shapefile.
               (outdir.joinpath(f'highwayshp_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'highway', f'highway-{args.year}')),
               # Modeled transit network transaction files.
               (outdir.joinpath(f'emmenet_transit_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'transit')),
               # Transit network transaction files by time of day period.
               (outdir.joinpath(f'emmenet_transit_tod_{args.title}_{args.year}.zip'),
                Path(args.transit_dir).joinpath(args.year)),
               # Peak transit network shapefile.
               (outdir.joinpath(f'transitshp_pk_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'transit', f'transit_pk-{args.year}')),
               # Off-peak transit network shapefile.
               (outdir.joinpath(f'transitshp_op_{args.title}_{args.year}.zip'),
                outdir.joinpath('networks', 'transit', f'transit_op-{args.year}')),
               # Skims.
               (outdir.joinpath(f'skims_{args.title}_{args.year}.zip'),
                outdir.joinpath('skims')),
               # Emme matrix directory.
               (outdir.joinpath(f'emmemat_{args.title}_{args.year}.zip'),
                projdir.joinpath('Database', 'emmemat')),
               # Emme databank.
               (outdir.joinpath(f'emmebank_{args.title}_{args.year}.zip'),
                projdir.joinpath('Database', 'emmebank'))]
    logging.info('Compressing outputs')
    with multiprocessing.Pool() as pool:
        list(tqdm(pool.imap_unordered(sharing.compress, ziplist), total=len(ziplist), desc='Compressing outputs'))
    logging.info('Finished.')

if __name__ == '__main__':
    main()