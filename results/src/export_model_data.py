import argparse
from pathlib import Path
import sys
import os
import logging
import shutil
import tbmtools.project as tbm
from tbmtools.results import vehicle_trips
from tbmtools.results import person_trips
from tbmtools.results import skims
from tbmtools.results import transit_network
from tbmtools.results import highway_network
from tbmtools.results import sharing

logging.basicConfig(filename='export_model_data.log',
                    filemode='w',
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    level=logging.INFO)

def main():
    parser = argparse.ArgumentParser(description='export standard model data')
    parser.add_argument('--project_file', type=tbm.emme_project_file, help='path to Emme project file')
    parser.add_argument('--title', help='modeled scenario title')
    parser.add_argument('--year', help='modeled scenario year')
    parser.add_argument('--transit_dir', help='path to transit directory with TOD network transaction files')
    args = parser.parse_args()
    print(args)
    
    scriptdir = Path(sys.argv[0]).parent
    os.chdir(str(scriptdir))
    print(Path.cwd())
    
    projdir = Path('../..').resolve()
    outdir = Path('../output').resolve()

    modeller = tbm.connect(args.project_file)
    logging.info(f'Connected to {modeller.desktop.project_file_name()}')
      
    # Make output directories.
    outdir.mkdir(exist_ok=True)
    outdir.joinpath('trips').mkdir(exist_ok=True)
    outdir.joinpath('trips', 'work_trips').mkdir(exist_ok=True)
    outdir.joinpath('trips', 'hov_trips').mkdir(exist_ok=True)
    outdir.joinpath('networks').mkdir(exist_ok=True)
    outdir.joinpath('networks', 'highway').mkdir(exist_ok=True)
    outdir.joinpath('networks', 'transit').mkdir(exist_ok=True)
    outdir.joinpath('skims').mkdir(exist_ok=True)
    outdir.joinpath('prods_attrs').mkdir(exist_ok=True)
    logging.info(f'Writing output to {outdir}')

    # Export vehicle trips and skims.
    logging.info('Starting vehicle trips')
    vehicle_trips.export_matrices(outdir, modeller)
    logging.info('Starting skims')
    skims.flag_transit_disconnects(modeller)
    skims.export_matrices(outdir, modeller)

    # Export trip roster, auto person trips, and transit person trips.
    logging.info('Starting person trips')
    person_trips.export_auto_matrices(projdir, outdir)
    person_trips.export_transit_matrices(outdir, modeller)

    # Export peak (AM peak) and off-peak (midday) transit network,
    # itineraries, and attributes as Emme transaction files and
    # shapefiles.
    logging.info('Starting transit network')
    transit_network.export(path=outdir.joinpath('networks', 'transit'),
                           scenario=int(args.year),
                           format='transaction',
                           modeller=modeller,
                           emmebank=modeller.emmebank)
    transit_network.export(path=outdir.joinpath('networks', 'transit'),
                           scenario=int(args.year),
                           format='shape',
                           modeller=modeller,
                           emmebank=modeller.emmebank)
    
    # Export each time of day highway network and attributes as Emme
    # transaction files.
    logging.info('Starting highway network')
    highway_network.export_by_tod(path=outdir.joinpath('networks', 'highway'),
                                  scenario=int(args.year),
                                  format='transaction',
                                  modeller=modeller,
                                  emmebank=modeller.emmebank)
    
    # Export peak highway networks and attributes as shapefiles.
    highway_network.export_by_tod(path=outdir.joinpath('networks', 'highway'),
                                  scenario=int(args.year),
                                  format='shape',
                                  modeller=modeller,
                                  emmebank=modeller.emmebank,
                                  period=[3, 7])
    # Export daily highway network and attributes as Emme transaction
    # files and shapefiles.
    highway_network.export_daily(path=outdir.joinpath('networks', 'highway'),
                                 scenario=int(args.year),
                                 format='transaction',
                                 modeller=modeller,
                                 emmebank=modeller.emmebank)
    highway_network.export_daily(path=outdir.joinpath('networks', 'highway'),
                                 scenario=int(args.year),
                                 format='shape',
                                 modeller=modeller,
                                 emmebank=modeller.emmebank)
    
    # Copy TG results to output directory.
    dir = projdir.joinpath('Database', 'tg', 'data')
    file = sorted(dir.glob('tg_results*.csv'))[0]
    shutil.copy(file, outdir)
    file_copy = outdir.joinpath(file.name)
    renamed_copy = file_copy.with_name(f'tg_results_{args.title}_{args.year}.csv')
    if renamed_copy.exists(): 
        os.remove(renamed_copy)
    file_copy.rename(renamed_copy)

    # Copy productions and attractions to output directory.
    files = sorted(dir.glob('*.in'))
    for file in files:
        shutil.copy(file, outdir.joinpath('prods_attrs'))

    # Pack model data for sharing.
    logging.info('Packing productions and attractions')
    sharing.pack(outdir.joinpath(f'prods_attrs_{args.title}_{args.year}.zip'),
                 outdir.joinpath('prods_attrs'))
    logging.info('Packing trips')
    sharing.pack(outdir.joinpath(f'trips_{args.title}_{args.year}.zip'),
                 outdir.joinpath('trips'))
    logging.info('Packing work trips')
    sharing.pack(outdir.joinpath(f'worktrips_{args.title}_{args.year}.zip'),
                 outdir.joinpath('trips', 'work_trips'))
    logging.info('Packing hov trips')
    sharing.pack(outdir.joinpath(f'hovtrips_{args.title}_{args.year}.zip'),
                 outdir.joinpath('trips', 'hov_trips'))
    logging.info('Packing highway transaction files')
    sharing.pack(outdir.joinpath(f'emmenet_highway_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'highway'))
    logging.info('Packing highway shapefiles')
    sharing.pack(outdir.joinpath(f'highwayshp_ampk_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'highway', f'highway_ampk-{args.year}'))
    sharing.pack(outdir.joinpath(f'highwayshp_pmpk_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'highway', f'highway_pmpk-{args.year}'))
    sharing.pack(outdir.joinpath(f'highwayshp_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'highway', f'highway-{args.year}'))
    logging.info('Packing modeled transit transaction files')
    sharing.pack(outdir.joinpath(f'emmenet_transit_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'transit'))
    logging.info('Packing TOD transit transaction files')
    sharing.pack(outdir.joinpath(f'emmenet_transit_tod_{args.title}_{args.year}.zip'),
                 Path(args.transit_dir).joinpath(args.year))
    logging.info('Packing transit shapefiles')
    sharing.pack(outdir.joinpath(f'transitshp_pk_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'transit', f'transit_pk-{args.year}'))
    sharing.pack(outdir.joinpath(f'transitshp_op_{args.title}_{args.year}.zip'),
                 outdir.joinpath('networks', 'transit', f'transit_op-{args.year}'))
    logging.info('Packing skims')
    sharing.pack(outdir.joinpath(f'skims_{args.title}_{args.year}.zip'),
                 outdir.joinpath('skims'))
    logging.info('Packing matrix directory')
    sharing.pack(outdir.joinpath(f'emmemat_{args.title}_{args.year}.zip'),
                 projdir.joinpath('Database', 'emmemat'))
    logging.info('Packing emmebank')
    sharing.pack(outdir.joinpath(f'emmebank_{args.title}_{args.year}.zip'),
                 projdir.joinpath('Database', 'emmebank'))
    logging.info('Finished packing')

if __name__ == '__main__':
    main()