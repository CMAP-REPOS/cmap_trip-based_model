"""Package model output data products for distribution.

This module exports model data from Emme projects, assembles package
directories, and renders documentation for the packaged results.
"""

import logging
import os
from pathlib import Path
import shutil

from jinja2 import Environment, FileSystemLoader
import markdown
from tqdm.contrib.concurrent import process_map

from . import project as tbm
from .utils import compress, copy_prods_attrs, load_config
from .matrix import vehicle_trip, skim, person_trip
from .network import highway, transit


def compress_data(data_dirs, out_dir, config, tag):
    """Compress exported data directories into ZIP archives.

    Parameters
    ----------
    data_dirs : dict
        Mapping of data product names to directories that should be compressed.
    out_dir : pathlib.Path
        Directory where the ZIP archives will be written.
    config : dict
        Configuration mapping containing the ``compressed`` section that
        defines the output archive names.
    tag : str
        Tag appended to each archive filename.

    Returns
    -------
    None
    """
    logging.info('Compressing data files')
    zip_file_names = [f'{label}_{tag}.zip' for label in config['compressed'].values()]
    src_paths = [data_dirs[data_product] for data_product in config['compressed'].keys()]
    process_map(compress, zip_file_names, src_paths, [out_dir] * len(zip_file_names), desc=f'Compressing {tag} data files')


def copy_data(src_dir, target_dir, config, tag, data_paths):
    """Copy packaged data files into the output directory.

    Parameters
    ----------
    src_dir : pathlib.Path
        Source project directory containing the TG results data.
    target_dir : pathlib.Path
        Destination directory for the packaged data files.
    config : dict
        Configuration mapping used to name the copied TG results file.
    tag : str
        Tag appended to the copied TG results filename.
    data_paths : dict
        Mapping of exported artifact names to their paths, including the trip
        roster.

    Returns
    -------
    None

    Raises
    ------
    FileNotFoundError
        If more than one TG results file is found in the source directory.
    """
    # Copy TG results.
    logging.info('Copying TG results')
    tg_dir = src_dir.joinpath('Database', 'tg')
    tg_results_file = sorted(tg_dir.joinpath('data').glob('tg_results*.csv'))
    if len(tg_results_file) > 1:
        raise FileNotFoundError(f"Multiple TG results files exist in {tg_dir.joinpath('data')}.")
    else:
        file = tg_results_file[0]
    shutil.copy(file, target_dir)
    # Rename TG results.
    file_copy = target_dir.joinpath(file.name)
    new_path = file_copy.with_name(f'{config["tg_data"]}_{tag}.csv')
    if new_path.exists():
        os.remove(new_path)
    file_copy.rename(new_path)
    data_paths['tg_data'] = new_path
    # Copy trip roster to package directory.
    shutil.copy(data_paths['trip_roster'], target_dir)


def export_data(proj_dir, out_dir, tag, config, modeller):
    """Export model data products into a staging directory.

    Parameters
    ----------
    proj_dir : pathlib.Path
        Project directory containing the Emme model files and supporting data.
    out_dir : pathlib.Path
        Directory where exported data products will be written.
    tag : str
        Tag used to distinguish packaged output files.
    config : dict
        Configuration mapping with scenario and output naming settings.
    modeller : inro.modeller.Modeller
        Modeller instance used to access Emme tools for exporting data.

    Returns
    -------
    dict
        Mapping of exported data product names to their output paths.
    """
    # Display a progress bar while exporting data.
    export_paths = dict()
    # Export data files.
    trip_tables_path = vehicle_trip.export_auto(out_dir,
                                                config["scenario_code"],
                                                modeller)
    export_paths['trip_tables'] = trip_tables_path
    skim_matrices_path = skim.export_transit(out_dir,
                                             config["scenario_code"],
                                             modeller)
    skim.export_highway(out_dir,
                        config["scenario_code"],
                        modeller)
    export_paths['skim_matrices'] = skim_matrices_path
    trip_roster_name = f"{config['trip_roster']}_{tag}.csv"
    trip_roster_path = person_trip.export_trip_roster(proj_dir,
                                                      out_dir,
                                                      out_filename=trip_roster_name)
    export_paths['trip_roster'] = trip_roster_path
    trip_tables_path, hov_trip_tables_path = person_trip.export_auto(proj_dir,
                                                                     out_dir,
                                                                     trip_roster_path)
    person_trip.export_transit(proj_dir,
                               out_dir,
                               config["scenario_code"],
                               modeller)
    export_paths['hov_trip_tables'] = hov_trip_tables_path
    transit_networks_path, peak_transit_network_path, offpeak_transit_network_path = transit.export_all(out_dir,
                                                                                                        config["scenario_code"],
                                                                                                        modeller)
    export_paths['transit_networks'] = transit_networks_path
    export_paths['peak_transit_network'] = peak_transit_network_path
    export_paths['offpeak_transit_network'] = offpeak_transit_network_path
    highway_networks_path, am_peak_highway_network_path, pm_peak_highway_network_path, daily_highway_network_path = highway.export_all(out_dir=out_dir,
                                                                                                                                        scenario_code=config["scenario_code"],
                                                                                                                                        modeller=modeller)
    export_paths['highway_networks'] = highway_networks_path
    export_paths['am_peak_highway_network'] = am_peak_highway_network_path
    export_paths['pm_peak_highway_network'] = pm_peak_highway_network_path
    export_paths['daily_highway_network'] = daily_highway_network_path
    export_paths['tod_transit_networks'] = Path(config['transit_directory'], str(config["scenario_code"]))
    export_paths['database'] = proj_dir.joinpath('Database/emmebank')
    export_paths['matrices'] = proj_dir.joinpath('Database/emmemat')
    export_paths['pa_tables'] = copy_prods_attrs(proj_dir, out_dir)

    return export_paths


def pack_data(proj_file, out_dir, config_file=Path(__file__).parent.joinpath('config.yaml'), templates=Path(__file__).parent.joinpath('templates')):
    """Package model data products into a compressed archive.

    Parameters
    ----------
    proj_file : pathlib.Path
        Path to the Emme project file to package.
    out_dir : pathlib.Path
        Root output directory where the packaged data will be written.
    config_file : pathlib.Path, optional
        Path to the YAML configuration file used to define packaging settings.
    templates : pathlib.Path, optional
        Directory containing the markdown and HTML templates used to render the
        data user guide.

    Returns
    -------
    pathlib.Path
        Path to the generated ZIP archive for the packaged data.
    """
    # Load configuration settings.
    proj_dir = proj_file.parent
    config = load_config(config_file, proj_dir)
    # Make output subdirectory.
    out_subdir = out_dir.joinpath(config["model_version"], str(config["scenario_code"]))
    out_subdir.mkdir(parents=True, exist_ok=True)
    # Set up log file.
    logging.basicConfig(filename=out_subdir.joinpath('pack.log'),
                        filemode='w',
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        level=logging.INFO)
    # Connect to EMME Modeller.
    modeller = tbm.connect(proj_file)
    logging.info(f'Connected to {modeller.desktop.project_file_name()}')
    # Set tag for file names.
    tag = f'{config["model_version"]}_{config["scenario_code"]}'
    logging.info(f'Writing output to {out_subdir}')
    # Export data.
    export_paths = export_data(proj_dir,
                               out_subdir,
                               tag,
                               config,
                               modeller)
    # Compress exported data.
    pkg_dir = out_subdir.joinpath(f'{config["model_version"]}_{config["scenario_code"]}')
    pkg_dir.mkdir(exist_ok=True)
    compress_data(data_dirs=export_paths,
                  out_dir=pkg_dir,
                  config=config,
                  tag=tag)
    # Copy individual data files.
    copy_data(src_dir=proj_dir,
              target_dir=pkg_dir,
              config=config,
              tag=tag,
              data_paths=export_paths)
    # Render the data user guide.
    config.update(config.pop('compressed'))
    data_user_guide_file = render_data_user_guide(templates, out_subdir, context=config)
    # Copy data user guide to package directory.
    shutil.copy(data_user_guide_file, pkg_dir)
    # Compress package directory.
    pkg = compress(f'{pkg_dir.name}.zip', pkg_dir, pkg_dir.parent)
    logging.info('Finished.')

    return pkg


def render_data_user_guide(templates, out_dir, context):
    """Render an HTML data user guide from Markdown and HTML templates.

    Parameters
    ----------
    templates : pathlib.Path
        Directory containing the template files used to build the guide.
    out_dir : pathlib.Path
        Directory where the generated Markdown and HTML files will be written.
    context : dict
        Mapping of configuration keys to values used to render the templates.

    Returns
    -------
    pathlib.Path
        Path to the rendered HTML data user guide file.
    """
    # Load the Markdown template for data user guide.
    environment = Environment(loader=FileSystemLoader(templates))
    md_template = environment.get_template('data_user_guide_md.txt')
    # Render the Markdown template.
    md_file = out_dir.joinpath('data_user_guide.md')
    with open(md_file, mode='w', encoding='utf-8') as file:
        file.write(md_template.render(context))
    # Read Markdown from file.
    with open(md_file, encoding='utf-8') as file:
        md = file.read()
    # Convert Markdown to HTML.
    html = markdown.markdown(text=md, extensions=['tables'])
    # Load the HTML template for data user guide.
    html_template_file = Path(__file__).parent.joinpath('templates', 'data_user_guide_html.txt')
    with open(html_template_file, encoding='utf-8') as file:
        html_template = file.read()
    # Render the HTML template.
    html_file = md_file.with_suffix('.html')
    with open(html_file, mode='w', encoding='utf-8') as file:
        file.write(html_template.replace('{{content}}', html))

    return html_file


def run_pack_data(args):
    """Package data for one or more Emme projects from CLI arguments.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments. If ``args.series`` is provided, it should
        point to a directory containing one or more Emme project files.

    Returns
    -------
    None
    """
    # Search for Emme project files.
    if args.series:
        proj_files = sorted(Path(args.series).rglob('*.emp'))
        out_dir = Path(args.series).joinpath('packaged_data')
        process_map(pack_data, proj_files, [out_dir] * len(proj_files), desc=f'Packing data into {out_dir}')
    else:
        proj_dir = Path(__file__).parents[4]
        proj_files = sorted(proj_dir.glob('*.emp'))
        if len(proj_files) > 1:
            raise FileNotFoundError(f"Multiple EMME project files exist in {proj_dir}.")
        else:
            proj_file = proj_files[0]
            out_dir = proj_file.parent.joinpath('packaged_data')
        pack_data(proj_file, out_dir)
