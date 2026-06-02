import sys
from pathlib import Path
import subprocess
import yaml
import inro.modeller as _m
# sys.path.append(str(Path(__file__).resolve().parents[3]))
# from tbmtools import project as tbm
sys.path.append(str(Path(__file__).resolve().parents[4].joinpath('Database', 'useful_macros')))
import input_data_mac

_src_dir = Path(__file__).resolve().parent
_proj_dir = _src_dir.parents[3]


def load_config():
    """Load values from configuration files.

    Reads configuration settings from Database/batch_file.yaml and
    Scripts/prepare/conformity_scenario/hand/config.yaml.

    Returns
    -------
    dict
        Configuration properties as keys and configuration property
        values as values.
    """
    with open(_proj_dir.joinpath('Database/batch_file.yaml')) as f:
        batch_file_config = yaml.safe_load(f)
    with open(_src_dir.parent.joinpath('hand/config.yaml')) as f:
        config = yaml.safe_load(f)
    config.update(batch_file_config)
    print('config loaded')
    return config

def copy_model_data(config):
    ps1script = _proj_dir.joinpath('copy_scenario_data.ps1')
    recent_conformity = config['similar_model_version']
    scenario_code = config['scenario_code']
    cmd = f'powershell.exe -file "{ps1script}" -recent_conformity {recent_conformity} -scenario {scenario_code}'
    cmd_output = subprocess.run(cmd,
                                shell=True,
                                capture_output=True,
                                text=True)
    if cmd_output.stderr != '':
        raise Exception(cmd_output.stderr)
    else:
        print(cmd_output.stdout)
        print(f'copied data from {recent_conformity} {scenario_code}')


def rename_project(config):
    """
    Renames database, project, and project file components with the
    specified conformity code and scenario code.
    """
    modeller = _m.Modeller()
    # Rename the database.
    year = config['scenario_years'][config['scenario_code']]
    emmebank = modeller.emmebank
    emmebank.title = f"{config['model_version']} Scenario {config['scenario_code']} [{year}]"
    # Rename the project.
    project = modeller.desktop.project
    project.name = f"{config['model_version']}_{config['scenario_code']}"
    project.save()
    # Rename the project files.
    emp_file = Path(project.path)
    prj_file = Path(project.path + '.prj')
    new_name = f"{config['model_version']}_{config['scenario_code']}"
    emp_file.rename(Path(emp_file.parent, new_name + '.emp'))
    prj_file.rename(Path(prj_file.parent, new_name + '.emp.prj'))
    print('project renamed')

def get_wfh_rates(config):
    scenario_year = config['scenario_years'][config['scenario_code']]
    wfh_rates = config['wfh_rates'][scenario_year]
    print('\n'
          f'Set these {scenario_year} WFH rates in \\Database\\batch_file.yaml:\n'
          f"    * usualwfhpct: {wfh_rates['fully_remote']}\n"
          f"    * tc14pct: {wfh_rates['hybrid']}\n")
    
def get_trip_growth_factors(config):
    scenario_year = config['scenario_years'][config['scenario_code']]
    trip_growth_factors = config['trip_growth_factors'][scenario_year]
    print('\n'
          f'Set this {scenario_year} trip growth factor in \\Database\\prep_macros\\distribute.trucks:\n'
          f"    * r104={trip_growth_factors['truck/poe']}\n"
          '\n'
          f'Set this {scenario_year} trip growth factor in \\Database\\prep_macros\\distribute.poes:\n'
          f"    * r5={trip_growth_factors['truck/poe']}\n"
          '\n'
          f'Set this {scenario_year} trip growth factor in \\Database\\cmap_trip_config.yaml:\n'
          f"    * visitor_trips_growth_factor: {trip_growth_factors['visit']}\n")

def get_tnc_surcharge_rates(config):
    scenario_year = config['scenario_years'][config['scenario_code']]
    years = sorted([int(i) for i in config['tnc_surcharges'].keys()])
    for year in years:
        if scenario_year >= year:
            surcharge = config['tnc_surcharges'][year]
    print('\n'
          f'Set these {scenario_year} TNC surcharges in \\Database\\cmap_trip_config.yaml:\n'
          '    * tnc:\n'
          '        surcharge_rates:\n'
          f'          downtown: {surcharge}\n'
          '    * tnc_pooled:\n'
          '        surcharge_rates:\n'
          f'          downtown: {surcharge}\n')

def main():
    config = load_config()
    copy_model_data(config)
    input_data_mac.main()
    print('warm start matrices generated')
    rename_project(config)
    get_wfh_rates(config)
    get_trip_growth_factors(config)
    get_tnc_surcharge_rates(config)


if __name__ == '__main__':
    sys.exit(main())