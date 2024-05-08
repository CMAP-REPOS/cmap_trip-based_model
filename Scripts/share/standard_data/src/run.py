from pathlib import Path

import yaml

import standard_data


def main():
    src_dir = Path(__file__).resolve().parent
    proj_dir = src_dir.parents[3]
    with open(proj_dir.joinpath('Database/batch_file.yaml')) as f:
        batch_file_config = yaml.safe_load(f)
    with open(src_dir.parent.joinpath('hand/config.yaml')) as f:
        config = yaml.safe_load(f)
    config['scenario_code'] = batch_file_config['scenario_code']
    config['model_version'] = batch_file_config['model_version']
    # standard_data.export(project_file=proj_dir.joinpath(config['project_filename']),
    #                      title=config['title'],
    #                      scenario_code=scenario_code)
    # standard_data.compress(title=config['title'],
    #                        scenario_code=scenario_code,
    #                        transit_dir=config['transit_dir'])
    standard_data.document(context=config)


if __name__ == '__main__':
    main()