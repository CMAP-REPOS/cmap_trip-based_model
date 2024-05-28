import standard_data


def main():
    config = standard_data.load_config()
    file_names = standard_data.add_tag(out_file_names=config['out_file_names'],
                                       title=config['model_version'],
                                       scenario_code=config['scenario_code'])
    file_names = standard_data.export(project_file_name=config['project_filename'],
                                      out_file_names=file_names,
                                      scenario_code=config['scenario_code'])
    file_names = standard_data.compress(out_file_names=file_names,
                                        scenario_code=config['scenario_code'],
                                        transit_dir=config['transit_directory'])
    config['out_file_names'] = file_names
    standard_data.document(context=config)


if __name__ == '__main__':
    main()