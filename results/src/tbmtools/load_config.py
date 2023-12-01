from pathlib import Path
import yaml

def read_config(in_path):
    config_file_path = Path(in_path).resolve()
    with open(config_file_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
    return config

print(read_config('./hand/results_config.yaml'), end='')