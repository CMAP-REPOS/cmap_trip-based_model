import yaml

from .config import SETTINGS_FILE
from . import build_networks
# from . import trip_generation


def get_model_run_settings(yaml_path):
    """Read model run settings from a YAML file.
    """
    with yaml_path.open('r') as f:
        settings = yaml.safe_load(f)
    return settings


def main():
    settings = get_model_run_settings(SETTINGS_FILE)
    build_networks.main(settings)
    # trip_generation.main(settings)


if __name__ == "__main__":
    main()