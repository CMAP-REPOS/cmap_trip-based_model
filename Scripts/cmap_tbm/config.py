from pathlib import Path

PROJ_DIR = Path(__file__).resolve().parents[2]
DB_DIR = PROJ_DIR.joinpath('Database')
TG_DIR = DB_DIR.joinpath('tg')
SETTINGS_FILE = DB_DIR.joinpath('batch_file.yaml')