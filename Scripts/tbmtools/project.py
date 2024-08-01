from pathlib import Path
import argparse
import inro.emme.desktop.app as _app
import inro.modeller as _m

def emme_project_file(path):
    """
    Validate an Emme project file path.
    
    Parameters:  path : str
                     Path to an Emme project file.
    
    Returns:  str
                  Path to Emme project file.
    """
    ext = Path(path).suffix
    if ext != '.emp':
        raise argparse.ArgumentTypeError('File must have an emp extension')
    if not Path(path).exists():
        raise argparse.ArgumentError('File does not exist')
    return path

def connect(path):
    """
    Start an Emme Desktop session and connect Emme Modeller to the Emme
    project.
    
    Parameters:  path : str or path object
                     Emme project file path or path to directory
                     containing Emme project file.
                     
    Returns:     Modeller object
    """
    if Path(path).is_file():
        empfile = str(path)
    elif Path(path).is_dir():
        empfile = sorted(Path(path).rglob('*.emp'))[0]
    app = _app.start_dedicated(visible=False,
                               user_initials='INRO',
                               project=emme_project_file(empfile))
    modeller = _m.Modeller(app)
    return modeller