"""Helpers for working with Emme projects.

This module provides utilities for validating Emme project paths and
connecting to Emme Desktop from Python.
"""

from pathlib import Path
import argparse
import inro.emme.desktop.app as _app
import inro.modeller as _m


def emme_project_file(path):
    """Validate an Emme project file path.

    Parameters
    ----------
    path : str or pathlib.Path
        Path to an Emme project file.

    Returns
    -------
    str or pathlib.Path
        The validated input path.

    Raises
    ------
    argparse.ArgumentTypeError
        If the path does not have an ``.emp`` extension.
    argparse.ArgumentError
        If the path does not exist.
    """
    ext = Path(path).suffix
    if ext != '.emp':
        raise argparse.ArgumentTypeError('File must have an emp extension')
    if not Path(path).exists():
        raise argparse.ArgumentError('File does not exist')
    
    return path


def connect(path):
    """Start an Emme Desktop session and connect Modeller to a project.

    Parameters
    ----------
    path : str or pathlib.Path
        Path to an Emme project file or a directory containing one.

    Returns
    -------
    inro.modeller.Modeller
        An initialized Modeller client connected to the Emme project.
    """
    if isinstance(path, str):
        path = Path(path)
    if path.is_file():
        empfile = path
    elif path.is_dir():
        empfile = sorted(path.glob('**/*.emp'))[0]
    app = _app.start_dedicated(visible=False,
                               user_initials='CMAP',
                               project=empfile)
    modeller = _m.Modeller(app)

    return modeller
