from pathlib import Path
from zipfile import ZipFile, ZIP_LZMA

def pack(zipfile, contentpath):
    """
    Pack file(s) into a compressed ZIP file.

    Parameters:  zipfile : str or path object
                     Path to destination ZIP file.

                 contentpath : str or path object
                     Path to a file or a directory with files to be
                     packed.

    Returns:     path object
                     Path to destination ZIP file.
    """
    if isinstance(contentpath, str):
        contentpath = Path(contentpath).resolve()
    with ZipFile(zipfile, mode='w', compression=ZIP_LZMA) as zip:
        if contentpath.is_file():
            zip.write(contentpath, arcname=contentpath.name)
        elif contentpath.is_dir():
            for content in contentpath.iterdir():
                if content.is_file():
                    zip.write(content, arcname=content.name)