from pathlib import Path
from zipfile import ZipFile, ZIP_LZMA

def compress(args):
    """
    Compress file(s) into a ZIP file.

    Parameters:  args : tuple
                     Packed arguments.

                     (zipfile, contentpath)

                     zipfile : str or path object
                         Path to destination ZIP file.

                     contentpath : str or path object
                         Path to a file or a directory with files to be
                         compressed.

    Returns:     None
    """
    # Unpack arguments.
    zipfile, contentpath = args
    # Handle arguments.
    if isinstance(contentpath, str):
        contentpath = Path(contentpath).resolve()
    # Compress content.
    with ZipFile(zipfile, mode='w', compression=ZIP_LZMA) as zip:
        if contentpath.is_file():
            zip.write(contentpath, arcname=contentpath.name)
        elif contentpath.is_dir():
            for content in contentpath.iterdir():
                if content.is_file():
                    zip.write(content, arcname=content.name)