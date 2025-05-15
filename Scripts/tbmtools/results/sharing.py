from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED

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
    with ZipFile(out_dir.joinpath(out_file_name), mode='w', compression=ZIP_DEFLATED, compresslevel=9) as zip:
        if source_path.is_file():
            zip.write(source_path, arcname=source_path.name)
        elif source_path.is_dir():
            for container in source_path.iterdir():
                if container.is_file():
                    zip.write(container, arcname=container.name)
