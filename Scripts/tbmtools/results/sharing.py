from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED


def mp_compress(args):
    """Wrap compress for multiprocessing.

    Unpack arguments, then call compress using the unpacked arguments.

    Parameters
    ----------
    args : iterable object
           Element of the iterable (iterable of iterables) passed to the
           process pool using a method that does not unpack iterables.
    """
    # Unpack arguments.
    out_file_name, source_path, out_dir = args
    # Compress.
    compress(out_file_name, source_path, out_dir)

def compress(out_file_name, source_path, out_dir):
    """Compress file(s) into a ZIP file.

    Parameters
    ----------
    out_file_name : str or path object
               Path to destination ZIP file.
    source_path : str or path object
               Path to a file or a directory with files to be
               compressed.
    """
    # Handle arguments.
    if isinstance(source_path, str):
        source_path = Path(source_path).resolve()
    # Compress content.
    with ZipFile(out_dir.joinpath(out_file_name), mode='w', compression=ZIP_DEFLATED, compresslevel=9) as zip:
        if source_path.is_file():
            zip.write(source_path, arcname=source_path.name)
        elif source_path.is_dir():
            for container in source_path.iterdir():
                if container.is_file():
                    zip.write(container, arcname=container.name)