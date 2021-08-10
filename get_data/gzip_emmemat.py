
import glob
import gzip
import shutil
import os

top_directory = os.path.normpath(os.path.join(os.path.dirname(__file__), ".."))

os.chdir(top_directory)

for filename in glob.glob("Database/emmemat/*.emx"):
    with open(filename, 'rb') as f_in:
        with gzip.open(filename+".gz", 'wb') as f_out:
            print(f"gzipping {filename}")
            shutil.copyfileobj(f_in, f_out)


