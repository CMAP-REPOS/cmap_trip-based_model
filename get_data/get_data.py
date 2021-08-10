#!/bin/python
#
#   This script decompresses and downloads data needed for the model, other
#   than what's already in the Git repository.

import os
import requests
import yaml
import gzip
import shutil

inflation_manifest = os.path.join(os.path.dirname(__file__), "inflation_manifest.yml")
top_directory = os.path.normpath(os.path.join(os.path.dirname(__file__), ".."))

os.chdir(top_directory)

with open(inflation_manifest, 'rt') as f:
    manifest = yaml.load(f, Loader=yaml.SafeLoader)

def download_file_from_s3(bucket, dest, obj, region="us-west-2"):
    destination = os.path.join(dest, obj)
    url = f"https://{bucket}.s3.{region}.amazonaws.com/{obj}"
    print(f"downloading {url}")
    with requests.get(url, stream=True) as r:
        with open(destination, 'wb') as f:
            shutil.copyfileobj(r.raw, f)

if "gzipped" in manifest:
    for raw, compressed in manifest["gzipped"].items():
        os.makedirs(os.path.dirname(raw), exist_ok=True)
        with gzip.open(compressed, 'rb') as f_in:
            with open(raw, 'wb') as f_out:
                print(f"gunzipping {compressed} -> {raw}")
                shutil.copyfileobj(f_in, f_out)

if "s3" in manifest:
    for raw in manifest["s3"]:
        os.makedirs(os.path.dirname(raw), exist_ok=True)
        if raw[-7:] == ".emx.gz":
            if os.path.isfile(raw[:-3]):
                continue
        else:
            if os.path.isfile(raw):
                continue
        download_file_from_s3(
            "camsys-cmap-trip-based-model",
            top_directory,
            raw,
            region="us-west-2",
        )
        if raw[-7:] == ".emx.gz":
            with gzip.open(raw, 'rb') as f_in:
                with open(raw[:-3], 'wb') as f_out:
                    print(f"gunzipping {raw} -> {raw[:-3]}")
                    shutil.copyfileobj(f_in, f_out)
            os.remove(raw)

