# CMAP Trip-based Model

This repository contains the code needed to run the CMAP trip-based model's 
mode, destination and time-of-day model components.

## Installation Instructions

- First, if "conda" is not already installed and accessible by the current user
  on the target development machine, install it. An easy installation solution
  is using the "Mambaforge" installer, which can be downloaded for Windows
  [here](https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-Windows-x86_64.exe). 
  Once downloaded, double-click and run the installer. Mamba is a somewhat faster
  implementation of the conda package installer, and can be used interchangably
  with conda - nearly any command you might run as `conda foo` can also be run as
  `mamba foo`.
  
- Then if needed also install git and (optionally) the GitHub command line interface.
  From either the Anaconda Prompt or the Miniforge Prompt, run:
  
  ```shell
  mamba install git gh -c conda-forge 
  ```
  
- Now clone the CMAP trip-based model code from GitHub, and checkout the CS2021 branch. 

  ```shell
  gh repo clone camsys/cmap_trip-based_model  
  cd cmap_trip-based_model  
  git checkout CS2021  
  ```
  
  To clone into a different directory than the current directory (probably the 
  user's home directory) change to that directory using `cd` first.
  
- Next, create a conda environment to use for the mode, destination and 
  time-of-day model components, which are written in Python.  
  Creating a unique environment will ensure that all the necessary computational 
  libraries are installed, and no conflicts are introduced with other tools 
  (including Emme). A conda environment file is included in the GitHub repository,
  which installs everything you need into an environment called "CMAP-TRIP".
  
  ```shell
  conda env create --file src/Mode-Dest-TOD/conda-environment.yml
  ````
  
  Many of the necessary files for a basic run of the model using the default base year
  inputs are already in the repository, but not everything.  The emmebank, skim data,
  and few other files are quite large and don't fit nicely into git.  You can
  get the rest of these files from Amazon's S3 service.
   
  ```shell
  conda activate CMAP-TRIP
  python get_data/get_data.py
  ```
  
## Execution Instructions

### Running the Full Model (including Feedback Iterations)

Running the full model requires an active and properly licensed EMME installation,
plus a conda package and environment manager (available as open source, described 
above under installation). Assuming these are available, run the batch script:

```shell
cd Database
Submit_Full_Regional_Model.bat
```

This batch script can be run from any flavor of conda terminal (Anaconda Prompt, 
Miniforge Prompt, etc) or from a plain Windows command prompt, which means it can
also be run just by double-clicking the `Submit_Full_Regional_Model.bat` file
in Windows Explorer.


### Running Only the Mode, Destination and Time-of-Day Components

To run the mode, destination, and time-of-day components for the CMAP Trip-based
model, first activate the correct conda environment. From either the Anaconda Prompt 
or the Miniforge Prompt, run:
  
```shell
conda activate CMAP-TRIP
```

(If you created an environment with some other name, activate that other environment instead.)

Then, you can run the model by calling its executable version right from that
same command prompt, pointing at the correct `Database` directory:
  
```shell
cmap_modedest .\path\to\Database
```

The `cmap_modedest` command line tool has a number of available options. The most important
of these include the number of jobs and maximum number of origin zones processed per chunk,
each of which needs to be set appropriately for the computer running the model to maximize
resource usage (CPU's, RAM) without exceeding available resources, which can cause 
out-of-memory errors or processor thrashing that can slow the overall runtime. 
These options can be reviewed in the online help:
  
```shell
cmap_modedest --help
```  



