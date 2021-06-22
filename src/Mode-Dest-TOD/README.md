# CMAP Trip-based Model


## Installation Instructions

- First, if "conda" is not already installed and accessible by the current user
  on the target development machine, install it. A preferred installation solution
  is using the "Mambaforge" installer, which can be downloaded for windows here:
  
  >  https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-Windows-x86_64.exe
    
  Once downloaded, double-click and run the installer. Mamba is a somewhat faster
  implementation of the conda package installer, and can be used interchangably
  with conda - nearly any command you might run as `conda foo` can also be run as
  `mamba foo`.
  
- Then if needed also install git and (optionally) the GitHub command line interface.
  From either the Anaconda Prompt or the Miniforge Prompt, run:
  
  > mamba install git gh -c conda-forge 
  
- Now clone the CMAP trip-based model code from GitHub, and checkout the CS2021 branch. 

  > gh repo clone camsys/cmap_trip-based_model
  > cd cmap_trip-based_model
  > git checkout CS2021
  
  To clone into a different directory than the current directory (probably the 
  user's home directory) change to that directory using `cd` first.
  
- Next, create a conda environment to use for the trip-based model components.  
  Creating a unique environment will ensure that all the necessary computational 
  libraries are installed, and no conflicts are introduced with other tools.
  A conda environment file is included in the GitHub repository.
  
  > mamba env create --file conda-environment.yml
  
  Also get the Database folder: 
  
  > https://camsys-my.sharepoint.com/:u:/p/jeffnewman/EWbrhcl0NnJDufOQD0rYWUABo7-BEHmlxaXX9VvWknOsRQ?e=UK5Hqv
  
## Execution Instructions

- To run the mode, destination, and time-of-day components for the CMAP Trip-based
  model, first activate the correct conda environment. From either the Anaconda Prompt 
  or the Miniforge Prompt, run:
  
  > conda activate CMAP-TRIP
  
  (If you created an environment with some other name, activate that other environment instead.)

- Then, you can run the model by calling its executable version right from that
  same command prompt, pointing at the correct `Database` directory:
  
  > cmap_modedest .\path\to\Database
   
