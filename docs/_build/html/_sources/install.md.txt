# Installing the model

## Dependencies

### Anaconda
Download [Anaconda Distribution](https://www.anaconda.com/download/success) and install it on the machine that will be running the model.
* Install for a single user
* Do not add to the path environment variable

### Git
Open Anaconda Prompt and run the command to install Git.
```bat
conda install --channel conda-forge git
```

### R
With R and RStudio installed on the machine, open RStudio and navigate to the package installation tool (Tools > Install Packages...). Use the tool to install the following packages and their dependencies from the CRAN repository to the default library:
* sf
* tidyverse
* foreign

### OpenPaths EMME or EMME Desktop
With EMME installed on the machine, sign in to CONNECTION Client using a Bentley account with a license.

## Model project folder

With Anaconda Prompt in the target directory on the machine's local drive, clone the TBM repo and move to the main branch.
```bat
git clone https://github.com/CMAP-REPOS/cmap_trip-based_model.git
cd cmap_trip-based_model
git checkout main
```