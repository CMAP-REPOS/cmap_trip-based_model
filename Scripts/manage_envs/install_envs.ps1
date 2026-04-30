# Install miniconda.
$condaPath = "$env:LOCALAPPDATA\miniconda3"
if (-not (Test-Path -Path "$condaPath\python.exe")) {
    Invoke-WebRequest -Uri "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe" -OutFile "$env:USERPROFILE\Downloads\Miniconda3-latest-Windows-x86_64.exe"
    Start-Process -FilePath "$env:USERPROFILE\Downloads\Miniconda3-latest-Windows-x86_64.exe" -ArgumentList "/S", "/D=$condaPath" -Wait
}
# Install uv.
$uvPath = "$env:USERPROFILE\.local\bin"
if (-not (Test-Path -Path "$uvPath\uv.exe")) {
    # Run the installer. Please review the printed message after installation.
    powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
    # Add uv to Path
    $env:Path = "$uvPath;$env:Path"
}
.$condaPath\shell\condabin\conda-hook.ps1
# Create the R conda environment for running R scripts.
if (-not (conda env list | Select-String -Pattern "tbm-r" -Quiet)) {
    conda env create --file "$PSScriptRoot\tbm-r\tbm-r.yml" --yes
    if ($LastExitCode -ne 0) {
        Write-Error "conda env create tbm-r failed"
        exit $LastExitCode
    }
}
# Create the Python conda environment for running cmap_modedest.
if (-not (conda env list | Select-String -Pattern "cmap-modedest" -Quiet)) {
    conda env create --file "$PSScriptRoot\cmap-modedest\cmap-modedest.yml" --yes
    if ($LastExitCode -ne 0) {
        Write-Error "conda env create cmap-modedest failed"
        exit $LastExitCode
    }
    conda activate cmap-modedest
    uv pip install --requirements "$PSScriptRoot\cmap-modedest\requirements.txt"
    if ($LastExitCode -ne 0) {
        Write-Error "(cmap-modedest) uv pip install failed"
        exit $LastExitCode
    }
    conda deactivate
}
# Sync the base uv environment for running the model.
if (-not (Test-Path -Path "$PSScriptRoot\..\..\uv.lock")) {
    uv lock
}
uv sync --locked
if ($LastExitCode -ne 0) {
    Write-Error "uv sync failed"
    exit $LastExitCode
}
# Extend the base environment to include Emme packages.
Copy-Item -Path "$PSScriptRoot\emme.pth" -Destination "$PSScriptRoot\..\..\.venv\Lib\site-packages"