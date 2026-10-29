$condaPath = "$env:LOCALAPPDATA\miniconda3"
.$condaPath\shell\condabin\conda-hook.ps1
conda remove --name tbm-r --yes --all
conda remove --name cmap-modedest --yes --all
Write-Host "Complete"
Read-Host -Prompt "Press Enter to exit"