# Import-Module -Name "$PSScriptRoot\manage\env\CondaEnv.psm1"
# Start-CondaEnv -EnvTag 'emme'
uv run python -m cmap_tbm
Read-Host -Prompt "`nPress Enter to exit"