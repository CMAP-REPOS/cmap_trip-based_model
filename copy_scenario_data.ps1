param([string]$recent_conformity = $(throw 'A required argument is missing for the following parameter: -recent_conformity <conformity code>.'),
      [string]$scenario = $(throw 'A required argument is missing for the following parameter: -scenario <scenario code>.'),
      [string]$source_dir = 'M:\catslib\modelprod\',
      [string]$destination_dir = $PSScriptRoot)

$source_db = "$source_dir$recent_conformity\$($recent_conformity+'_'+$scenario)*\cmap_trip-based_model\Database\"
$destination_db = "$destination_dir\Database\"
$items = @("emmebank"
           "emmemat"
           "tg\UrbanSim_inputs"
           "tg\fortran\ATTR_IN.TXT"
           "tg\fortran\HH_IN.TXT"
           "tg\fortran\GQ_IN.TXT"
           "tg\fortran\POPSYN_HH.CSV"
           "tg\fortran\airport_sz.csv"
           "tg\fortran\SCHOOL_IN.CSV"
           "tg\fortran\wfhmodule\indusmix.csv")

foreach($item in $items){
    if(Test-Path "$destination_db$item"){
        $existing_item = "$destination_db$item"
        if(($existing_item -like "C:*") -or ($existing_item -like "D:*") -or ($existing_item -like "E:*") -or ($existing_item -like "F:*")){
            Write-Host "Replacing $existing_item..."
            Remove-Item $existing_item -recurse}
        else{
            throw "Can't use this to delete existing files from network drives."}}
    #Write-Host "Copying $source_db$item to $destination_db..."
    Copy-Item "$source_db$item" -destination "$destination_db$item" -recurse}

Write-Host "Finished."