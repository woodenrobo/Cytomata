#first make sure that your WinRAR path is correct, then set the path to .zips and the folder you want to create
#right-click the folder in which this script is -> open in terminal -> copy-paste this script`s contents into the terminal
#ACHTUNG, the zips will be deleted afterwards, do NOT copy the last fer rows of the script if you are unsure what you are doing


$Winrar = "C:\Program Files\WinRAR\WinRAR.exe"
$Path = "C:\Users\woode\OneDrive - Charité - Universitätsmedizin Berlin\DOCTORATE\Cytomata_data\iahp_panel_3\fcs\2_debarcoded\"
$Batch = "20250514"
$Name = $Batch+"_DEBARCODED_250522\"
$File = $Batch+"_Blut_Panel3_CV19.fcs"

New-Item -Path $Path -Name $Name -ItemType "directory"

$Zips = Get-ChildItem -filter "*.zip" -path $Path 

$Destination = $Path+$Name


Set-Location -Path $Path
foreach ($zip in $Zips)
{
    Rename-Item $zip -NewName ("BC_$($zip)")

}




Set-Location -Path $Path
$Zips = Get-ChildItem -filter "*.zip" -path $Path 

foreach ($zip in $Zips)
{
    Set-Location -Path $Path
    &$Winrar x $zip *.fcs $Destination+$zip'\'
    Get-Process winrar | Wait-Process
    Set-Location -Path $Destination+$zip
    $NewSuffix = '_'+$zip -replace '\+', '' -replace '.zip','.fcs'
    $NewFileName = $File -replace '.fcs$',$NewSuffix
    Rename-Item $File -NewName $NewFileName
    Move-Item -Path $NewFileName -Destination $Destination
    Set-Location -Path $Destination
    Remove-Item * -Include *.zip -Exclude *.fcs -Force
}

Set-Location -Path $Path
$Zips = Get-ChildItem -filter "*.zip" -path $Path 
Remove-Item $Zips
