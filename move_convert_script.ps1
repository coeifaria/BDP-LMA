# move_convert_script.ps1
param(
    [Parameter(Mandatory=$true)]
    [string]$SourcePath,

    [Parameter(Mandatory=$true)]
    [string]$DestinationPath
)

try {
    Move-Item -Path $SourcePath -Destination $DestinationPath -Force -ErrorAction Stop
    Write-Host "Successfully moved '$SourcePath' to '$DestinationPath'"
}
catch {
    Write-Host "Error moving file: $($_.Exception.Message)"
    exit 1 # Indicate failure
}