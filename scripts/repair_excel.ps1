param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

# -- wait until the file is unlocked --
$tries = 0
while ($tries -lt 10) {
    try {
        $f = [IO.File]::Open($FilePath, 'Open', 'ReadWrite', 'None')
        $f.Close()
        break
    } catch {
        Start-Sleep -Milliseconds 300
        $tries++
    }
}

# -- small pause to let the FS settle --
Start-Sleep -Seconds 1

# -- open & re-save in Excel COM --
$xl = New-Object -Com Excel.Application
$xl.DisplayAlerts = $false
$xl.Visible       = $false
try {
    $wb = $xl.Workbooks.Open((Resolve-Path $FilePath).Path)
    $wb.Save()
    $wb.Close($false)
    Write-Host "✔ $FilePath repaired"
} catch {
    Write-Host "✘ failed to repair $FilePath`n$_"
} finally {
    $xl.Quit()
    [Runtime.InteropServices.Marshal]::ReleaseComObject($xl) | Out-Null
}
