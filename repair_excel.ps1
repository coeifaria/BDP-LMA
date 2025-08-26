param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

# 1) Wait until R has finished writing the file
$maxTries = 10; $n = 0
while ($n -lt $maxTries) {
    try {
        $stream = [IO.File]::Open($FilePath, 'Open', 'ReadWrite', 'None')
        $stream.Close()
        break
    } catch {
        Start-Sleep -Milliseconds 300
        $n++
    }
}
if ($n -ge $maxTries) {
    Write-Host "❗️ $FilePath still locked after waiting. Skipping."
    exit 1
}

# 2) Give the file system a moment
Start-Sleep -Seconds 1

# 3) Open & re-save in Excel via COM
$xl = New-Object -Com Excel.Application
$xl.DisplayAlerts = $false
$xl.Visible       = $false
try {
    $wb = $xl.Workbooks.Open((Resolve-Path $FilePath).Path)
    $wb.Save()
    $wb.Close($false)
    Write-Host "Processed $FilePath"
} catch {
    Write-Host "Failed to process ${FilePath}:`n $_"
} finally {
    $xl.Quit()
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl) | Out-Null
}
