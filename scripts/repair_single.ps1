param(
  [Parameter(Mandatory=$true)] [string]$MacroName,
  [Parameter(Mandatory=$true)] [string]$WorkbookPath
)

# 1) Launch Excel via COM
$xl = New-Object -ComObject Excel.Application
$xl.Visible = $false
$xl.DisplayAlerts = $false
$xl.AutomationSecurity = 1  # allow macros

# 2) Open the workbook ReadOnly so no write‐lock
#    UpdateLinks=0, ReadOnly=$true
$wb = $xl.Workbooks.Open($WorkbookPath, 0, $true)

try {
    # 3) Give Excel a moment to initialize
    Start-Sleep -Seconds 1
    # 4) Run your public Sub by name
    $xl.Run($MacroName)
}
catch {
    Write-Error "Error running macro '$MacroName': $($_.Exception.Message)"
}
finally {
    # 5) Always close & quit
    if ($wb) { $wb.Close($false) }
    $xl.Quit()

    # 6) Release COM references and force GC
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb)  | Out-Null
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl)  | Out-Null
    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}
