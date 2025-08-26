param(
  [Parameter(Mandatory=$true)]
  [string]$MacroName,

  [Parameter(Mandatory=$true)]
  [string]$WorkbookPath
)

# 1) Launch Excel via COM
$xl = New-Object -ComObject Excel.Application
$xl.Visible = $false
$xl.DisplayAlerts = $false
$xl.AutomationSecurity = 1  # msoAutomationSecurityLow to allow macros

# 2) Open your .xlsm in editable mode
#    Parameters: Filename, UpdateLinks=0, ReadOnly=$false
$wb = $xl.Workbooks.Open($WorkbookPath, 0, $false)

try {
    # 3) Give Excel a moment to initialize
    Start-Sleep -Seconds 1

    # 4) Run the macro (e.g. "Process4a")
    $xl.Run($MacroName)
}
catch {
    Write-Error "Error running macro '$MacroName': $($_.Exception.Message)"
}
finally {
    # 5) Always close & quit Excel
    if ($wb) { $wb.Close($false) }
    $xl.Quit()

    # 6) Release COM references & force garbage collection
    [Runtime.InteropServices.Marshal]::ReleaseComObject($wb)  | Out-Null
    [Runtime.InteropServices.Marshal]::ReleaseComObject($xl)  | Out-Null
    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}
