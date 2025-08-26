param(
    [Parameter(Mandatory=$true)]
    [string]$MacroName,

    [Parameter(Mandatory=$true)]
    [string]$WorkbookPath,

    [Parameter(Mandatory=$true)]
    [string]$DataFilePath
)

# 1) Launch Excel via COM
$xl = New-Object -ComObject Excel.Application
$xl.Visible = $false
$xl.DisplayAlerts = $false
$xl.AutomationSecurity = 1 # Allow macros
$wb = $null # Initialize workbook object to null

try {
    # 2) Open the workbook (MOVED INSIDE THE 'TRY' BLOCK)
    # This ensures that if opening fails, we still run the cleanup code in 'finally'
    $wb = $xl.Workbooks.Open($WorkbookPath, 0, $false)

    # 3) Give Excel a moment to initialize
    Start-Sleep -Seconds 1

    # *** Update the DynamicPath Named Range ***
    $dataFolder = Split-Path -Parent $DataFilePath
    $dynamicPathRange = $null
    foreach ($n in $wb.Names) {
        if ($n.Name -eq "DynamicPath") {
            $dynamicPathRange = $n.RefersToRange
            break
        }
    }

    if ($dynamicPathRange -ne $null) {
        # Append "\" to ensure it's a folder path as expected by Power Query
        $dynamicPathRange.Cells(1,1).Value2 = $dataFolder + "\"
        Write-Host "Updated DynamicPath named range in '$WorkbookPath' to: $($dataFolder)\"
    } else {
        Write-Host "WARNING: Named range 'DynamicPath' not found in '$WorkbookPath'. Power Query may fail."
    }
    # *** END NEW LOGIC ***

    # 4) Run your public Sub by name
    $xl.Run($MacroName)

    # 5) Save changes (including the updated named range and any macro output)
    $wb.Save()
}
catch {
    Write-Error "Error running macro '$MacroName' or updating named range: $($_.Exception.Message)"
}
finally {
    # 6) Always close & quit
    if ($wb) { $wb.Close($false) } # Close without prompting
    $xl.Quit()

    # 7) Release COM references and force garbage collection
    # This is the single, correct cleanup block.
    if ($wb) {
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($wb) | Out-Null
    }
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($xl) | Out-Null
    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}