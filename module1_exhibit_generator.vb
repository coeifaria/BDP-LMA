' =========================================
' FIXED VERSION - DELETES ROWS BUT NO LOOPING
' PUT ALL OF THIS CODE IN ONE STANDARD MODULE
' =========================================


Sub MonitorExhibitsFile()
    Dim exhibitsPath As String
    Dim currentModTime As Date
    Static lastModTime As Date
    
    ' Build path to exhibits.xlsx in same directory as this workbook
    exhibitsPath = ThisWorkbook.Path & "\exhibits.xlsx"
    
    If Dir(exhibitsPath) <> "" Then
        currentModTime = FileDateTime(exhibitsPath)
        
        ' If this is first check, just store the time
        If lastModTime = 0 Then
            lastModTime = currentModTime
        ElseIf currentModTime > lastModTime Then
            ' File was modified since last check
            lastModTime = currentModTime  ' Update the timestamp
            
            ' Refresh data first, then run cleanup
            ThisWorkbook.RefreshAll
            Application.Wait (Now + TimeValue("0:00:08"))  ' Wait for refresh
            DeleteEmptyRowsOnce
            Exit Sub
        End If
        
        ' Check again in 30 seconds
        Application.OnTime Now + TimeValue("0:00:02"), "MonitorExhibitsFile"
    Else
        ' exhibits.xlsx not found - check again in 30 seconds
        Application.OnTime Now + TimeValue("0:00:02"), "MonitorExhibitsFile"
    End If
End Sub



Sub DeleteEmptyRowsOnce()
    Dim ws As Worksheet
    Dim rng As Range
    Dim i As Long
    Dim totalDeleted As Long
    Dim currentSheet As String
    
    ' Prevent multiple simultaneous runs
    Static IsRunning As Boolean
    If IsRunning Then Exit Sub
    IsRunning = True
    
    ' Store current active sheet
    On Error Resume Next
    currentSheet = ActiveSheet.Name
    On Error GoTo 0
    
    totalDeleted = 0
    Application.ScreenUpdating = False
    ' Keep EnableEvents = True for auto-refresh on file open
    
    ' Process Sheet 1: Exhibit 4 - NCVNML WageChart
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets("Exhibit 4 - NCVNML WageChart")
    On Error GoTo 0
    
    If Not ws Is Nothing Then
        Set rng = ws.Range("A13:D20")
        For i = rng.Rows.Count To 1 Step -1
            Dim rowRange As Range
            Set rowRange = rng.Rows(i)
            
            ' Check if Description column (A) has 0 or is effectively empty
            Dim descValue As Variant
            descValue = rowRange.Cells(1, 1).Value
            
            If descValue = 0 Or descValue = "" Or descValue = " " Or IsNull(descValue) Then
                rowRange.EntireRow.Delete
                totalDeleted = totalDeleted + 1
            End If
        Next i
        Set ws = Nothing
    End If
    
    ' Process Sheet 2: Exhibit 4 - SCVSML WageChart
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets("Exhibit 4 - SCVSML WageChart")
    On Error GoTo 0
    
    If Not ws Is Nothing Then
        Set rng = ws.Range("A13:D20")
        For i = rng.Rows.Count To 1 Step -1
            Set rowRange = rng.Rows(i)
            descValue = rowRange.Cells(1, 1).Value
            
            If descValue = 0 Or descValue = "" Or descValue = " " Or IsNull(descValue) Then
                rowRange.EntireRow.Delete
                totalDeleted = totalDeleted + 1
            End If
        Next i
        Set ws = Nothing
    End If
    
    ' Process Sheet 3: Exhibit 5 - CVML WageChart
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets("Exhibit 5 - CVML WageChart")
    On Error GoTo 0
    
    If Not ws Is Nothing Then
        Set rng = ws.Range("A13:D20")
        For i = rng.Rows.Count To 1 Step -1
            Set rowRange = rng.Rows(i)
            descValue = rowRange.Cells(1, 1).Value
            
            If descValue = 0 Or descValue = "" Or descValue = " " Or IsNull(descValue) Then
                rowRange.EntireRow.Delete
                totalDeleted = totalDeleted + 1
            End If
        Next i
        Set ws = Nothing
    End If
    
    ' Restore settings
    Application.ScreenUpdating = True
    
    ' Return to original sheet
    If currentSheet <> "" Then
        On Error Resume Next
        ThisWorkbook.Worksheets(currentSheet).Activate
        On Error GoTo 0
    End If
    
    ' Reset the running flag
    IsRunning = False
    
    Application.OnTime Now + TimeValue("0:00:02"), "CreateWordDocumentWithLinkedChart"

    MsgBox totalDeleted & " empty rows deleted successfully!"

End Sub

Sub RefreshAndDeleteOnce()
    ' Prevent multiple runs
    Static IsRefreshing As Boolean
    If IsRefreshing Then
        MsgBox "Refresh already in progress..."
        Exit Sub
    End If
    
    IsRefreshing = True
    
    ' Small delay to ensure any auto-refresh is complete
    Application.Wait (Now + TimeValue("0:00:02"))
    
    ' Run the cleanup ONCE
    Call DeleteEmptyRowsOnce
    
    IsRefreshing = False
    
    MsgBox "Cleanup completed! Word documents will be created shortly..."
End Sub

' =========================================
' PUT THIS IN THISWORKBOOK (double-click ThisWorkbook in VBA)
' =========================================


Sub CheckRefreshAndClean()
    ' Check if any queries are still refreshing
    Dim conn As WorkbookConnection
    Dim stillRefreshing As Boolean
    
    stillRefreshing = False
    
    ' Check all connections to see if any are still refreshing
    For Each conn In ThisWorkbook.Connections
        If conn.Type = xlConnectionTypeOLEDB Or conn.Type = xlConnectionTypeODBC Then
            ' Power Query connections don't have a direct "refreshing" property
            ' So we'll use a different approach
        End If
    Next conn
    
    ' Simpler approach: just wait a bit longer then run cleanup
    Application.OnTime Now + TimeValue("0:00:02"), "DeleteEmptyRowsOnce"
End Sub

Sub CreateWordDocumentWithLinkedChart()
    Dim wordApp As Object
    Dim wordDoc As Object
    Dim ws As Worksheet
    Dim chartObj As ChartObject
    Dim originalWordAppWasRunning As Boolean
    Dim saveSuccessful As Boolean
    Dim savedFiles As String

    ' Constants for PasteSpecial (using numerical values for robustness if reference is not set)
    Const wdPasteOLEObject As Long = 0 ' For pasting an OLE object (the chart itself)
    Const wdDoNotSaveChanges As Long = 0 ' For Close/Quit methods, to avoid prompts

    ' Prevent multiple simultaneous runs
    Static IsCreating As Boolean
    If IsCreating Then Exit Sub
    IsCreating = True

    Application.ScreenUpdating = False ' Turn off screen updating for speed

    ' Check if Word is already running
    On Error Resume Next ' Temporarily disable error handling for GetObject
    Set wordApp = GetObject(, "Word.Application")
    If wordApp Is Nothing Then
        ' Word is not running, so create a new instance
        Set wordApp = CreateObject("Word.Application")
        originalWordAppWasRunning = False ' We opened Word
    Else
        ' Word was already running
        originalWordAppWasRunning = True
    End If
    On Error GoTo 0 ' Re-enable error handling

    If wordApp Is Nothing Then
        MsgBox "Failed to start or connect to Microsoft Word.", vbCritical
        IsCreating = False
        Application.ScreenUpdating = True
        Exit Sub
    End If

    ' Make Word visible
    wordApp.Visible = True

    ' Define sheets and corresponding filenames
    Dim sheetNames As Variant
    Dim fileNames As Variant
    Dim i As Long

    sheetNames = Array("Exhibit 4 - NCVNML WageChart", "Exhibit 4 - SCVSML WageChart", "Exhibit 5 - CVML WageChart", "Exhibit 9 - Ed. Attainment", "Exhibit 11 - Template")
    fileNames = Array("ex4a_linked.docx", "ex4b_linked.docx", "ex5_linked.docx", "ex9_linked.docx", "ex11_linked.docx")

    savedFiles = "" ' Track successfully saved files

    ' Process each sheet separately
    For i = LBound(sheetNames) To UBound(sheetNames)
        On Error Resume Next ' Temporarily disable error handling for sheet access
        Set ws = ThisWorkbook.Worksheets(sheetNames(i))
        On Error GoTo 0 ' Re-enable error handling

        If Not ws Is Nothing Then ' Check if the sheet exists
            Debug.Print "Processing sheet: " & sheetNames(i)

            If ws.ChartObjects.Count > 0 Then ' Check if charts exist on the sheet
                Set chartObj = ws.ChartObjects(1) ' Get the first chart on the sheet
                Debug.Print "Found chart: " & chartObj.Name

                ' Create new document for this chart
                Set wordDoc = wordApp.Documents.Add

                If wordDoc Is Nothing Then
                    Debug.Print "Failed to create Word document for " & sheetNames(i)
                    GoTo NextSheet
                End If

                ' Clear clipboard (important before copying)
                Application.CutCopyMode = False

                ' Select and Copy the chart
                chartObj.Select
                chartObj.Copy

                ' Give the clipboard a moment to get the chart
                Application.Wait (Now + TimeValue("0:00:01")) ' Wait for 1 second

                ' Use Range approach to paste the chart
                Dim pasteSuccess As Boolean
                Dim insertRange As Object
                pasteSuccess = False

                ' Get the insertion point at the beginning of the document
                Set insertRange = wordDoc.Range(0, 0)

                ' Method 1: Try using Range.PasteSpecial
                On Error Resume Next
                insertRange.PasteSpecial _
                    Link:=True, _
                    DataType:=5, _
                    DisplayAsIcon:=False
                If Err.Number = 0 Then
                    pasteSuccess = True
                    Debug.Print "Chart pasted successfully as linked Excel Chart Object using Range method."
                End If
                Err.Clear
                On Error GoTo 0

                ' Method 2: If Range method failed, try the Selection approach
                If Not pasteSuccess Then
                    On Error Resume Next
                    ' Ensure Word is active and ready
                    wordApp.Activate
                    wordDoc.Activate
                    
                    ' Move to beginning of document
                    wordDoc.Application.Selection.HomeKey Unit:=6 ' Move to beginning of document (wdStory = 6)
                    
                    ' Try PasteSpecial with explicit parameters
                    wordDoc.Application.Selection.PasteSpecial _
                        Link:=True, _
                        DataType:=5, _
                        DisplayAsIcon:=False, _
                        IconFileName:="", _
                        IconIndex:=0, _
                        IconLabel:=""
                    
                    If Err.Number = 0 Then
                        pasteSuccess = True
                        Debug.Print "Chart pasted successfully using Selection method."
                    End If
                    Err.Clear
                    On Error GoTo 0
                End If

                ' Method 3: Final fallback - basic paste with link
                If Not pasteSuccess Then
                    On Error Resume Next
                    wordDoc.Application.Selection.HomeKey Unit:=6 ' Move to beginning of document
                    wordDoc.Application.Selection.PasteSpecial Link:=True
                    If Err.Number = 0 Then
                        pasteSuccess = True
                        Debug.Print "Chart pasted successfully using basic PasteSpecial with Link."
                    End If
                    Err.Clear
                    On Error GoTo 0
                End If

                If pasteSuccess Then
                    Debug.Print "Successfully added chart to document."
                    Application.CutCopyMode = False ' Clear clipboard mode after pasting

                    ' Save the document
                    Dim wordFileName As String
                    Dim finalSavedPath As String
                    wordFileName = ThisWorkbook.Path & "\" & fileNames(i)
                    saveSuccessful = False

                    ' Try to save to primary location
                    On Error Resume Next
                    wordDoc.SaveAs2 wordFileName
                    If Err.Number = 0 Then
                        saveSuccessful = True
                        finalSavedPath = wordFileName
                        Debug.Print "Saved to: " & finalSavedPath
                    Else
                        Debug.Print "Failed to save to workbook path: " & Err.Description
                        Err.Clear
                        
                        ' Try Desktop as fallback
                        Dim desktopPath As String
                        desktopPath = CreateObject("WScript.Shell").SpecialFolders("Desktop")
                        Dim desktopFileName As String
                        desktopFileName = desktopPath & "\" & fileNames(i)
                        
                        wordDoc.SaveAs2 desktopFileName
                        If Err.Number = 0 Then
                            saveSuccessful = True
                            finalSavedPath = desktopFileName
                            Debug.Print "Saved to Desktop: " & finalSavedPath
                        Else
                            Debug.Print "Failed to save to Desktop: " & Err.Description
                        End If
                    End If
                    On Error GoTo 0

                    If saveSuccessful Then
                        savedFiles = savedFiles & finalSavedPath & vbCrLf
                        ' Close the document after saving
                        wordDoc.Close SaveChanges:=wdDoNotSaveChanges
                    Else
                        Debug.Print "Could not save document for " & sheetNames(i)
                        ' Leave document open if save failed
                    End If

                Else
                    Debug.Print "All linking methods failed for chart from " & sheetNames(i)
                    ' Close the failed document
                    wordDoc.Close SaveChanges:=wdDoNotSaveChanges
                End If

            Else
                Debug.Print "No charts found on sheet: " & sheetNames(i)
            End If
        Else
            Debug.Print "Sheet not found: " & sheetNames(i)
        End If

NextSheet:
        ' Continue to next sheet
    Next i

    ' Final cleanup and user notification
    Application.ScreenUpdating = True ' Re-enable screen updating
    IsCreating = False ' Reset running flag

    ' Close Word if we opened it
    If Not originalWordAppWasRunning Then
        On Error Resume Next
        wordApp.Quit SaveChanges:=wdDoNotSaveChanges
        On Error GoTo 0
    End If

    ' Show completion message
    If Len(savedFiles) > 0 Then
        MsgBox "Word documents created with linked charts!" & vbCrLf & vbCrLf & "Saved files:" & vbCrLf & savedFiles, vbInformation
        ' Open the folder where files were saved (use the first successful save location)
        Dim firstFilePath As String
        firstFilePath = Split(savedFiles, vbCrLf)(0)
        If Len(firstFilePath) > 0 Then
            Shell "explorer.exe /select," & Chr(34) & firstFilePath & Chr(34), vbNormalFocus
        End If
    Else
        MsgBox "No documents were successfully created. Check that the specified worksheets exist and contain charts.", vbExclamation
    End If

    ' Final cleanup of object variables
    Set chartObj = Nothing
    Set wordDoc = Nothing
    Set wordApp = Nothing

End Sub




