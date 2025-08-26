Option Explicit

'———————————————————————
' 1) Delete blank rows in A21:J40 of a given sheet
'———————————————————————
Sub DeleteEmptyRowsForSheet(wsName As String)
    Dim ws As Worksheet, rng As Range, i As Long
    On Error Resume Next: Set ws = ThisWorkbook.Worksheets(wsName): On Error GoTo 0
    If ws Is Nothing Then Exit Sub

    Application.ScreenUpdating = False
    Set rng = ws.Range("A21:J40")
    For i = rng.Rows.Count To 1 Step -1
        With rng.Rows(i)
            If Trim(.Cells(1, 1).Value & "") = "" Or .Cells(1, 1).Value = 0 Then
                .EntireRow.Delete
            End If
        End With
    Next i
    Application.ScreenUpdating = True
End Sub

'———————————————————————
' 2) Copy first chart on wsName into Word
'———————————————————————
Sub CreateWordForSheet(wsName As String, outputDoc As String)
    Dim wdApp As Object, wdDoc As Object, wr As Object, ch As ChartObject
    With ThisWorkbook.Worksheets(wsName)
        If .ChartObjects.Count = 0 Then Exit Sub
        Set ch = .ChartObjects(1)
    End With
    ch.Copy
    Set wdApp = CreateObject("Word.Application")
    wdApp.Visible = False
    Set wdDoc = wdApp.Documents.Add
    DoEvents: Application.Wait Now + TimeValue("0:00:01")
    Set wr = wdDoc.Range(0, 0)
    wr.Paste
    wdDoc.SaveAs2 ThisWorkbook.Path & "\" & outputDoc
    wdDoc.Close False
    wdApp.Quit
End Sub

'———————————————————————
' 3) Refresh, clean, FORMAT, then export a sheet's chart (CORRECTED WORKFLOW)
'———————————————————————
Sub ProcessExhibit(wsName As String, connName As String, outputDoc As String, _
                   Optional bDeleteRows As Boolean = True, Optional formatType As String = "None")
    
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets(wsName)
    
    ' 1. Refresh Data
    If ws.ListObjects.Count > 0 Then
        With ws.ListObjects(1).QueryTable
            .BackgroundQuery = False
            .Refresh
        End With
    Else
        With ThisWorkbook.Connections(connName).OLEDBConnection
            .BackgroundQuery = False
        End With
        ThisWorkbook.Connections(connName).Refresh
    End If
    Application.Wait Now + TimeValue("0:00:02")
    
    ' 2. Delete Rows (if applicable)
    If bDeleteRows Then
        DeleteEmptyRowsForSheet wsName
    End If
    
    ' 3. Format Columns
    Select Case formatType
        Case "Accounting"
            FormatAsAccounting wsName
        Case "Percentage"
            FormatAsPercentage wsName
    End Select
    
    ' 4. Create Word Doc
    CreateWordForSheet wsName, outputDoc
End Sub

'———————————————————————
' 4) Formats specified columns to Accounting
'———————————————————————
Sub FormatAsAccounting(wsName As String)
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(wsName)
    On Error GoTo 0
    If ws Is Nothing Then Exit Sub
    ws.Range("B:D, G:I").NumberFormat = "_($* #,##0.00_);_($* (#,##0.00);_($* ""-""??_);_(@_)"
End Sub

'———————————————————————
' 5) Formats specified columns to Percentage
'———————————————————————
Sub FormatAsPercentage(wsName As String)
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(wsName)
    On Error GoTo 0
    If ws Is Nothing Then Exit Sub
    ws.Range("B:E").NumberFormat = "0%"
End Sub


'———————————————————————
' 6) Wrapper subs that PowerShell will call (UPDATED)
'———————————————————————
Public Sub Process2()
    ProcessExhibit "Exhibit 2 - Percentage Change", "Query - Exhibit2", "ex2_linked.docx", False
End Sub

Public Sub Process4a()
    ProcessExhibit "Exhibit 4 - NCVNML WageChart", "Query - Exhibit4a", "ex4a_linked.docx", True, "Accounting"
End Sub

Public Sub Process4b()
    ProcessExhibit "Exhibit 4 - SCVSML WageChart", "Query - Exhibit4b", "ex4b_linked.docx", True, "Accounting"
End Sub

Public Sub Process5()
    ProcessExhibit "Exhibit 5 - CVML WageChart", "Query - Exhibit5", "ex5_linked.docx", True, "Accounting"
End Sub

Public Sub Process9()
    ProcessExhibit "Exhibit 9 - Ed. Attainment", "Query - Exhibit9", "ex9_linked.docx", True, "Percentage"
End Sub

Public Sub Process11()
    ProcessExhibit "Exhibit 11 - Template", "Query - Exhibit11", "ex11_linked.docx"
End Sub