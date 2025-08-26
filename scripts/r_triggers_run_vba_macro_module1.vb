Option Explicit

'———————————————————————
' 1) Delete blank rows in A21:D40 of a given sheet
Sub DeleteEmptyRowsForSheet(wsName As String)
    Dim ws As Worksheet, rng As Range, i As Long
    On Error Resume Next
      Set ws = ThisWorkbook.Worksheets(wsName)
    On Error GoTo 0
    If ws Is Nothing Then Exit Sub

    Application.ScreenUpdating = False
    Set rng = ws.Range("A21:D40")
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
' 2) Copy the first chart on wsName into Word as a linked OLE object
Sub CreateWordForSheet(wsName As String, outputDoc As String)
    Dim wdApp As Object, wdDoc As Object, wrange As Object, ch As ChartObject

    With ThisWorkbook.Worksheets(wsName)
        If .ChartObjects.Count = 0 Then Exit Sub
        Set ch = .ChartObjects(1)
    End With

    ch.Copy

    Set wdApp = CreateObject("Word.Application")
    wdApp.Visible = False
    Set wdDoc = wdApp.Documents.Add

    ' give the clipboard a moment
    DoEvents: Application.Wait Now + TimeValue("0:00:01")

    Set wrange = wdDoc.Range(0, 0)
    wrange.Paste

    wdDoc.SaveAs2 ThisWorkbook.Path & "\" & outputDoc
    wdDoc.Close False
    wdApp.Quit
End Sub

'———————————————————————
' 3) Refresh one sheet, clean it, export its chart
Sub ProcessExhibit(wsName As String, connName As String, outputDoc As String)
    Dim ws As Worksheet

    ' a) Synchronous Power Query refresh via its ListObject
    Set ws = ThisWorkbook.Worksheets(wsName)
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

    ' b) Delete blanks
    DeleteEmptyRowsForSheet wsName

    ' c) Export chart to Word
    CreateWordForSheet wsName, outputDoc
End Sub

'———————————————————————
' 4) Five tiny wrappers — these are the macros PowerShell will call:
Public Sub Process4a():  ProcessExhibit "Exhibit 4 - NCVNML WageChart", "Query - Exhibit4a", "ex4a_linked.docx": End Sub
Public Sub Process4b():  ProcessExhibit "Exhibit 4 - SCVSML WageChart", "Query - Exhibit4b", "ex4b_linked.docx": End Sub
Public Sub Process5():   ProcessExhibit "Exhibit 5 - CVML WageChart",   "Query - Exhibit5",    "ex5_linked.docx":  End Sub
Public Sub Process9():   ProcessExhibit "Exhibit 9 - Ed. Attainment",   "Query - Exhibit9",    "ex9_linked.docx":  End Sub
Public Sub Process11():  ProcessExhibit "Exhibit 11 - Template",       "Query - Exhibit11",   "ex11_linked.docx": End Sub
