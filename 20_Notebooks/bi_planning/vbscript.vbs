' http://www.listendata.com/2016/07/run-vba-in-r.html

Set objExcel = CreateObject("Excel.Application") 
objExcel.Visible = True
objExcel.DisplayAlerts=False

Set wb = objExcel.Workbooks.Open("C:\Users\fpadt\OneDrive - GrandVision\Documents\RW\QAR\20_Notebooks\bi_planning\Results\Main.xlsm")

objExcel.Application.Run "Main.xlsm!Main"

Sub BI_Planning()
  Dim intRows   As Integer
  Dim intCols   As Integer
  Dim rngStart  As Range
  Dim rngDate   As Range
  Dim rngEntry  As Range
  Dim rngTotal  As Range

  ' set start anchor and determine region used
  Set wb = ThisWorkbook
  Set shtActive = wb.Sheets(1)
  Set rngStart = shtActive.Cells(1, 1)
  Set rngDate = shtActive.Cells(1, 12)
  
  With rngStart
    intRows = .CurrentRegion.Rows.Count
    intCols = .CurrentRegion.Columns.Count
  End With
    
  With shtActive.Range(rngStart, rngStart.End(xlToRight))
    With .Font
      .ThemeColor = xlThemeColorDark1
      .Bold = True
    End With
    .Interior.ThemeColor = xlThemeColorLight2
  End With

  ' Group columns for easy of use
  shtActive.Columns("D:J").Columns.Group

  With shtActive.Range(rngDate, rngDate.End(xlToRight))
    .NumberFormat = "yyyy mm dd / ddd"
    .Orientation = 90
  End With

  Set rngEntry = rngDate.Offset(1, -1).Resize(intRows - 1, intCols - 10)
                               
 ' Identify Planning lines
  With rngEntry
    .FormatConditions.Add Type:=xlExpression, Formula1:="=$K2=""PLN"""
    .FormatConditions(.FormatConditions.Count).SetFirstPriority
    With .FormatConditions(1).Interior
      .ThemeColor = xlThemeColorAccent5
      .TintAndShade = 0.799981688894314
    End With
    .FormatConditions(1).StopIfTrue = False
  End With
  
  ' Weekends
  With rngEntry
    .FormatConditions.Add Type:=xlExpression, Formula1:= _
        "=OR(WEEKDAY(K$1;1)=1;WEEKDAY(K$1;1)=7)"
    .FormatConditions(.FormatConditions.Count).SetFirstPriority
    With .FormatConditions(1).Interior
      .ThemeColor = xlThemeColorDark1
      .TintAndShade = -0.249946592608417
    End With
    .FormatConditions(1).StopIfTrue = False
  End With
  
  Set rngTotal = Range(rngDate.Offset(1, 0), rngDate.End(xlDown))
  With rngDate
    .Parent.Columns(rngDate.Column).Insert Shift:=xlToRight, _
            CopyOrigin:=xlFormatFromLeftOrAbove
    .Offset(0, -1).FormulaR1C1 = "Total"
  End With
  
  With rngTotal.Offset(0, -1)
    .FormulaR1C1 = "=SUM(RC[1]:RC[" & intCols - 11 & "])"
    With .Interior
      .ThemeColor = xlThemeColorAccent4
      .TintAndShade = 0.799981688894314
    End With
  End With
    
  rngStart.AutoFilter
  wb.Windows(1).DisplayGridlines = False
  shtActive.Cells.Select
  shtActive.Cells.EntireColumn.AutoFit

End Sub











