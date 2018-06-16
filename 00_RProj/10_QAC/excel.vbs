' http://www.listendata.com/2016/07/run-vba-in-r.html

Set objExcel = CreateObject("Excel.Application") 
objExcel.Visible = True
objExcel.DisplayAlerts=False
Set wb = objExcel.Workbooks.Open("C:/Users/fpadt/OneDrive - GrandVision/Documents/RW/QAR/60_Results/RSPC-Schedule.xlsx")
'objExcel.Application.Run "Book1.xls!macro1"
'wb.save