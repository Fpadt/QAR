' http://www.listendata.com/2016/07/run-vba-in-r.html

Set objExcel = CreateObject("Excel.Application") 
objExcel.Visible = True
objExcel.DisplayAlerts=False

Set wb = objExcel.Workbooks.Open("C:\Users\fpadt\OneDrive - GrandVision\Documents\RW\QAR\20_Notebooks\bi_planning\Results\PLAN_20180710_W28.csv")

'objExcel.Application.Run "Book1.xls!macro1"
'wb.saveas

