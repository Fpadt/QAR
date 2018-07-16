' http://www.listendata.com/2016/07/run-vba-in-r.html

Set objExcel = CreateObject("Excel.Application") 
objExcel.Visible = True
objExcel.DisplayAlerts=False

Set wb = objExcel.Workbooks.Open("C:\Users\fpadt\OneDrive - GrandVision\Documents\RW\QAR\20_Notebooks\bi_planning\Results\Main.xlsm")

objExcel.Application.Run "Main.xlsm!Main"

wb.close 



