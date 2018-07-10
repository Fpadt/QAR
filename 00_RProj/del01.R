SID.lng  <- c("RD1100", "RA1300", "RP1300", "RS1300", "RT1300" )
SID.sht  <- substr(SID.lng, 1, 3)

pXLSX <-  "DataSource"

if (file.exists(paste0(RES, "/", pXLSX, ".xlsx")) == TRUE) {
  invisible(file.remove(paste0(RES, "/", pXLSX, ".xlsx")))
}

dtROOSOURCE  <- 
  read_and_union("ROOSOURCE")           %>%
  .[, TOT := .N, by = .(OLTPSOURCE)]    %>%
  .[TOT < 6]                            %>%
  dcast.data.table(
    formula = OLTPSOURCE ~ SYSTID, 
    fun.aggregate = length, 
    value.var = "SYSTID")               %T>%
  setcolorder(c("OLTPSOURCE", SID.sht))



if (TRUE == TRUE) {
  fWriteToSheet(
    dtROOSOURCE, 
    RES, pXLSX, "ROOSOURCE", pAppend = FALSE )}

dtROOSPRMSC  <- 
  read_and_union("ROOSPRMSC")           %>%
  .[, TOT := .N, by = .(OLTPSOURCE)]    %>%
  .[TOT < 6]                            %>%
  dcast.data.table(
    formula = OLTPSOURCE ~ SYSTID, 
    fun.aggregate = length, 
    value.var = "SYSTID")               %T>%
  setcolorder(c("OLTPSOURCE", SID.sht))

if (TRUE == TRUE) {
  fWriteToSheet(
    dtROOSPRMSC, 
    RES, pXLSX, "ROOSPRMSC", pAppend = TRUE )}