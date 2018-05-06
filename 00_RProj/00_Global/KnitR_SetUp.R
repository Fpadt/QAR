knitr::opts_chunk$set(
  echo    = FALSE,
  eval    = TRUE,
  message = FALSE,
  warning = FALSE,
  image   = TRUE,
  results = "hide"
)

# folder locations
RW   <- file.path("C:", "Users", "fpadt", "OneDrive - GrandVision", "Documents", "RW")
PRJ  <- file.path(RW , "QAR")
MOD  <- file.path(PRJ, "00_RProj")
RAW  <- file.path(PRJ, "10_RawData")
DAT  <- file.path(PRJ, "11_PrepData")
ANA  <- file.path(PRJ, "30_Analysis")
RES  <- file.path(PRJ, "60_Results")
FIG  <- file.path(PRJ, "70_Figures")
SAP  <- DAT 

# Load functions
invisible(source(file.path(MOD, "00_Global", "iSynGeneral.R")))
invisible(source(file.path(MOD, "00_Global", "SAP2R.R")))

dtPATH <- 
  data.table(
    DESCR = c("RW", "PRJ", "MOD", "RAW", "DAT", "ANA", "RES", "FIG"),
    PATH  = c( RW ,  PRJ ,  MOD ,  RAW ,  DAT ,  ANA ,  RES ,  FIG )
  )

# Open Excel for storing results
if (file.exists(paste0(RES, "/", pXLSX, ".xlsx")) == TRUE) {
  invisible(file.remove(paste0(RES, "/", pXLSX, ".xlsx")))
}

if (pWSHT == TRUE) {
  fWriteToSheet(
    dtPATH, 
    RES, pXLSX, "PATHS", pAppend = FALSE )}

gvLOGO_PATH <- file.path(FIG, "GV_LOGO.png") 
if (!file.exists(gvLOGO_PATH)) {
  download.file(
    url      = "http://www.grandvision.com/img/logoGV.png",
    destfile = gvLOGO_PATH,
    mode     = 'wb')
}
knitr::include_graphics(gvLOGO_PATH)
