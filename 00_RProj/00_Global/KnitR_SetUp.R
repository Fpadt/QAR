knitr::opts_chunk$set(
  echo    = FALSE,
  eval    = TRUE,
  message = FALSE,
  warning = FALSE,
  image   = TRUE,
  results = "hide"
)

# folder locations
RW   <- file.path("~", "RW")
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

gvLOGO_PATH <- file.path(FIG, "GV_LOGO.png") 
if (!file.exists(gvLOGO_PATH)) {
  download.file(
    url      = "http://www.grandvision.com/img/logoGV.png",
    destfile = gvLOGO_PATH,
    mode     = 'wb')
}
knitr::include_graphics(gvLOGO_PATH)
