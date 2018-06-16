# Cleanse memory
rm(list = ls())
gc()

# KnitR SetUp location
KS   <- file.path("C:", "Users", "fpadt", "OneDrive - GrandVision", "Documents", "RW",
                  "QAR", "00_RProj", "00_Global", "KnitR_SetUp.R")

# Data Settings
pXLSX    <- "RSPC-Schedule" 
pWSHT    <- TRUE
newfiles <- TRUE

# Load functions
invisible(source(KS))
library(later)

fWalkTheDog <- function(x){
   later(fWatchDog, x)
 }

fGetACC <- function(){ 
  
  RET <- 
    fReadSAPTable(
      pSystID   = "BP1", 
      pClient   = "300", 
      pTable    = "USR02", 
      pOptions  = list("CLASS  like 'EXT%'", "AND",
                       "UFLAG  = '0'"),
      pFields   = list(
        'BNAME', 'TRDAT', 'LTIME', 
        'CLASS', 'UFLAG', 'GLTGB')) %>%
    .[, LTIME := as.numeric(LTIME)] %T>%
    setcolorder(
      c("BNAME", "SYSTID", "CLIENT", "CLASS", 
        "TRDAT", "LTIME" , "GLTGB" , "UFLAG"))   
  
  return(RET)}

fWatchDog <- function(){
  load(file = "dtACC.RData")  
  
  dtNEW <- fGetACC() 
    
  dtACC <- 
    rbindlist(
      list(
        dtACC, 
        dtNEW)) %>%
    unique()  %T>%
    setcolorder(names(dtNEW))  %T>%
    setkey("BNAME")
  
  save(dtACC, file = "dtACC.RData")
  
  print(paste("Bark!", "at", Sys.time()))
}

later(function(){
  lapply(
    as.list(seq.int(from = 60, 
                    to   = 12*3600, 
                    by   = 300)), 
    fWalkTheDog )}, 5)


later(function(){
  lapply(
    as.list(seq.int(from = 0, 
                    to   = 60, 
                    by   = 10)), 
    fWalkTheDog )}, 0)
