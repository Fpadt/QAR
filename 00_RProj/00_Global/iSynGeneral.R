# Floris Padt
# 4-5-2015

#### Library ####
library(knitr     , verbose = FALSE, quietly = TRUE)
library(printr    , verbose = FALSE, quietly = TRUE)
library(xlsx      , verbose = FALSE, quietly = TRUE)
library(data.table, verbose = FALSE, quietly = TRUE)
library(RODBC     , verbose = FALSE, quietly = TRUE)
library(ggplot2   , verbose = FALSE, quietly = TRUE)
library(reshape2  , verbose = FALSE, quietly = TRUE)
library(stringr   , verbose = FALSE, quietly = TRUE)

# ---- ReadingData ----
fGetTable <- function(pTable, pKey, pSystID = "BA1", pClient = "200"){
  
  if(!missing(pSystID)){
    l_Table <- paste0(pSystID, "C", pClient, "_", pTable )
  } else {
    l_Table <- pTable
  }

  # Open COnnection to DataBase
  A2R    <- odbcConnect("ACCESS2R")
  
  dtTABLE <- as.data.table(
    sqlFetch(A2R, l_Table, 
             stringsAsFactors = FALSE)
  )
  
  # Set Key
  if(!missing(pKey)){
    setkeyv(dtTABLE, pKey)  
  }
  
  # Get Attributes
  if(!missing(pSystID)){
    dfCMT <- sqlQuery(A2R, 
                      paste("SELECT START, DTIME, RECORDCOUNT", 
                            "FROM zsDD02_SLCT",
                            "WHERE ((SYSTEMID=", 
                            paste0("'", pSystID, "'"),  
                            ") AND (CLIENT=", 
                            pClient, ") AND (TABNAME=", 
                            paste0("'", pTable , "'"), 
                            "));")) 
    
    attr(dtTABLE, "RefreshDate")      <- dfCMT[["START"]]
    attr(dtTABLE, "RecordsExtracted") <- dfCMT[["RECORDCOUNT"]]
  }
  close(A2R)
  
  return(dtTABLE)
}

# ---- WriteResults ----
fWriteToSheet <- function(pData, pPath, pFileName, pSheetName, pAppend=FALSE){
  pFileName <- paste0(pPath, "/", pFileName, ".xlsx")
  write.xlsx(pData, 
             file = pFileName, sheetName= pSheetName, 
             col.names=TRUE, row.names=FALSE, append=pAppend, showNA=TRUE)
}