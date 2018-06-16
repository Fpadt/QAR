# Floris Padt
# 4-5-2015

#### Library ####
library(knitr     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(printr    , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE) # not available for 3.2.2
library(xlsx      , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE) # Java 64-Bit needed
suppressMessages(
  library(bit64     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE))
library(readxl    , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2   , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(reshape2  , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(stringr   , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)

library(magrittr  , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(fst       , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(RCurl     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(readr     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(tidyverse , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(purrr     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)


# library(forecast)
# library(fpp)
# library(RODBC     , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)


# ---- ReadingData ----
# fGetTable <- function(pTable, pKey, pSystID = "BA1", pClient = "200"){
#   
#   if(!missing(pSystID)){
#     l_Table <- paste0(pSystID, "C", pClient, "_", pTable )
#   } else {
#     l_Table <- pTable
#   }
# 
#   # Open COnnection to DataBase
#   A2R    <- odbcConnect("ACCESS2R")
#   
#   dtTABLE <- as.data.table(
#     sqlFetch(A2R, l_Table, 
#              stringsAsFactors = FALSE)
#   )
#   
#   # Set Key
#   if(!missing(pKey)){
#     setkeyv(dtTABLE, pKey)  
#   }
#   
#   # Get Attributes
#   if(!missing(pSystID)){
#     dfCMT <- sqlQuery(A2R, 
#                       paste("SELECT START, DTIME, RECORDCOUNT", 
#                             "FROM zsDD02_SLCT",
#                             "WHERE ((SYSTEMID=", 
#                             paste0("'", pSystID, "'"),  
#                             ") AND (CLIENT=", 
#                             pClient, ") AND (TABNAME=", 
#                             paste0("'", pTable , "'"), 
#                             "));")) 
#     
#     attr(dtTABLE, "RefreshDate")      <- dfCMT[["START"]]
#     attr(dtTABLE, "RecordsExtracted") <- dfCMT[["RECORDCOUNT"]]
#   }
#   close(A2R)
#   
#   return(dtTABLE)
# }



# ---- ReadingData ----
# Function to Read .txt file from specific System 
# ---- ReadingData ----
fGetEXPTable <- function(pTableName, pKey, pSystID, pClient = "300"){
  
  if (!missing(pSystID)) {
    l_Table <- paste0(pSystID, "C", pClient, "_", pTableName )
  } else {
    l_Table <- pTableName
  }
  
  # set Path
  cRAWDATA    <- file.path("C:", "Users", "floris", "SAPEXPORT")
  cEXT        <- "txt" 
  
  dtTABLE <- as.data.table(
    fread(file.path(cRAWDATA, paste(l_Table, cEXT, sep = ".")), 
          sep = ";",  
          colClasses = "character",  stringsAsFactors = FALSE)
  )
  
  # Set Key
  if (!missing(pKey)) {
    setkeyv(dtTABLE, pKey)  
  }
  
  # Replace Illegal field names
  oNAMES <- names(dtTABLE)[grep(pattern = "/", x = names(dtTABLE))]
  nNAMES <- sub(pattern = "/BIC/", "", x = oNAMES)
  setnames(dtTABLE, oNAMES, nNAMES)
  
  # Get Attributes
  setattr(dtTABLE, "RefreshDate",       
    file.mtime(file.path(cRAWDATA, paste(l_Table, cEXT, sep = "."))))
  setattr(dtTABLE, "FileSize",
    file.size(file.path(cRAWDATA, paste(l_Table, cEXT, sep = "."))))
  
  return(dtTABLE)
}

# Function to Read .txt file from specific System 
# ---- ReadingData ----
fGetOHDTable <- function(pTableName, pKey, pAddHeader = FALSE, 
                         pSystID, pClient){
  
  if (!missing(pSystID)) {
    l_Table <- paste0(pSystID, "C", pClient, "_", pTableName )
  } else {
    l_Table <- pTableName
  }
  
  # set Path
  cFTP       <- file.path("C:", "FTP")
  cEXT        <- "txt" 
  
  dtTABLE <- as.data.table(
    fread(file.path(cFTP, paste(l_Table, cEXT, sep = ".")), 
          sep = ";", header = !pAddHeader, 
          colClasses = "character",  stringsAsFactors = FALSE)
  )
  
  # Set Key
  if (!missing(pKey)) {
    setkeyv(dtTABLE, pKey)  
  }
  
  if (pAddHeader) {
    if (missing(pSystID)) {pSystID <- "BP1"}
    if (missing(pClient)) {pClient <- "300"}
    
    l_hdr <- fGetFieldNM(pTableName = pTableName, 
                         pSystID = pSystID, pClient = pClient)
    setnames(dtTABLE, l_hdr)
  }
  
  # Get Attributes
  attr(dtTABLE, "RefreshDate")      <- 
    file.mtime(file.path(cFTP, paste(l_Table, cEXT, sep = ".")))
  attr(dtTABLE, "FileSize")      <- 
    file.size(file.path(cFTP, paste(l_Table, cEXT, sep = ".")))
  
  return(dtTABLE)
}


fGetFieldNM <- function(
  pTableName, pSystID = "BP1", pClient = "300", pTYPE = "ALL" ){
  
  CSAPEXPORT <- file.path("C:", "SAPexport")
  cFTP       <- file.path("C:", "FTP")
  
  dtOHDEST   <- fread(paste(cFTP, "OHDEST.txt", sep = "/"))
  
  dtRSBOHFIELDS <- as.data.table(
    read.table(paste(CSAPEXPORT, 
                     paste0(pSystID, "C", pClient, "_", "RSBOHFIELDS.txt" ), 
                     sep = "/"),
               dec = ",", sep = ";", header = TRUE, 
               colClasses = c("NULL", "character", 
                              rep("character", 2), "numeric", 
                              rep("character", 3), rep("NULL", 11)))  )
  setkey(dtRSBOHFIELDS, "OHDEST", "POSIT")
  
  lOHDEST       <- dtOHDEST[TABLE == pTableName, OHDEST]
  dtRSBOHFIELDS <- dtRSBOHFIELDS[OHDEST == lOHDEST]
  
  vCHA <- c("CHAR", "CUKY", "UNIT", "DATS", "NUMC", "TIMS")
  vKYF <- c("QUAN", "CURR", "DEC")

  lFLDNM <- switch(
    pTYPE,
    ALL = dtRSBOHFIELDS[, FIELDNM],
    CHA = dtRSBOHFIELDS[DATATYPE %in% vCHA, FIELDNM],
    KYF = dtRSBOHFIELDS[DATATYPE %in% vKYF, FIELDNM])
  
  lFLDNM    <- sub("^/BIC/", "", lFLDNM)
  
  return(lFLDNM)
}

fGetKYF <- function(pTableName, pSystID = "BP1", pClient = "300"){
  
  CSAPEXPORT <- file.path("C:", "SAPexport")
  cFTP       <- file.path("C:", "FTP")
  
  dtOHDEST   <- fread(paste(cFTP, "OHDEST.txt", sep = "/"), sep = ";")
  
  dtRSBOHFIELDS <- as.data.table(
    read.table(paste(CSAPEXPORT, 
                     paste0(pSystID, "C", pClient, "_", "RSBOHFIELDS.txt" ), 
                     sep = "/"),
               dec = ",", sep = ";", header = TRUE, 
               colClasses = c("NULL", "character", 
                              rep("character", 2), "numeric", 
                              rep("character", 3), rep("NULL", 11))) )
  setkey(dtRSBOHFIELDS, "OHDEST", "POSIT")
  
  lOHDEST <- dtOHDEST[TABLE == pTableName]$OHDEST
  
  lDAT    <- dtRSBOHFIELDS[OHDEST == lOHDEST & DATATYPE == "CURR", FIELDNM ]
  lHDR    <- sub("^/BIC/", "", lHDR)
  
  return(lHDR)
}

# # setnames(dtMATPLT, old = names(dtMATPLT), new = fGetHeader(pTableName = "MAT_PLANT"))  
# fGetHeader <- function(pTableName){
#   
#   cRAWDATA      <- file.path(".", "data", "raw")
#   
#   dtOHDEST      <- fread(paste(cRAWDATA, "OHDEST.txt", sep = "/"))
#   
#   dtRSBOHFIELDS <- as.data.table(
#     read.table(paste(cRAWDATA, 
#                      "RSBOHFIELDS.txt", sep = "/"),
#                dec = ",", sep = ";", header = TRUE, 
#                colClasses = c("NULL", "character", 
#                               rep("NULL", 2), "numeric", 
#                               "character", rep("NULL", 13)))  )
#   setkey(dtRSBOHFIELDS, "OHDEST", "POSIT")
#   
#   lOHDEST <- dtOHDEST[TABLE == pTableName]$OHDEST
#   
#   lHDR    <- dtRSBOHFIELDS[OHDEST == lOHDEST, ]$TEMPIOBJNM
#   lHDR    <- sub("0", "", lHDR)
#   
#   return(lHDR)
# }

fTableOverview <- function(pTable){
  
  lstUnique    <- lapply(pTable   , FUN = unique)
  vUniqueCount <- sapply(lstUnique, FUN = length)
  vUniqueCount <- sort(vUniqueCount, decreasing = TRUE)
  
  dtRATIO    <- data.table(FLDNM  = names(vUniqueCount),
                           UCOUNT = vUniqueCount,
                           RATIO  = round(nrow(pTable)/vUniqueCount, 0)) 
  
  fHDR   <- function(x){
    l_ret <- paste(sort(x)[1:ifelse(length(x) < 11, length(x), 10)], collapse = "/")
    if (substr(l_ret, 1, 1) == "/") { l_ret <- paste0("#", l_ret)}
    return(l_ret)
  }
  
  
  dtTMP  <- data.table(FLDNM = names(lstUnique),
                       EXAMP = sapply(lstUnique, FUN = fHDR))
  
  dtRATIO <-
    merge(dtRATIO, dtTMP, by = "FLDNM")[order(UCOUNT, decreasing = TRUE)]
  
  return(list(UCOUNT = matrix(vUniqueCount,
                              ncol = 1,
                              dimnames = list(names(vUniqueCount), "Count")), 
              UNIQUE = lstUnique,
              dtRATIO = dtRATIO ))
}


# ---- WriteResults ----
fWriteToSheet <- function(pData, pPath, pFileName, pSheetName, pAppend=FALSE){
  pFileName <- paste0(pPath, "/", pFileName, ".xlsx")
  if (nrow(pData) > 0) {
    write.xlsx(pData, 
               file = pFileName, sheetName = pSheetName, 
               col.names = TRUE, row.names = FALSE, 
               append = pAppend, showNA = TRUE)
  }
}

# --- GetSAPSite
fGetSAPSite <- function(
  pLegacySites, pEANTYP, 
  pBS = "BA1", pBC = "200",
  pES = "RA1", pEC = "250"){
  
  # check if data is available else load it
  if (!exists("dtWERKS")) {
    
    dtPLANT     <- fGetTable(pTable = "/BI0/PPLANT",
                             pKey = "PLANT",
                             pSystID = pBS, pClient = pBC)
    
    dtT001W     <- fGetTable(pTable = "T001W", pKey = c("WERKS"),
                             pSystID = pES, pClient = pEC)
    
    dtADRC      <- fGetTable(pTable = "ADRC", 
                             pSystID = pES, pClient = pEC)
    setnames(dtADRC, "ADDRNUMBER", "ADRNR")
    
    dtWERKS  <- merge(dtT001W[ , .(ADRNR, WERKS, VKORG, VTWEG, VLFKZ)], 
                      dtADRC[  , .(ADRNR, SORT2)],
                      all.x = TRUE, by = "ADRNR")
    
    dtTMP01  <- dtWERKS[ is.na(SORT2)][, SORT2 := NULL]
    dtWERKS  <- dtWERKS[!is.na(SORT2)]
    
    if (pEANTYP == "Z2") {
      dtWERKS  <- dtWERKS[substr(VKORG, 1, 2) %in% c("GB", "IE")]
    } else {
      if (pEANTYP == "Z1") {
        dtWERKS  <- dtWERKS[substr(VKORG, 1, 2) %in% c("NL", "BE")]
      }
    }    
#     setnames(dtWERKS, c("SORT2"), c("LWRKS"))
    
    # Quality check on Duplicates
    dtWERKS  <- dtWERKS[, DUP := duplicated(dtWERKS, 
                                           by = c("SORT2", "VKORG"))]
    
    dtTMP02  <- copy(dtWERKS)
    dtTMP02  <- dtTMP02[, DUP := any(DUP), 
                        by = c("SORT2", "VKORG") ]
    dtTMP02  <- dtTMP02[DUP == TRUE ]
#     fWriteToSheet(dtTMP02, 
#                   pPath, pXLSX, "WERKS_DUP", pAppend = TRUE )
#     fWriteToSheet(dtWERKS[DUP == TRUE], 
#                   pPath, pXLSX, "LWRKS_DEL", pAppend = TRUE )
    
    dtWERKS  <- dtWERKS[DUP == FALSE][, DUP := NULL]
    
    # Information on closed stores
    dtCWRKS  <- fread(file.path(".", "RAW_DATA", "CWRKS.csv"), 
                         sep = ";")
    setkey(dtCWRKS, "WERKS")
    dtCWRKS <- dtCWRKS[EANTYP == pEANTYP]
    
    # Add Sales Org from list which include the closed Stores
    dtCWRKS  <- dtCWRKS[, SORT2 := substr(WERKS, 2,4)]
    setnames(dtCWRKS, c("WERKS"), c("LWRKS"))
    setnames(dtWERKS, 
             c("VKORG"   , "VTWEG"),
             c("SALESORG", "DISTR_CHAN"))

    setnames(dtPLANT,
             c("PLANT", "/BIC/G1LGSTNO"),
             c("WERKS", "LWRKS"))
    dtPLANT <- dtPLANT[substr(WERKS, 1, 1) == "H", 
                       .(WERKS, SALESORG, DISTR_CHAN, LWRKS)]
    dtPLANT[, SORT2 := substr(LWRKS, 2, 4)]

    # Create List of Dummy stores which are not used
    l_DUMMIES_USED <- dtPLANT$WERKS
    l_DUMMIES_FREE <- setdiff(paste0(pLGCINDC, str_pad(1:1000, 3, pad = "0")),
                              l_DUMMIES_USED )

    dtCWRKS <- dtCWRKS[SORT2 %in% setdiff(dtCWRKS$SORT2, dtWERKS$SORT2)]
    dtCWRKS <- dtCWRKS[LWRKS %in% pLegacySites]
    dtCWRKS <- dtCWRKS[, WERKS := l_DUMMIES_FREE[1:nrow(dtCWRKS)]]

    dtWERKS <- dtWERKS[, LWRKS := NA]
    dtWERKS <- rbind(dtWERKS[, .(WERKS, SALESORG, DISTR_CHAN, SORT2, LWRKS)],
                     dtCWRKS[, .(WERKS, SALESORG, DISTR_CHAN, SORT2, LWRKS)],
                     dtPLANT[, .(WERKS, SALESORG, DISTR_CHAN, SORT2, LWRKS)])

 
# dtWERKS   <- merge(dtWERKS, 
#                    dtCWRKS, 
#                    all.y = TRUE, by = "SORT2" )
# dtORG     <- dtWERKS[, .N, by =.(VKORG, SALESORG)]
dtWERKS   <- dtWERKS[, .(WERKS, LWRKS, SALESORG, DISTR_CHAN)]
    
    #     fWriteToSheet(dtTMP01, 
#                   pPath, pXLSX, "WERKS_NO_SORT2", pAppend = TRUE )
    
 
  }

}

fGetTableDAP <- 
  function(pTable, pQAR_OBJECT, 
           pLOGSYSTEMS = c("BD1C100", "BA1C200", "BP1C300")){
 
    # Open COnnection to DataBase
    A2R    <- odbcConnect("ACCESS2R")
    
    for (pLOGSYS in pLOGSYSTEMS) {
      l_Table <- paste0(pLOGSYS, "_", pTable )
      
      dtTABLE <- as.data.table(
        sqlFetch(A2R, l_Table, 
                 stringsAsFactors = FALSE)
      )
      
      if (!exists("dtRETURN")) {
        dtRETURN <- dtTABLE
      } else {
        dtRETURN <- rbind(dtRETURN, dtTABLE)
      }
    }
  
  close(A2R)
  
  dtRETURN[, QAR_OBJ := pQAR_OBJECT]
  
  return(dtRETURN)
}

fGetOverviewDAP <- 
  function(){
    
    dtDAP        <- fGetTable(pTable = "tblALIGN")

    for (i in 1:nrow(dtDAP)) {

      dtTABLE <- fGetTableDAP(dtDAP[i]$TABNAME, dtDAP[i]$Comment)
      dtTABLE  <- dtTABLE[, .(SYSTID, QAR_OBJ)]
      
      if (!exists("dtRETURN")) {
        dtRETURN <- dtTABLE
      } else {
        dtRETURN <- rbind(dtRETURN, dtTABLE)
      }
    }
    
    return(dtRETURN)
  }

# --- GetTRFNMAP

fGetTRFNMAP <- function(pTable) {
  
  # Transformation
  dtRSTRAN  <- fGetEXPTable(pTableName = "RSTRAN", pKey    = "TRANID",
                            pSystID    = "BP1"   , pClient = "300" ) 
  
  # Transformation Fields
  dtRSTRANFIELD  <- fGetEXPTable(pTableName = "RSTRANFIELD", pKey    = "TRANID", 
                                 pSystID    = "BP1"        , pClient = "300" ) 
  
  dtTRFN <- merge(dtRSTRAN, dtRSTRANFIELD, 
                  by = c("SYSTID", "OBJVERS", "TRANID"))
  dtTRFN[, `:=`(RULEPOSIT  = as.integer(sub(",00", "", RULEPOSIT)),
                RULEID     = as.integer(RULEID))]
  setkey(dtTRFN, TRANID, RULEID )
  
  dtTRFN <- dtTRFN[TARGETTYPE      == "IOBJ" & 
                     TARGETSUBTYPE == "ATTR" & 
                     TARGETNAME    == pTable,
                   .(FIELD = paste(FIELDNM, collapse = ";")), 
                   by = .(TRANID, RULEID, FIELDTYPE)]
  
  dtMAPPING <- merge(dtTRFN[FIELDTYPE == "F", 
                            c("TRANID", "RULEID", "FIELD" ), with = FALSE], 
                     dtTRFN[FIELDTYPE == "I", 
                            c("TRANID", "RULEID", "FIELD" ), with = FALSE], 
                     by = c("TRANID", "RULEID"), all = TRUE)
  setnames(dtMAPPING, c("FIELD.x", "FIELD.y"), c("FROM", "ATTRINM"))
  
  return(dtMAPPING) 
}

fGetDD <- function(pTable){
  # DD for the following tables DRAW, EINA, EINE, MAKT, MALG, MAPR, MARA, 
  # MARC, MARM, MAW1, MBEW, MEAN, MLGN, MVKE, PROP, WLK2, WRPL

  # DD03M delivers the field text and more
  dtDD03M <- as.data.table(
    read_excel("./10_RawData/DD03M.XLSX", sheet = "Sheet1", 
               col_names = TRUE, col_types = NULL, na = "",
               skip = 0))
  dtDD03M <- dtDD03M[, POSITION := as.integer(POSITION)]
  
  # Texttable and CheckTable
  dtDD30V <- as.data.table(
    read_excel("./10_RawData/DD30V.XLSX", sheet = "Sheet1", 
               col_names = TRUE, col_types = rep("text", 20), na = "",
               skip = 0))
  dtDD30V <- unique(dtDD30V[, .(SELMETHOD, TEXTTAB)])
  dtDD30V[, `:=`(CHECKTABLE = SELMETHOD)]
  
  dtDD30V <- dtDD30V[,
                     .(TXTTAB = paste(TEXTTAB, collapse = ";")), 
                     by = .(CHECKTABLE)]
  
  dtDD30V <- dtDD30V[!TXTTAB == "NA"]
  
  dtDD03M <- merge(dtDD03M, dtDD30V, by = "CHECKTABLE", all.x = TRUE)
  dtDD03M <- dtDD03M[, .(TABNAME, FIELDNAME, CHECKTABLE, TXTTAB, POSITION, 
                         DATATYPE, LENG, INTLEN, INTTYPE, DECIMALS, 
                         DDTEXT, REPTEXT, SCRTEXT_S, SCRTEXT_M, 
                         SCRTEXT_L, LOWERCASE)]
  setkey(dtDD03M, TABNAME, POSITION)
  
  if (!missing(pTable)) {
    dtDD03M <- dtDD03M[TABNAME == pTable]
  }
  return(dtDD03M)
}

fGetAP245 <- function(pTable){
  
  dtAP245 <- as.data.table(
    read_excel("./10_RawData/AP245_ART.XLSX", sheet = "Sheet1", 
               col_names = TRUE, col_types = rep("text", 52), na = "",
               skip = 0))
  
  setnames(dtAP245, 
           c(2, 3, 4, 49, 50, 51, 52), 
           c("DDTEXT", "TABNAME", "FIELDNAME", "BI", "TXT", "TYPE", "CMT"))
  return(dtAP245[BI == "Y", 
                 .(TABNAME, FIELDNAME, DDTEXT, BI, TXT, TYPE, CMT)])
}

fGetZipTable <- function(pFullNameZip, pTable){
  dtTable <- as.data.table(
    read.table(unz(pFullNameZip, pTable), 
               nrows = -1, header = T, quote = "\"", sep = ";")
  )
  
  return(dtTable)
}


fValidCheckDigit <- function(pEAN){

  fIsValidEAN <- function(x){
    
    x <- lapply(x, as.integer)
    iTMP <- with(x, 
                 3*(C1 + C3 + C5 + C7 + C9  + C11 + C13) + 
                   (C2 + C4 + C6 + C8 + C10 + C12)
    ) 
    
    iTMP   <- 10 * ((iTMP %/% 10) + 1) - iTMP
    fValid <- iTMP %% 10 == x$C14
    
    return(fValid)
  }
  
  # Make length 14 by adding leading zero's
  pEAN  <- str_pad(string = pEAN, width = 14, side = "left", pad = "0")
  dtTMP <- data.table(EAN = pEAN)
  dtTMP[, c(paste0("C", 1:14)) := (tstrsplit(EAN, "", fixed = TRUE))]
  dtTMP[, V := fIsValidEAN(.SD), .SDcols = paste0("C", 1:14)]

  return(dtTMP$V)
}

#' Check Mean
#'
#' @param pEAN 
#'
#' @return data.table with EAN and RC codes
#' RC 0 - Valid EAN
#' RC 5 - Non-numeric EAN
#' RC 6 - Too short for EAN, length < 8
#' RC 7 - Too long for EAN , length > 14
#' RC 8 - Check Digit not valid 
#' @export
#'
#' @examples
fChkEAN <- function(pEAN){
  
  dtRC <-
    data.table(
      RC       = 0L:8L,
      ERR_DESC = c(
        "Valid EAN",
        "Not Used",
        "Not Used",
        "Not Used",
        "Not Used",
        "EAN Contains Characters",
        "Too short for EAN, length <  8",
        "Too long for EAN , length > 14",
        "Check Digit not valid"
      ))
  
  if (!class(pEAN) == "character") {
    return(pEAN)
  }
  
  # Create data.table with all EAN with RC = 0 ( being Valid)
  dtEAN <- data.table(EAN = numeric(0), EANT = numeric(0),
                      RC = numeric(0) , LENGTH = numeric(0))
  dtTMP <- data.table(EAN = pEAN, EANT = pEAN,
                      RC = 0, LENGTH = nchar(pEAN))
  
  # Check if EAN is Numeric, non-numeric = RC 5
  dtTMP[grepl(pattern = "[^[0-9]]*", x = EAN), 
        `:=`(RC = 5, LENGTH = nchar(EAN))]
  dtEAN <- rbind(dtEAN, dtTMP[RC != 0])
  dtTMP <- dtTMP[RC == 0]
  
  if (nrow(dtTMP) > 0) {
    # Check Length of trimmed EAN, without leading/trailing spaces, leading 0
    dtTMP[, EANT := gsub(pattern = "^0*", 
                        replacement = "", 
                        x = str_trim(string = EAN, side = "both"))]
    dtTMP[, LENGTH := nchar(EANT)]
    # Too Short, RC 6
    dtTMP[LENGTH < 8,  RC := 6] 
    dtEAN <- rbind(dtEAN, dtTMP[RC != 0])
    dtTMP <- dtTMP[RC == 0]
  }
  
  if (nrow(dtTMP) > 0) {  
    # Too Long, RC 7
    dtTMP[LENGTH > 14, RC := 7] 
    dtEAN <- rbind(dtEAN, dtTMP[RC != 0])
    dtTMP <- dtTMP[RC == 0]
  }
  
  if (nrow(dtTMP) > 0) {
    # Check the check Digit, invalid = RC 8
    dtTMP[!fValidCheckDigit(EANT), 
          `:=`(RC = 8, LENGTH = nchar(EANT))] 
    dtEAN <- rbind(dtEAN, dtTMP)
  }
  
  dtEAN <- dtRC[dtEAN, on = .(RC)]
  
  return(dtEAN)
}

fRemoveFieldNamePart <- function(pTable, pPart = "^/BIC/"){
  
  setnames(pTable, names(pTable), sub(pattern = pPart, "", x = names(pTable)))

  return(pTable)
}

fGetDS <- function(pSystID = "BP1", pClient = "300"){
  
  # DataSource Meta data
  dtRSDSSEGFD  <- fGetEXPTable(pTable  = "RSDSSEGFD", 
                               pKey    = c("DATASOURCE", "POSIT"),
                               pSystID = pSystID, pClient = pClient)
  
  dtRSDSSEGFDT <- fGetEXPTable(pTable  = "RSDSSEGFDT", 
                               pKey    = c("DATASOURCE", "POSIT"), 
                               pSystID = pSystID, pClient = pClient)
  dtRSDSSEGFDT <- dtRSDSSEGFDT[LANGU == "E", .(DATASOURCE, SEGID, POSIT, TXTLG)]
  
  # Get Datasource fields in the right order
  dtDS   <- dtRSDSSEGFDT[dtRSDSSEGFD, on = c("DATASOURCE", "SEGID", "POSIT") ]
  dtDS   <- dtDS[, .(SYSTID , DATASOURCE, POSIT, 
                     FIELDNM, DATATYPE  , DECIMALS, IOBJNM, TXTLG)]
  rm(dtRSDSSEGFD, dtRSDSSEGFDT)
  
  # Replace Illegal field names and fix the order
  dtDS     <- dtDS[, FIELDNM := sub(pattern = "/BIC/", "", x = FIELDNM) ]
  
  # Order by Datasource and Position
  dtDS         <- dtDS[, POSIT := as.numeric(
    sub(pattern = ",", replacement = ".", x = POSIT, fixed = TRUE))] 
  setkey(dtDS, "DATASOURCE", "POSIT")
  

  return(dtDS)
}

fAlignDS <- function(pDT, pDS, pSystID = "BP1", pClient = "300"){
  
  dtDATA <- pDT
  
  # Get All DS fields
  dtDS  <- fGetDS(pSystID, pClient)
  vDSFD <- dtDS[DATASOURCE == pDS, FIELDNM]
  
  # Add Missing Fields, make those NA
  vAFD  <- setdiff(vDSFD, names(dtDATA)) 
  if (length(vAFD > 0)) {dtDATA[, (vAFD) := NA]}
  
  #Restrict fields to DS fields in the right order
  dtDATA <- dtDATA[, vDSFD, with = FALSE] #

  return(dtDATA)
  
}

fOpen_in_Excel <- 
  function(pDT, pPath = "./Results", pFN){
    
    if (!dir.exists(pPath)) {
      dir.create(pPath)
    }
    
    pFFN <- file.path(pPath, pFN)
    write.table(
      x = pDT,
      file = pFFN, 
      sep = ";", row.names = F)
    shell.exec(normalizePath(pFFN))    
  }