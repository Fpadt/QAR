# Floris Padt
# 2018 Jan 28

#### Library ####
# due to incompatibility of RSAP with reshape2
if ("reshape2" %in% loadedNamespaces()) {
  detach("package:reshape2", unload = TRUE)}

library(RSAP      , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(reshape   , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(magrittr  , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)

# ---- ReadingData ----
fReadSAPTable <- 
  function(pSystID, pClient, pTable, pOptions, pFields, pAnalysis){

  lcon <- fGetSAPConnection(pSystId = pSystID, pClient = pClient)
  
  # Get Fields from Library
  if (missing(pFields)) {
    pFields <- 
      fGetFields(
        pSYSTID   = pSystID,
        pCLIENT   = pClient,
        pTable    = pTable,
        pAnalysis = pAnalysis)}
  
  dtTABLE <- 
    RSAPReadTable(
      con      = lcon, 
      saptable = pTable, 
      options  = pOptions, 
      fields   = pFields)                           %>%
    as.data.table()                                 %>%
    .[ , `:=`(SYSTID = pSystID, CLIENT = pClient )] %T>%
    setcolorder(c("SYSTID", "CLIENT", 
                  setdiff(names(.), 
                          c("SYSTID", "CLIENT"))))
  
  return(dtTABLE)
}

fGetSAPConnection <- 
  function(pSystId, pClient){

    lID <- paste(pSystId, pClient, sep = "C")
    
    # Return exisitng connection if available
    if (exists("lstCON")) {
      lcon <- lstCON[[lID]]
      if (length(lcon) > 0) {
        return(lcon)}
    } else {
      lstCON <<- list()        
    }
      
    # temporary Table with New SAP Connection    
    lnew <- fCreateSAPConnection(pSystId, pClient)
    
    lstCON[[lID]] <<- lnew 

    return(fGetSAPConnection(pSystId, pClient))

}

fCreateSAPConnection <- 
  function(pSystId, pClient){
    
    # Load connection parameters
    if (!exists(x = "dtSAP_LOGON")) {
      load(file = file.path(DAT, "SAP_LOGON.RData"))
    }
    
    lcon <- 
      RSAPConnect(
        dtSAP_LOGON[
          systid == pSystId & client == pClient, 
          .(ashost, sysnr, client, user, passwd, lang, trace, lcheck)]  )
    
    return(lcon)
  }

fCloseAllSAPConnections <- function() {
  lapply(lstCON, FUN = RSAPClose)}

fGetChain <- 
  function(pSYSTID, pCLIENT, pPROGNAME, pVARIANT){
  
  parms <- list('REPORT' = pPROGNAME, 'VARIANT' = pVARIANT)
  lcon  <- fGetSAPConnection(pSystId = pSYSTID, pClient = pCLIENT)
  
  lCHAIN <- 
    RSAPInvoke(lcon, "RS_VARIANT_CONTENTS_RFC", parms) %>%
    .$VALUTAB                                          %>%
    as.data.table()                                    %>%
    .[str_trim(SELNAME) == "CHAIN", str_trim(LOW)]  
  
  return(lCHAIN)
  
}

fGetFields <- 
  function(pSYSTID = "BP1", pCLIENT = "300", pTable, pAnalysis){
    
    if (missing(pAnalysis)) {
      
      lFLDS <- 
        fReadDataDictionary(
          pSystID = pSYSTID,
          pClient = pCLIENT,
          pTable  = pTable)$FIELDNAME
      
    } else {
      
      fLoad_dtDD_SLCT()
      
      lFLDS <- 
        dtDD_SLCT[
          SID      == pSYSTID &
          CLNT     == pCLIENT &
          TABLE    == pTable  &
          ANALYSIS == pAnalysis, FIELD ]
    }
    
    return(as.list(lFLDS))
  }

fGetOptions <- 
  function(pSYSTID = "BP1", pCLIENT = "300", pTable){
  
  fLoad_dtDD_SLCT()
  
  return(as.list(
    dtDD_SLCT[
      SID   == pSYSTID &
      CLNT  == pCLIENT &
      TABLE == pTable, FILTER ]))
  }

fLoad_dtDD_SLCT <- 
  function(){
  # Load connection parameters
  if (!exists(x = "dtDD_SLCT")) {
    dtDD_SLCT <<- read_excel(
      file.path(PRJ, "10_RawData", "qryDD_SLCT.xlsx"), 
      col_types = c("numeric", "skip", "skip"   , "skip",
                    "text"   , "text", "numeric", "text", 
                    "skip"   , "skip", "skip"   , "skip", 
                    "skip"   , "skip", "text"   , "text", 
                    "skip"   , "skip", "text"   , "skip")) %>%
      as.data.table()  }  
  }

fRead_and_Union <- 
  function(pSID.lng, pTable, pOptions, pFields){
  
  dtReturn <- 
    do.call(
      rbind,
      lapply(as.list(pSID.lng), function(x) {
        fReadSAPTable(
          pSystID  = substr(x, 1, 3),
          pClient  = substr(x, 4, 6),
          pTable   = pTable,
          pOptions = pOptions,
          pFields  = pFields )
      }))
  
  return(dtReturn)
  }

fReadDataDictionary <- 
  function(pSystID, pClient, pTable){
    
    pOptions <- list(paste0("TABNAME = '", pTable, "'"))
    
    pFields  <- list(
      'TABNAME', 'FIELDNAME' , 'AS4LOCAL'  , 'AS4VERS'   , 'POSITION',
      'KEYFLAG', 'MANDATORY' , 'ROLLNAME'  , 'CHECKTABLE', 'ADMINFIELD',
      'INTTYPE', 'INTLEN'    , 'REFTABLE'  , 'PRECFIELD' , 'REFFIELD',
      'CONROUT', 'NOTNULL'   , 'DATATYPE'  , 'LENG'      , 'DECIMALS',
      'DOMNAME', 'SHLPORIGIN', 'TABLETYPE' , 'DEPTH'     , 'COMPTYPE',
      'REFTYPE', 'LANGUFLAG' , 'DBPOSITION', 'ANONYMOUS' , 'OUTPUTSTYLE')

    # TableField Information
    dtDD03L <- 
      fReadSAPTable(
        pSystID  = pSystID,
        pClient  = pClient,
        pTable   = "DD03L",
        pOptions = c(pOptions, "AND", list("COMPTYPE = 'E'")),
        pFields  = pFields)
    
    # Description of the Table
    dtDD02T <- 
      fReadSAPTable(
        pSystID  = pSystID,
        pClient  = pClient,
        pTable   = "DD02T",
        pOptions = c(pOptions, "AND", list("DDLANGUAGE = 'E'")),
        pFields  = list('TABNAME', 'DDTEXT'))

    # Description of the Table Fields (if exists)        
    dtDD03T <- 
      fReadSAPTable(
        pSystID  = pSystID,
        pClient  = pClient,
        pTable   = "DD03T",
        pOptions = c(pOptions, "AND", list("DDLANGUAGE = 'E'")),
        pFields  = list('TABNAME', 'FIELDNAME', 'DDTEXT'))
    
    dtRET <- 
      dtDD03T[dtDD03L, 
              on = .(SYSTID, CLIENT, TABNAME, FIELDNAME)]  %T>%
      setnames("DDTEXT", "FLDDESC")                        %>%
      dtDD02T[., 
              on = .(SYSTID, CLIENT, TABNAME)]             %T>%
      setnames("DDTEXT", "TABDESC")                        %T>%
      setorder("POSITION")

    return(dtRET)
}