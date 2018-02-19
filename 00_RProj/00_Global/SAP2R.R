# file SAP2R.R
# copyright (C) 2012 Piers Harding & Floris Padt
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
# Floris Padt
# 2018 Jan 28

# RSDPL_CUBE_DATA_READ
# RSDPL_CHA_MASTER_DATA_READ	DM API: Reading master data of a characteristic
# RSDPL_CHA_TEXT_DATA_READ	DM API: Reading text data of a characteristic
# RSDSO_WRITE_API_RFC
# RS_VARIANT_DELETE_RFC
# RS_CREATE_VARIANT_RFC
# PRGN_ACTIVITY_GROUP_DELETE
# ME_USER_CHANGE_PASSWORD
# EM_GET_NUMBER_OF_ENTRIES
# BAPI_USER_UNLOCK
# RS_VARIANT_CONTENTS_RFC
# RSSEM_CHA_VALUES_GET
# BAPI_CUBE_GETDETAIL
# BAPI_CUBE_GETLIST
# RRW3_GET_QUERY_VIEW_DATA
# RSPC_API_CHAIN_SCHEDULE


#### Library ####
# due to incompatibility of RSAP with reshape2
# if ("reshape" %in% loadedNamespaces()) {
#   detach("package:reshape", unload = TRUE)}

library(RSAP      , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(reshape2  , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(data.table, verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)
library(magrittr  , verbose = FALSE, quietly = TRUE, warn.conflicts = FALSE)

# ---- My own RSAP function slightly modified
SAP2R_ReadTable <- 
  function(con, saptable, options=list(), fields=list()){
    
    if (!RSAPValidHandle(con))
      stop("argument is not a valid RSAP con")
    
    parms <- 
      list(
        'DELIMITER'   = ';',
        'QUERY_TABLE' = saptable,
        'OPTIONS'     = list('TEXT'      = options),
        'FIELDS'      = list('FIELDNAME' = fields))
    
    res  <- RSAPInvoke(con, "RFC_READ_TABLE", parms)
    flds <- sub("\\s+$", "", res$FIELDS$FIELDNAME)
    data <- NULL
    
    if (length(res$DATA$WA) == 0) {
      data <- data.table()
    } else {
      data <- data.table(
        res$DATA, 
        reshape2:::colsplit(res$DATA$WA,  pattern = ";", names = flds))
    }
    
    # Change Data Class
    for (i in 1:length(flds)) {
      f   <- flds[i]
      typ <- res$FIELDS$TYPE[i]
      
      if (typ == 'N' || typ == 'I' || typ == 'P') {
        data[[f]] <- 
          sub("\\s+$", "", data[[f]]) 
        # %>%
        #   sub("^\\s+", "", data[[f]]) %>%
        #   paste0(
        #     ifelse(grepl(pattern = "-$", x = data[[f]]), 
        #            "-", ""), sub(pattern = "-$", "", data[[f]])) %>%
        #   as.numeric()
        #   # sub("[^\\d\\.\\-\\,]", "", data[[f]], perl = TRUE) %>%
        #   # as.numeric()                                       
        # 
        # # as.numeric(unlist(lapply(data[[f]], 
        # # FUN = function(x) {sub("[^\\d\\.\\-\\,]", "", x, perl = TRUE)})));
        # data[[f]][is.na(data[[f]])] <- 0 
        
      } else {
        data[[f]] <- sub("\\s+$", "", data[[f]]);
      }
    }
    data$WA <- NULL
    return(data)
  }

# ---- ReadingData ----
fReadSAPTable <- 
  function(pSystID        , pClient, pTable, 
           pOptions=list(), pFields=list(), 
           pAnalysis){

  lcon <- fGetSAPConnection(pSystId = pSystID, pClient = pClient)
  
  # Get Fields from Library
  if (missing(pFields) | length(pFields) == 0) {
    pFields <- 
      fGetFields(
        pSYSTID   = pSystID,
        pCLIENT   = pClient,
        pTable    = pTable,
        pAnalysis = pAnalysis)}
  
  dtTABLE <- 
    SAP2R_ReadTable(
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
      if (length(lcon) > 0 & 
          !is.null(RSAPValidHandle(lcon))) {
        if (RSAPValidHandle(lcon)) {
          return(lcon)}}
    } else {
      lstCON <<- list()        
    }
      
    # temporary Table with New SAP Connection    
    lnew <- fCreateSAPConnection(pSystId, pClient)
    
    lstCON[[lID]] <<- lnew 

    return(fGetSAPConnection(pSystId, pClient))

}

fGetSAPLogon <- 
  function() {
    
    # Load connection parameters
    if (!exists(x = "dtSAP_LOGON")) {
      load(file = file.path(DAT, "SAP_LOGON.RData"))
    }    
    
    dtSAP_LOGON <<- dtSAP_LOGON
    
    return(dtSAP_LOGON)
  }

fCreateSAPConnection <- 
  function(pSystId, pClient){
    
    fGetSAPLogon()
    
    lcon <- 
      RSAPConnect(
        dtSAP_LOGON[
          systid == pSystId & client == pClient, 
          .(ashost, sysnr, client, user, passwd, lang, trace, lcheck)]  )
    
    return(lcon)
  }

fCloseAllSAPConnections <- function() {
  lapply(lstCON, FUN = RSAPClose)
  
  rm(list = c("lstCON", "dtSAP_LOGON"))}

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
    
    return(dtDD_SLCT)
  }

fRead_and_Union <- 
  function(pSIDCLNT, pTable, pOptions, pFields, pEnv, pType){
    
    if (missing(pSIDCLNT)) {   
      pSID.lng <- fGetSID(pEnv, pType)
    } else {
      pSID.lng <- pSIDCLNT
    }
    
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


fShowAnalysis4TableField <- 
  function(pSYSTID, pCLIENT, pTABLE, pFIELD) {
    
    ret <- 
      fLoad_dtDD_SLCT()[
        SID   == pSYSTID &
        CLNT  == pCLIENT &
        TABLE == pTABLE  &  
        FIELD == pFIELD, ]

    return(ret)
  }

fShowTableFields4Analysis <- 
  function(pSYSTID, pCLIENT, pTABLE, pANALYSIS) {
    
    ret <- 
      fLoad_dtDD_SLCT()[
        SID      == pSYSTID &
        CLNT     == pCLIENT &
        TABLE    == pTABLE  &    
        ANALYSIS == pANALYSIS, ]
    
    return(ret)
  }

fShowAnalysis <- 
  function(pSYSTID, pCLIENT, pANALYSIS) {
    
    ret <- 
      fLoad_dtDD_SLCT()[
        SID      == pSYSTID &
          CLNT     == pCLIENT &
          ANALYSIS == pANALYSIS, ]
    
    return(ret)
  }

fGetSID <- 
  function(pEnv, pType) {
    
    dtT <- fGetSAPLogon()
    
    if (!missing(pType)) {
      dtT <- dtT[type == pType] } 
    
    if (!missing(pEnv)) {
      dtT <- dtT[env == pEnv] } 
    
    return(
      paste0(
        dtT[, systid],
        dtT[, client]))
  }

fChar2Num <- function(x){
  y <- 
    paste0(
      ifelse(
        grepl(pattern     = "-", 
              x           = x, 
              perl        = TRUE),
        "-", ""), 
      gsub(pattern     = "[\\s]|[-]", 
           replacement = "", 
           x           = x, 
           perl        = TRUE))
  
  return(as.numeric(y))
  
}
