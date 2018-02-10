# BU1 error
dtTXT <- 
  fReadSAPTable(
    pSystID   = lSystID, 
    pClient   = "300", 
    pTable    = "/BIC/TG1RIM", 
    pOptions  = list("LANGU = 'E'"))


pFields <- fGetFields(
  pSYSTID   = "BP1",
  pCLIENT   = "300",
  pTable    = saptable)

con <- fGetSAPConnection(pSystId = "BP1", pClient = "300")

dtTABLE <- 
  RSAPReadTable2(
    con      = con, 
    saptable = saptable, 
    options  = list(), 
    fields   = pFields)  

saptable <- "/BIC/TG1RIM" 
options   <- list()
fields    <- pFields 

RSAPReadTable2 <- function(con, saptable, options=list(), fields=list())
{
  if(!RSAPValidHandle(con))
    stop("argument is not a valid RSAP con")
  library(reshape2)
  parms <- list('DELIMITER' = ';',
                'QUERY_TABLE' = saptable,
                'OPTIONS' = list('TEXT' = options),
                'FIELDS' = list('FIELDNAME' = fields)
  )
  res <- RSAPInvoke(con, "RFC_READ_TABLE", parms)
  flds <- sub("\\s+$", "", res$FIELDS$FIELDNAME)
  data <- NULL
  if (length(res$DATA$WA) == 0) {
    data <- data.frame()
  }
  else {
    data <- data.frame(res$DATA, colsplit(res$DATA$WA, pattern = ";", names = flds))
  }
  
  for (i in 1:length(flds)) {
    f <- flds[i]
    typ <- res$FIELDS$TYPE[i]
    if (typ == 'N' || typ == 'I' || typ == 'P') {
      data[[f]] <- as.numeric(unlist(lapply(data[[f]], FUN=function (x) {sub("[^\\d\\.\\-\\,]", "", x, perl=TRUE)})));
      data[[f]][is.na(data[[f]])] <- 0 
    } else {
      data[[f]] <- sub("\\s+$", "", data[[f]]);
    }
  }
  data$WA <- NULL
  return(data)
}
