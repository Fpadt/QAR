source('../QAR/00_RProj/00_Global/iSynGeneral.R')

dtMAT_SALES   <- fread("c:/ftp/MAT-SALES.txt", sep=";", 
                       colClasses = "character")
dtRSBOHFIELDS <- fGetSAPTable(pTable = "RSBOHFIELDS", 
                              pSystID = "BP1",
                              pClient = 300)  
View(dtRSBOHFIELDS[OHDEST == "G1OHDMSL",])

dtMAT_SALES_FLDS <- dtRSBOHFIELDS[OHDEST == "G1OHDMSL", ]
dtMAT_SALES_FLDS$POSIT <- as.numeric(gsub(pattern = ",00", 
                                          replacement = "",
                                          x = dtMAT_SALES_FLDS$POSIT))
dtMAT_SALES_FLDS   <- dtMAT_SALES_FLDS[order(POSIT)]
names(dtMAT_SALES) <- dtMAT_SALES_FLDS$TEMPIOBJNM

lstTMP   <- lapply(dtMAT_SALES, FUN=unique) 
vMAT_SLS <- sapply(lstTMP, FUN=length)
sort(vMAT_SLS)

####


lstTMP   <- lapply(dtMATERIAL, FUN=unique) 
vMAT <- sapply(lstTMP, FUN=length)
sort(vMAT)

dtMAT_PLANT   <- fread("c:/ftp/MAT_PLANT.txt", sep=";", 
                      colClasses = "character")

dtMAT_PLANT_FLDS <- dtRSBOHFIELDS[OHDEST == "G1OHDMPL", ]
dtMAT_PLANT_FLDS$POSIT <- as.numeric(gsub(pattern = ",00", 
                                         replacement = "",
                                         x = dtMAT_PLANT_FLDS$POSIT))
dtMAT_PLANT_FLDS   <- dtMAT_PLANT_FLDS[order(POSIT)]
names(dtMAT_PLANT) <- dtMAT_PLANT_FLDS$TEMPIOBJNM

lstTMP   <- lapply(dtMAT_PLANT, FUN=unique) 
vMAT <- sapply(lstTMP, FUN=length)
sort(vMAT)


# G1OHDMAT
# G1OHDMPL
# G1OHDMSL
# G1OHDPLT