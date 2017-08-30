dtRSTRANFIELD      <- 
  fGetEXPTable(pTableName = "RSTRANFIELD", 
               pSystID = "BP1", pClient = 300)

dtRSTRANFIELD <- 
  dtRSTRANFIELD[TRANID == "0KS3KW5HXWTXUHMMEENXK9R9SDZCJFMH",
                .(RULEID, FIELDNM, FIELDTYPE)]

dtTRANS <- 
  dtRSTRANFIELD[FIELDTYPE == "I"][
    dtRSTRANFIELD[FIELDTYPE == "F"],
    on = .(RULEID)
  ]