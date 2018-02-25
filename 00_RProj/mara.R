
dtMARA1 <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAD1.txt", sep = "\t", 
                 skip="MATNR",
                 colClasses = "character",  encoding="UTF-8", header = TRUE)
dtMARA2 <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAD2.txt", sep = "\t",
                 colClasses = "character")
dtMARA3 <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAD3.txt", sep = "\t",
                 colClasses = "character")
dtMARA4 <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAD4.txt", sep = "\t",
                 colClasses = "character", header = TRUE)
dtMARA5 <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAD5.txt", sep = "\t",
                 colClasses = "character", header = TRUE)
dtMARA1[, `:=`(V1 = NULL, MANDT = NULL)]
dtMARA2[, `:=`(V1 = NULL, MANDT = NULL)]
dtMARA3[, `:=`(V1 = NULL, MANDT = NULL)]
dtMARA4[, `:=`(V1 = NULL, MANDT = NULL)]
dtMARA5[, `:=`(V1 = NULL, MANDT = NULL, MAKTX = NULL)]






dtMARAQ <- fread("C:/Users/fpadt/Documents/SAP/SAP GUI/MARAQ.txt", sep = ";",
                 colClasses = "character")
