library(data.table)
library(magrittr)

ff <- fread("c:/tmp/FF2.csv", colClasses = "character")

# ff <- 
#   ff[RBUKRS == "GB13"]     %>%
#   .[SAKNR   == "244691"]   %>%
#   .[FISCPER == "2014003"]  %>%
#   .[WERKS   == "1001"]

write.table(x = ff, 
            file = "c:/tmp/20170920_FI-GL_History.csv",
            sep = ";",quote = FALSE, 
            col.names = TRUE, row.names = FALSE)