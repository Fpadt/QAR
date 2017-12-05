library(data.table)

tmp     <- file.path("c:", "tmp", "F3")
myfiles <- list.files(path = tmp)
resfile <- file.path(tmp, 
                     paste0(
                       format(Sys.Date(), "%Y%m%d"), "-",
                       substr(myfiles[12], 1,5),"_ALL2.csv"))

mylist <- list()
for (myf in myfiles ) {
  mylist[[myf]] <- fread(input = file.path(tmp, myf), colClasses = "character")
}

mytab <- rbindlist(mylist)
write.table(x = mytab, file = resfile,
            sep = ";", dec = ".", quote = FALSE,
            row.names = FALSE, col.names = TRUE)

