outFile <- file("../output.Rmd", "w") 

for (i in list.files()){ 
  x <- readLines(i) 
  writeLines(c(x, ""), outFile) 
} 
close(outFile) 

