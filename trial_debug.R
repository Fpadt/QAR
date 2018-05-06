fGetMatch <- function(x, y){
  # x <- dtT2[, eval(x), with = FALSE]
  ret <- intersect(x, y) %>%
    length()
  
  ret <- ret/length(unique(x))
  return(100*ret)} 