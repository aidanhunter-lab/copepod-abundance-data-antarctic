vgrepl <- function(pattern, x, ignore.case = FALSE, SIMPLIFY = TRUE){
  f <- Vectorize(grepl, vectorize.args = 'pattern', SIMPLIFY = SIMPLIFY)
  f(pattern, x, ignore.case = ignore.case)}