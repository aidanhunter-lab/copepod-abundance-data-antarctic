
prompt.user.input <- function(prompt, preprint = NULL){
  if(!is.null(preprint)) print(preprint, row.names = FALSE)
  readline(prompt)
}
