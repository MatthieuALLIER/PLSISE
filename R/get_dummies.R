#' Get dummies from PLSISE
#'
#' @description
#' Create dummies of a factor variable
#'
#' @param x a factor containing values to make dummies of
#'
#' @returns a dataframe containing the dummies
#'
#' @export
#'
get_dummies <- function(x){
  if(!is.factor(x)){stop("Error : x is not factor")}
  dummies <- sapply(levels(x), function(modality) ifelse(x==modality, 1, 0))
  return(data.frame(dummies))
}
