#' Center scale from PLSISE
#'
#' @description
#'  Center and scale a numeric data.frame
#'
#' @param x a dataframe of numeric to center and scale
#'
#' @returns mean : means of columns
#'  sd : standard deviations of columns
#'  Xk : Centered and scaled X dataframe 
#'
#' @export
#'
center_scale <- function(x){
  means <- sapply(x, mean)
  sds <- sapply(x, sd)
  sds <- ifelse(sds==0, 1, sds)
  x.cs <- (x - means[col(x)]) / sds[col(x)]
  return(list("mean"=means, "sd"=sds, "Xk"=x.cs))
}
