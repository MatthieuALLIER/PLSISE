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
