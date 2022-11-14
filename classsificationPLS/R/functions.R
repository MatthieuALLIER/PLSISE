center_scale <- function(x){
  means <- sapply(x, mean)
  sds <- sapply(x, sd)
  sds <- ifelse(sds==0, 1, sds)
  x.cs <- (x - means[col(x)]) / sds[col(x)]
  return(list("mean"=means, "sd"=sds, "Xk"=x.cs))
}

get_dummies <- function(x){
  x <- as.factor(x)
  x.dummies <- dummy_cols(x)[,2:(length(levels(x))+1)]
  colnames(x.dummies) <- levels(x)
  return(x.dummies)
}

