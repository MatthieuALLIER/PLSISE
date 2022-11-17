get_dummies <- function(x){
  if(!is.factor(x)){
    return("error")
  }
  n <- length(x)
  q <- nlevels(x)
  names <- levels(x)
  dummies <- matrix(0, n, q)
  colnames(dummies) <- names
  for(modality in 1:q){
    dummies[,modality] <- ifelse(x==names[modality], 1, 0)
  }
  return(data.frame(dummies))
}
