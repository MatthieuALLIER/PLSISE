#' Fitting PLS-DA model
#'
#' @description
#' `predict()` Predicted values based on PLSDA object.
#'
#' @param PLSDA an object of class PLSDA : a fitted with PLS-DA
#'   method (fit) model.
#'
#' @param newdata A data frame in which to look for variables with
#'   which to predict.
#'
#' @param type type of prediction : "class" for predicted class,
#'   "posterior" for probabilities or "value" for values of predictions#'
#'
#' @returns Y predictions for newdata as required in type parameter
#'
#' @export
#'
predict <- function(PLSDA, newdata, type = "class"){
  X <- t(newdata) - colMeans(PLSDA$X)
  X <- X / sapply(PLSDA$X, sd)
  Y <- t(X) %*% PLSDA$coef
  Y <- Y + PLSDA$intercept
  
  Ysoftmax <- t(apply(Y, 1, function(x) exp(x) / sum(exp(x))))
  
  Yclass <- as.factor(PLSDA$yname[apply(Ysoftmax, 1, which.max)])
  levels(Yclass) <- colnames(PLSDA$y)
  
  if(type=="value"){return(Y)}
  if(type=="posterior"){return(Ysoftmax)}
  if(type=="class"){return(Yclass)}
}
