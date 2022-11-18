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
#' @param type type of prediction : "class" for predicted class or
#'   "posterior" for probabilities
#'
#'
#' @returns
#'
#'
predict <- function(PLSDA, newdata, type = "class"){
  X <- t(newdata) - colMeans(PLSDA$X)
  X <- X / sapply(PLSDA$X, sd)
  Y <- t(X) %*% PLSDA$coef
  Y <- Y + PLSDA$intercept
  
  Ysoftmax <- t(apply(Y, 1, function(x) exp(x) / sum(exp(x))))
  
  if(type=="posterior"){
    return(Ysoftmax)
  }else if(type=="class"){
    Yclass <- PLSDA$yname[apply(Ysoftmax, 1, which.max)]
    return(as.factor(Yclass))
  }
}
