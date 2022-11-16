#' Fitting PLS-DA model
#'
#' @description
#' `fit()` Fit the PLS-DA Classification to data, it can be used to carry
#'  classification on a target variable with 2 or more modalities
#'
#' @param formula an object of class "formula" (or one that can be
#'   coerced to that class): a symbolic description of the
#'   model to be fitted.
#'
#' @param data a data frame, (or object coercible
#'   by as.data.frame to a data frame) containing the variables
#'   in the model.
#'
#' @param ncomp an integer: the number of components to keep in the model.
#'
#'
#' @returns an object of class PLSDA with the fitted model and it Attributes.
#'
#'
fit <- function(formula, data, ncomp = 2){

  #Get y and X names of columns
  yname <- toString(formula[[2]])
  Xnames <- attributes(terms(formula, data=data))$term.labels

  #Get y and X data
  y <- data[, yname]
  X <- data[, Xnames]

  #Descripteur d'observation
  n <- nrow(X)
  p <- ncol(X)
  q <- nlevels(y)

  #Transform y to dummies
  y <- get_dummies(y)

  #Get mean, sd and center scale data
  x.cs <- center_scale(X)
  Xk <- as.matrix(x.cs$Xk)
  X_mean <- x.cs$mean
  X_sd <- x.cs$sd

  y.cs <- center_scale(y)
  yk <- as.matrix(y.cs$Xk)
  y_mean <- y.cs$mean
  y_sd <- y.cs$sd

  #Initialisation
  U <- matrix(0, p, ncomp)#Weights of X
  V <- matrix(0, q, ncomp)#Weights of y
  Xi <- matrix(0, n, ncomp)#Scores of X
  Om <- matrix(0, n, ncomp)#Scores of y
  Ga <- matrix(0, p, ncomp)#Loadings of X
  De <- matrix(0, q, ncomp)#Loadings of y

  Sy <- matrix(y[,1])

  for(k in 1:ncomp){

    Wx_old <- 100

    for(i in 1:500){

      n_iter <- i

      Wx <- t(Xk) %*% Sy / sum(Sy^2)
      Wx <- Wx / sqrt(sum(Wx^2))
      Sx <- Xk %*% Wx
      Wy <- t(yk) %*% Sx / sum(Sx^2)
      Sy <- yk %*% Wy / sum(Wy^2)
      Wx_diff <- Wx - Wx_old

      if(sum(Wx_diff^2) < 1e-10 | q == 1){break}
      Wx_old <- Wx
    }

    if(n_iter == 500){print("Max number of iteration reached")}

    Sx <- Xk %*% Wx
    Sy <- yk %*% Wy / sum(Wy^2)
    Lx <- t(Xk) %*% Sx / sum(Sx^2)
    Xk <- Xk - Sx %*% t(Lx)
    Ly <- t(yk) %*% Sx / sum(Sx^2)
    yk <- yk - Sx %*% t(Ly)

    #Save values for conponent k
    U[,k] <- Wx
    V[,k] <- Wy
    Xi[,k] <- Sx
    Om[,k] <- Sy
    Ga[,k] <- Lx
    De[,k] <- Ly
  }

  A <- (t(Ga) %*% U)
  RotatX <- U %*% (solve(t(A) %*% A) %*% t(A))

  coef <- RotatX %*% t(De)
  coef <- coef * y_sd
  intercept <- y_mean

  #class S3 PLSA
  instance <- list("X" = X,
                   "y" = y,
                   "coef" = coef,
                   "intercept" = intercept,
                   "ScoresX" = Xi,
                   "ScoresY" = Om,
                   "WeightsX" = U,
                   "WeightsY" = V,
                   "LoadingsX" = Ga,
                   "LoadingsY" = De,
                   "N_iter" = n_iter
                   )
  class(instance) <- "PLSDA"

  return(instance)
}

