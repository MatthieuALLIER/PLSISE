#' Cross validation from PLSISE
#'
#' @description
#'  Run cross validation of the PLS-DA classifier on a dataset. Use the fit and
#'  predict function from PLSISE package. Compare PLSDA model by fscore
#'
#' @param formula an object of class "formula", symbolic description of model 
#'  to be fitted.
#'
#' @param data a data frame containing the variables in model.
#' 
#' @param ncomp an integer: the number of components to keep in model.
#' 
#' @param cv an integer: the number of folds for the cross validation method
#' 
#' @param method Method used to split the dataset in folds, use 
#'  "splitTrainTest" to create random train and test datasets at each validation,
#'  use "kFold" to split "data" in "cv" folds and use one of them as test at 
#'  each validation. 
#'
#' @returns MeanFscore : mean of fscore on validation,
#'  BestFscore : max fscore on validation,  
#'  BestModel : model with best fscore on validation
#'
#' @export
#'
cross_validation <- function(formula, data, ncomp, cv = 5, method = "splitTrainTest"){
  n <- nrow(data)
  data <- data[sample(1:n),]
  fold <- list()
  if(method == "splitTrainTest"){
    for(i in 1:cv){
      #Split dataset
      nT <- n*0.7
      nF <- n - nT
      ind <- sample(c(rep(TRUE,nT),rep(FALSE,nF)))
      fold[[i]] <- ind
    }
  }
  if(method == "kFold"){
    foldSize <- n / cv
    for(i in 1:cv){
      ind <- rep(TRUE, n)
      bb <- i * foldSize - foldSize + 1
      hb <- i * foldSize
      ind[bb:hb] <- FALSE
      fold[[i]] <- ind
    }
  }
  
  globalFscoreVector <- c()
  models <- list()
  for(k in 1:cv){
    #Get the cols of X
    Xnames <- attributes(terms(formula, data=data))$term.labels
    yname <- toString(formula[[2]])
    
    #Get fold
    ind <- fold[[k]]
    train <- data[ind,]
    test <- data[!ind,]
    
    #Keep only X cols on test
    Xtest <- data.frame(test[, Xnames])
    Ytest <- test[, yname]
    
    #Fit the model on train and predict on test
    plsTrain <- fit(formula, train)
    predTest <- predict(plsTrain, Xtest)
    
    classReport <- classification_report(Ytest, predTest)
    globalFscore <- classReport$GlobalFscore
    
    globalFscoreVector <- append(globalFscoreVector, globalFscore)
    models[[k]] <- plsTrain
  }
  model <- models[[which.max(globalFscoreVector)]]
  fscoreMean <- mean(globalFscoreVector, nar.rm=T)
  fscore <- globalFscoreVector[which.max(globalFscoreVector)]
  res <- list("MeanFscore" = fscoreMean, 
              "BestFscore" = fscore, 
              "BestModel" = model)
  return(res)
}
