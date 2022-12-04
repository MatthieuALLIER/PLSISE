#' selection variables from PLSISE
#'
#' @description
#' Predict class, probabilities or values of X data based on a fitted PLS-DA model
#'
#' @param DF A data frame in which to look for variables with
#'   which to predict.

#' @param cible vector Y representing the different modalities for the membership classes
#'
#' @param alpha significance level for the variables to be included in the selection
#'
#' @returns lst_Var_Selected variables selected by the algorithm
#'
#' @export
#'
select_variable <- function(DF,cible,alpha = 0.05){
  
  # Checking the parameters
  if (!is.data.frame(DF)){stop("Error : Format Dataframe exigé")}
  nbNum <- sum(sapply(DF,is.numeric))
  if (nbNum < ncol(DF)){stop("Error : Les colonnes explicatives ne sont pas toutes numériques")}
  if (!is.factor(cible)) {cible <- factor(cible)}
  if (nrow(DF) != length(cible)){stop("Différences d'observations entre DF et Y")}
  
  # Preparatory calculations
  n = nrow(DF) # Number of observations
  p = ncol(DF) # Number of explanatory variables
  levels = levels(cible) # Displays the different modalities of the target variable
  nlevels = nlevels(cible) # Number of terms in the target variable
  
  # Matrix of conditional variances covariances
  V_cov_cond = lapply(levels,function(t){m <- as.matrix(DF[cible==t,]);(nrow(m)-1)*cov(m)})
  
  # Intra-class covariance matrix
  W = Reduce("+",V_cov_cond)/(n) 
  
  # Matrix of total variances covariances
  V = (n-1)/n*cov(DF) 

  # Initialization of parameters for searching and adding variables
  lst_Var_Selected = c() # Selected variables (empty at start)
  TotVar = colnames(DF) # Set of explanatory variables
  i = 0 # Number of variables selected in the current step
  lambda = 1.0  # Start value (0 variables selected at start)

  while (TRUE){
    
    if (length(TotVar) == 0){ # Case where no variable to select
      break  # Exit the loop and end the variable selection process
      }
    
    ResultMatrix = matrix(0,nrow=length(TotVar),ncol=3) # Results Matrix 
    rownames(ResultMatrix) = TotVar # Name of the variables in line
    colnames(ResultMatrix) = c("Lambda","Fisher","p-value") # Attribution of the 3 results on each variable

    for (variable in TotVar){ # For each explanatory variable
      lst_temp = c(lst_Var_Selected,variable) # Add the variable to the list
      W_temp = as.matrix(W[lst_temp,lst_temp]) # Computation of the matrix W according to the current list of variables
      V_temp = as.matrix(V[lst_temp,lst_temp]) # Calculation of the matrix V according to the current list of variables

      lambda_temp = det(W_temp)/det(V_temp) # Lambda calculation
      Fisher = (n-nlevels-i)/(nlevels-1)*(lambda/lambda_temp-1) # Test statistics
      pvalue = pf(Fisher,nlevels-1,n-nlevels-i,lower.tail=FALSE) # Calculation of the p-value
      ResultMatrix[variable,] = c(lambda_temp,Fisher,pvalue) # We add the results on the line corresponding to the variable 
    }
    
    if (nrow(ResultMatrix) > 1){ 
      ResultMatrix = ResultMatrix[order(ResultMatrix[,"Fisher"],decreasing=TRUE),]} 
      # The results of the matrix are sorted in descending order on the Fisher statistic
    
    if (alpha > ResultMatrix[1,"p-value"]){ # The best variable is recovered by checking the p-value for the significance of the variable
      best = rownames(ResultMatrix)[1] # Name of the selected variable
      lst_Var_Selected = c(lst_Var_Selected,best) # Add the variable to the list of selected variables
      TotVar = TotVar[TotVar != best]
      i = i + 1
      lambda = ResultMatrix[1,"Lambda"]
    } else {
      break # No corresponding variable.
    }
  }
 return(lst_Var_Selected) # Selected output variables
}
