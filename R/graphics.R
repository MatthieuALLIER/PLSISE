#' Scree plot from PLSISE
#'
#' @description Show scree plot of PLSDA : X object
#'
#' @param PLSDA an object of class PLSDA : a fitted with PLS-DA
#'   method (fit) model.
#'
#' @param method The criterion for component selection, "kaiser" or 
#'  "broken_sticks"
#'
#' @returns The scree plot
#'
#' @export
#'
scree_plot <- function(PLSDA, method = "kaiser"){
  
  if(method != "kaiser" & method != "broken_sticks"){
    return("Unknown method, check out for one known")
    break
  }
  
  # Get the number of component choosed in the pls fit
  n_comp = PLSDA$N_comp
  
  # Center and scale the data
  center = center_scale(PLSDA$X[,c(1:n_comp)])
  # Calculate the eigen
  eig = eigen(cor(center$Xk))
  eigen = eig$values
  
  ### Methods ###
  # Kaiser 
  toSelect_kaiser = max(which(eigen > 1))
  
  # Broken Sticks 
  toCalculate = rep(1,n_comp)
  for(i in 1:n_comp){
    toCalculate[i] = toCalculate[i] / i
  }
  broken_sticks <- data.frame(eigen, toCalculate)
  for(i in 1:nrow(broken_sticks)){
    broken_sticks$toCalculate[i] = sum(toCalculate[c(i:nrow(broken_sticks))])
  }
  
  # Result Broken Sticks
  toSelect_BrokenSticks = max(which(broken_sticks$eigen > broken_sticks$toCalculate))
  
  # Result Kaiser
  ifelse(method == "kaiser", select <- toSelect_kaiser, select <- toSelect_BrokenSticks)
  
  ColorSelected = "rgba(222,45,38,1)"
  ColorNotSelected = "rgba(70,130,180,1)"
  
  CompSelected = c(rep(ColorSelected, select), rep(ColorNotSelected, n_comp-select))
  
  # Barplot with plotly
  scree <- plot_ly(
    x = PLSDA$Comps,
    y = eigen,
    type = "bar",
    name = "Eigen of each Comps",
    marker = list(color = CompSelected)
  ) %>%
    # Add the scatter for the elbow method
    add_trace(x = ~PLSDA$Comps, y = ~eigen, 
              type = 'scatter', mode = 'lines+markers', name = "Inertia between each Comps")
  
  # Add plot title and Delete xaxis and yaxis title
  scree <- scree %>% layout(title = "PLS Regression Screeplot",
                            xaxis = list(title = "",
                                    categoryorder = "array",
                                    categoryarray = PLSDA$Comps),
                            yaxis = list(title = ""))
  return(scree)
  
}

#' PLS individuals from PLSISE
#'
#' @description Show individuals on factorial plan
#'
#' @param PLSDA an object of class PLSDA : a fitted with PLS-DA
#'   method (fit) model.
#'
#' @param Axis_1 an integer : the number of the component on x axis
#'  
#' @param Axis_2 an integer : the number of the component on y axis
#'
#' @returns The individuals on factorial plan plot
#'
#' @export
#'
pls_individuals <- function(PLSDA, Axis_1 = 1, Axis_2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = PLSDA$N_comp
  
  if(Axis_1 > n_comp | Axis_2 > n_comp){
    
    print("Error : Axis value is higher than the numbers of components")
  } else {
    
    class = rep(NA, length(PLSDA$y))
    # Create class columns depending on PLS functions
    for(c in 1:length(PLSDA$ynames)){
      for(l in 1:nrow(PLSDA$y)){
        ifelse(PLSDA$y[l,c] == 1, class[l] <- PLSDA$ynames[c], class[l] <- class[l])
      }
    }
    
    indiv <- plot_ly(x = ~PLSDA$ScoresX[,Axis_1], y = ~PLSDA$ScoresX[,Axis_2], 
                     color = ~class, type = "scatter", mode = "markers")
    
    indiv <- indiv %>% layout(
      title = "Chart of individuals",
      xaxis = list(title = PLSDA$Comps[Axis_1]),
      yaxis = list(title = PLSDA$Comps[Axis_2])
    )
    
    return(indiv)
  }
}

#' PLS variables from PLSISE
#'
#' @description Show variables on factorial plan
#'
#' @param PLSDA an object of class PLSDA : a fitted with PLS-DA
#'   method (fit) model.
#'
#' @param Axis_1 an integer : the number of the component on x axis
#'  
#' @param Axis_2 an integer : the number of the component on y axis
#'
#' @returns The variables on factorial plan plot
#'
#' @export
#'
pls_variables <- function(PLSDA, Axis_1 = 1, Axis_2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = PLSDA$N_comp
  
  if(Axis_1 > n_comp  | Axis_2 > n_comp){
    
    print("Error : Axis value is higher than the numbers of components")
    
  } else {
    
    # Calculate the eigen
    eig = eigen(cor(PLSDA$X))
    sdev = sqrt(eig$values)
    
    # Calculate the coordinate of the axis
    X = PLSDA$LoadingsX[,Axis_1]*sdev[Axis_1]
    Y = PLSDA$LoadingsX[,Axis_2]*sdev[Axis_2]
    
    # Text proprieties
    t <- list(
      family = "sans serif",
      size = 14,
      color = toRGB("red"))
    
    # 
    corr_circle <- plot_ly(
      x = X,
      y = Y,
      width = 500,
      height = 500,
      text = PLSDA$Xnames
    )
    
    # Uncomment below if we want to have the point + text
    # corr_circle <- corr_circle %>% add_markers()
    corr_circle <- corr_circle %>% add_text(textfont = t, textposition = "top right")
    
    # Layout properties
    corr_circle <- corr_circle %>% layout(
      title = "Correlation circle",
      xaxis = list(title = paste("Component", Axis_1, sep = " ")),
      yaxis = list(title = paste("Component", Axis_2, sep = " ")),
      shapes = list(
        # problème avec la taille du cercle - A Modifier
        list(
          x0 = -1, 
          x1 = 1, 
          y0 = -1, 
          y1 = 1, 
          type = "circle"
        ))
    )
    
    return(corr_circle)
    
  }
}

#' X plot from PLSISE
#'
#' @description Show individuals by variables of a dataset
#'
#' @param data a data.frame object
#'
#' @param varX Number or name of variable to represent on x axis
#'  
#' @param varY Number or name of variable to represent on y axis
#' 
#' @param class A factor : class of individuals
#'
#' @returns The plot of individuals on variables plan
#'
#' @export
#'
x_plot <- function(data , varX, varY, class){
  
  varXnames = names(data[varX])
  varYnames = names(data[varY])
  
  scatter <- plot_ly(x = as.numeric(unlist(data[varX])), 
                     y = as.numeric(unlist(data[varY])), 
                     color = ~as.factor(unlist(data[class])),
                     type = "scatter", mode =  "markers")
  
  scatter <- scatter %>% layout(
    title = "Explanatory variable",
    xaxis = list(title = varXnames),
    yaxis = list(title = varYnames)
  )
  
  return(scatter)
  
}