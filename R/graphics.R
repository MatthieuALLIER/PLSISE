scree_plot <- function(pls, method = "kaiser"){
  
  if(method != "kaiser" & method != "broken_sticks"){
    return("Unknown method, check out for one known")
    break
  }
  
  # Get the number of component choosed in the pls fit
  n_comp = pls$N_comp
  
  # Center and scale the data
  center = center_scale(pls$X[,c(1:n_comp)])
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
    x = pls$Comps,
    y = eigen,
    type = "bar",
    name = "Eigen of each Comps",
    marker = list(color = CompSelected)
  ) %>%
    # Add the scatter for the elbow method
    add_trace(x = ~pls$Comps, y = ~eigen, 
              type = 'scatter', mode = 'lines+markers', name = "Inertia between each Comps")
  
  # Add plot title and Delete xaxis and yaxis title
  scree <- scree %>% layout(title = "PLS Regression Screeplot",
                            xaxis = list(title = "",
                                    categoryorder = "array",
                                    categoryarray = pls$Comps),
                            yaxis = list(title = ""))
  return(scree)
  
}

facto_axis <- function(pls, Axis_1 = 1, Axis_2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = pls$N_comp
  
  if(Axis_1 > n_comp | Axis_2 > n_comp){
    
    print("Error : Axis value is higher than the numbers of components")
  } else {
    
    Species = rep(NA, 150)
    # Create species columns depending on PLS functions
    for(c in 1:length(pls$ynames)){
      for(l in 1:nrow(pls$y)){
        ifelse(pls$y[l,c] == 1, Species[l] <- pls$ynames[c], Species[l] <- Species[l])
      }
    }
    
    indiv <- plot_ly(x = ~pls$ScoresX[,Axis_1], y = ~pls$ScoresX[,Axis_2], 
                     color = ~Species, type = "scatter", mode = "markers")
    
    indiv <- indiv %>% layout(
      title = "Chart of individuals",
      xaxis = list(title = pls$Comps[Axis_1]),
      yaxis = list(title = pls$Comps[Axis_2])
    )
    
    return(indiv)
  }
}

variables <- function(pls, Axis_1 = 1, Axis_2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = pls$N_comp
  
  if(Axis_1 > n_comp  | Axis_2 > n_comp){
    
    print("Error : Axis value is higher than the numbers of components")
    
  } else {
    
    # Calculate the eigen
    eig = eigen(cor(pls$X))
    sdev = sqrt(eig$values)
    
    # Calculate the coordinate of the axis
    X = pls$LoadingsX[,Axis_1]*sdev[Axis_1]
    Y = pls$LoadingsX[,Axis_2]*sdev[Axis_2]
    
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
      text = pls$Xnames
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

explicatives <- function(data , varX, varY, class){
  
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
