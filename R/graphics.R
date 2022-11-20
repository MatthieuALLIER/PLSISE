scree_plot <- function(pls_fit = pls){
    
  # Dependencies
  library(plotly)
    
  # Get the number of component choosed in the pls fit
  n_comp = pls$N_comp
    
  # Center and scale the data
  center = center_scale(pls$X[,c(1:n_comp)])
  # Calculate the eigen
  eig = eigen(cor(center$Xk))
  eigen = eig$values / sum(eig$values)
  
  # Barplot with plotly
  scree <- plot_ly(
    x = pls$Comps,
    y = eigen,
    type = "bar",
    name = "Eigen of each Comps"
    ) %>%
    # Add the scatter for the elbow method
    add_trace(x = ~pls$Comps, y = ~eigen, 
              type = 'scatter', mode = 'lines', name = "Inertia between each Comps")
    
  # Add plot title and Delete xaxis and yaxis title
  scree <- scree %>% layout(title = "PLS Regression Screeplot",
                              xaxis = list(title = ""),
                              yaxis = list(title = ""))
  return(scree)
}
  
scree_plot()
  
facto_axis <- function(Axis_1 = 1, Axis_2 = 2){
    
  # Get the number of component choosed in the pls fit
  n_comp = pls$N_comp
    
  if(Axis_1 > n_comp | Axis_2 > n_comp){
      
    print("Error : Axis value is higher than the numbers of components")
  } else {
      
    library(plotly)
    
    Species = rep(NA, 150)
    # Create species columns depending on predict
    for(c in 1:length(pls$ynames)){
      for(l in 1:nrow(pls$y)){
        ifelse(pls$y[l,c] == 1, Species[l] <- pls$ynames[c], Species[l] <- Species[l])
        }
    }
      
    indiv <- plot_ly(x = pls$ScoresX[,Axis_1], y = pls$ScoresX[,Axis_2], color = ~Species)
    
    return(indiv)
    }
  }
  
facto_axis()
  

variables <- function(Axis_1 = 1, Axis_2 = 2){
  
  library(plotly)
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
      text = pls$Xnames
    )
    
    # Uncomment below if we want to have the point + text
    # corr_circle <- corr_circle %>% add_markers()
    corr_circle <- corr_circle %>% add_text(textfont = t, textposition = "top right")
    
    # Layout properties
    corr_circle <- corr_circle %>% layout(
      title = "Correlation circle",
      width = 500,
      height = 500,
      xaxis = list(title = paste("Component", Axis_1, sep = " ")),
      yaxis = list(title = paste("Component", Axis_2, sep = " ")),
      shapes = list(
        # problème avec la taille du cercle - A Modifier
        list(
          x0 = -1.1, 
          x1 = 1.1, 
          y0 = -1.1, 
          y1 = 1.1, 
          type = "circle"
        ))
      )
    
    return(corr_circle)
                   
  }
}

variables()