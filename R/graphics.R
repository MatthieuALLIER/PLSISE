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
              type = 'scatter', mode = 'lines', name = "Elbow method")
  
  # Add plot title and Delete xaxis and yaxis title
  scree <- scree %>% layout(title = "PLS Regression Screeplot",
                            xaxis = list(title = ""),
                            yaxis = list(title = ""))
  return(scree)
}

scree_plot()