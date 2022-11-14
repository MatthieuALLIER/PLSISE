source("libraries.R")
source("functions.R")

pls <- fit(formula = Species~., data = iris, ncomp = 2)

pls$coef
