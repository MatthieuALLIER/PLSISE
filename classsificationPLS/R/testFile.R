source("libraries.R")
source("center_scale.R")
source("fit.R")

pls <- fit(formula = Species~., data = iris, ncomp = 2)

pls$coef
