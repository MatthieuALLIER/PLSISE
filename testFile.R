library(devtools)
install_github("h-titouan/PLSISE")
library(PLSISE)

pls <- fit(formula = Species~., data = iris, ncomp = 2)

pls$ScoresX
