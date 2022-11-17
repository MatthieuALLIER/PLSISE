library(devtools)
install_github("h-titouan/PLSISE", force = T)
library(PLSISE)

pls <- fit(formula = Species~., data = iris, ncomp = 3)

pls$ScoresX
