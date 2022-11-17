library(devtools)
install_github("h-titouan/PLSISE", force = T)
library(PLSISE)

pls <- fit(formula = Caca~., data = iris, ncomp = 2)

pls$ScoresX
