library(devtools)
install_github("h-titouan/PLSISE", force = T)
library(PLSISE)

PLSDA <- fit(formula = Species~., data = iris, ncomp = 2)
predict(PLSDA, PLSDA$X)

