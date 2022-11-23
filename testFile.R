library(devtools)
install_github("h-titouan/PLSISE", force = T)
library(PLSISE)

PLSDA <- fit(formula = Species~., data = data, ncomp = 2)
ypred <- predict(PLSDA, PLSDA$X)
classification_report(data$Species, ypred)
cross_validation(Species~., data = data, method = "kFold")

