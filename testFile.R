library(devtools)
install_github("h-titouan/PLSISE", force = T)
library(PLSISE)

#Fit the PLSDA model on iris dataset
iris = iris
PLSDA <- fit(formula = Species~., data = iris, ncomp = 2)

#Object in PLSDA fitted model
names(PLSDA)

#Fit report function
print(PLSDA)
summary(PLSDA)

#Predict class of iris based on PLSDA model
ypred <- predict(PLSDA, PLSDA$X)

#Classification report of Y by Ypred from iris
classification_report(iris$Species, ypred)

#Cross validation of the model on iris dataset
cross_validation(Species~., data = iris, method = "kFold") 

