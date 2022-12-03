# PLSISE Project

Our project is part of the R programming course taught in the Master Data Science of the University Lumière Lyon 2. The main objective of this project is to set up the creation of a package under the R Studio software by integrating the PLS-DA method (Partial Least Square Discriminant Analysis Regression). The final goal is to be able to install our package directly from Github. 

## PLS-DA
The regression method of Partial Least Squares Discriminant Analysis, or supervised classification, is a technique generally used in chemometrics to optimize the separation between different groups of samples. It consists of a first data matrix X corresponding to the different characteristics (independant variables) and the second matrix Y which corresponds to the group, to the membership of a class (dependant variable). This method is in fact an extension of the PLS1 method which treats a single continuous dependent variable whereas the PLS2 method, also called PLS-DA can treat several categorical dependent variables. 

It is a method to maximize the covariance between the independent variables X and the dependent variable Y of highly multidimensional data by finding a linear subspace of the explanatory variables. The new subspace allows the prediction of the variable Y based on a reduced number of factors corresponding to the PLS components. Thus, these factors covering the subspace on which the independent variables are projected describe the behavior of the dependent variables Y.

## Package 
The installation of the package is required and is done directly through the Github access. In order to do this ...

Once the installation is successful, you can now access all the features of the package, especially the functions you see below.
- fit
- predict
- classification report
- variables selection
- graphics
- R Shiny Application

For the following, we used the iris dataset available directly on R studio for the presentation and the tests of our various functions.

## Fit function
The fit function corresponds to our learning function which returns an object of type "PLSDA" as output. The three main parameters of the function fit :
- formula : it is an object that defines the problem to solve
- data : corresponds to the data frame to process
- ncomp : number of components to be retained 
```
PLSDA <- fit(formula = Species~., data = iris, ncomp = 2)
```
We also overloaded two methods to get a display adapted to our objects returned by fit. The first one is the print function which provides a ranking function to assign classes to individuals. The second overloaded function is the summary function.
```
print(PLSDA)
summary(PLSDA)
```
Outputs :

![print](https://github.com/h-titouan/PLSISE/blob/main/img/print.png)

## Predict function
The predict function is a feature of our package allowing the prediction of the class on a new data set. This function takes 3 parameters as input:
- PLSDA : It corresponds to the PLSDA object provided by the fit function
- newdata : New data set to predict class membership.
- type : The type of output desired by the function. By default it is set to "class" to get the membership of the predicted class. It is also possible to set it to "posterior" to obtain the probabilities of class membership.
```
ypred <- predict(PLSDA = PLSDA, newdata = PLSDA$X, type = "class")
```
Output :

![predict](https://github.com/h-titouan/PLSISE/blob/main/img/predict.png)

## Classification report function
This function takes two parameters as input :
- y : Contains the class of individuals selected directly in our dataset
- ypred : predicted data provided by the predict function
```
classification_report(y = iris$Species, ypred = ypred)
```
Outputs :

![classreport](https://github.com/h-titouan/PLSISE/blob/main/img/classreport.png)

##  Variables selection function
The variable selection function allows to keep only the variables that are likely to be relevant for the predictive model. The method used for our selection is based on the principle of forward methods, i.e. starting from an empty set and inserting the variables as we go along, using a Fisher statistical test and checking the threshold value for the significance or not of the variable.

The three input parameters of the function :
- DF : set of explanatory variables
- cible : target vector
- alpha : Threshold defining the contribution of the variables (significant or not)
```
selection <- select_variable(DF = iris[1:4], cible = iris$Species, alpha = 0.03)
```
Output :

![selectvar](https://github.com/h-titouan/PLSISE/blob/main/img/selectvar.png)

## Graphics
We have integrated graphics within our project to provide the user with a visual aspect and more clarity in our function outputs and results.
There are 3 main functions:
1. scree_plot

this function allows us to display the scree plot of PLSDA corresponding to the object X. It takes only one input parameter, the object of class PLSDA.
```
scree_plot(PLSDA)
```
Output :

![scree](https://github.com/h-titouan/PLSISE/blob/main/img/scree.png)

2. pls_individuals

this function allows us to display the individuals on the factorial plane by taking as input of the function a main parameter, the PLSDA class object and lets the user choose to redefine the 2 axes of the plane initially defined at "1" for Axis_1 and at "2" for Axis_2.
```
pls_individuals(PLSDA)
```
Output :

![individuals](https://github.com/h-titouan/PLSISE/blob/main/img/individuals.png)

3. pls_variables

This function also allows you to display the variables on the factorial plane in the form of a circle to visualize the correlation between them. It takes the same input parameters as the previous function pls_individuals.
```
pls_variables(PLSDA)
```
Output :

![variables](https://github.com/h-titouan/PLSISE/blob/main/img/variables.png)


## R Shiny application
Once the application is launched, we arrive directly on the "Add files" home page where we have to select our data set. The integration of the file on the application is customized. You can choose to import it with or without headers by choosing the column separator. You can also choose to display only the first rows of the dataset or the whole dataset.

![Accueil](https://github.com/h-titouan/PLSISE/blob/main/img/Accueil.png)

You can see above the first lines of the iris set and on the right side of the page the descriptive statistics for each variable. We also note the information such as the length of the dataset and the type of the different modalities. 

## Contributors

Matthieu Allier  
Léo Haton  
Titouan Houde 



