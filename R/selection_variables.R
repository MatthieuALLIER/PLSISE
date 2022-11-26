# SELECTION DE VARIABLE

# ENTREES DE LA FONCTION
# DF : ensemble des variables explicatives
# cible : vecteur cible
# alpha : Seuil définissant la contribution des variables (significative ou non)

select_variable <- function(DF,cible,alpha = 0.05){
  
  # vérif des paramètres
  if (!is.data.frame(DF)){stop("Error : Format Dataframe exigé")}
  nbNum <- sum(sapply(DF,is.numeric))
  if (nbNum < ncol(DF)){stop("Error : Les colonnes explicatives ne sont pas toutes numériques")}
  if (!is.factor(cible)) {cible <- factor(cible)}
  if (nrow(DF) != length(cible)){stop("Différences d'observations entre DF et Y")}
  
  # Calculs préparatoires
  n = nrow(DF) # Nombre d'observations
  p = ncol(DF) # Nombre de variable explicatives
  levels = levels(cible) # Affiche les différentes modalités de la variable cible
  nlevels = nlevels(cible) # Nombre de modalités dans la variable cible
  
  # Matrice des variances covariances conditionnelles
  V_cov_cond = lapply(levels,function(t){m <- as.matrix(DF[cible==t,]);(nrow(m)-1)*cov(m)})
  
  # Matrice des covariance intra-classe
  W = Reduce("+",V_cov_cond)/(n) 
  
  # Matrice des variances covariances totales
  V = (n-1)/n*cov(DF) 

  # Initialisation des paramètres pour la recherche et l'ajout des variables
  lst_Var_Selected = c() # Variables sélectionnées (vide au départ)
  TotVar = colnames(DF) # Ensemble des variables explicatives
  i = 0 # Nombre de variables sélectionnées à l'étape en cours
  lambda = 1.0  # Valeur de départ (0 variables sélectionnées au départ)

  while (TRUE){
    
    if (length(TotVar) == 0){ # Cas où pas de variable à sélectionner
      break  # On sort de la boucle et fin du processus de sélection de variable
      }
    
    ResultMatrix = matrix(0,nrow=length(TotVar),ncol=3) # Matrice des résultats 
    rownames(ResultMatrix) = TotVar # Nom des variables en ligne
    colnames(ResultMatrix) = c("Lambda","Fisher","p-value") # Attribution des 3 résultats sur chaque variable

    for (variable in TotVar){ # Pour chaque variable explicative
      lst_temp = c(lst_Var_Selected,variable) # Ajout de la variable dans la liste
      W_temp = as.matrix(W[lst_temp,lst_temp]) # Calcul de la matrice W en fonction de la liste courante des variables
      V_temp = as.matrix(V[lst_temp,lst_temp]) # Calcul de la matrice V en fonction de la liste courante des variables

      lambda_temp = det(W_temp)/det(V_temp) # Calcul du lambda
      Fisher = (n-nlevels-i)/(nlevels-1)*(lambda/lambda_temp-1) # Statistique de test
      pvalue = pf(Fisher,nlevels-1,n-nlevels-i,lower.tail=FALSE) # Calcul de la p-value
      ResultMatrix[variable,] = c(lambda_temp,Fisher,pvalue) # On ajoute les résultats sur la ligne correspondante à la variable 
    }
    
    if (nrow(ResultMatrix) > 1){ 
      ResultMatrix = ResultMatrix[order(ResultMatrix[,"Fisher"],decreasing=TRUE),]} 
      # On trie les résultats de la matrice par ordre décroissant sur la statistique de Fisher
    
    if (alpha > ResultMatrix[1,"p-value"]){ # On récupère la meilleure variable en vérifiant la valeur de la p-value pour la significativité de la variable
      best = rownames(ResultMatrix)[1] # Nom de la variable sélectionnée
      lst_Var_Selected = c(lst_Var_Selected,best) # Ajout de la variable dans la liste des variables sélectionnées
      TotVar = TotVar[TotVar != best]
      i = i + 1
      lambda = ResultMatrix[1,"Lambda"]
    } else {
      break # Aucune variable correspondante.
    }
  }
 return(lst_Var_Selected) # Variables sélectionnées en sortie
}

# Test
data(iris)
str(iris)

#Appel de la fonction
test <- select_variable(DF=iris[1:4],cible=iris$Species,alpha=0.01)
print(test)
