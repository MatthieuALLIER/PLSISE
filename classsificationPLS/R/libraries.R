listOfPackages <- c(
  #here packages between "" and separated by comma
)

if(length(listOfPackages) == 0){skip=T}

get.packages <- function(packages, skip = skip){
  if(skip){
    print("No packages required!")
  }
  else{
    for(i in seq.int(length(packages))){
      if(!require(packages[i], character.only = T)){
        install.packages(packages[i])
      }
      library(packages[i], character.only = T)
    }
  }
}

get.packages(listOfPackages, T)
