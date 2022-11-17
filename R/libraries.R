listOfPackages <- c(
  #here packages between "" and separated by comma
)

skip <- ifelse(length(listOfPackages) == 0, T, F)

if(skip){
  print("No packages required!")
}else{
  for(i in seq.int(length(listOfPackages))){
    if(!require(listOfPackages[i], character.only = T)){
      install.packages(listOfPackages[i])
    }
    library(listOfPackages[i], character.only = T)
  }
}
