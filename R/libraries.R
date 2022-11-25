listOfPackages <- c(
  "plotly",
  "shiny",
  "shinythemes",
  "shinyjs"
)

skip <- ifelse(length(listOfPackages) == 0, T, F)

if(skip){
  print("No packages required!")
}else{
  for(i in seq.int(length(listOfPackages))){
    if(!(listOfPackages[i] %in% installed.packages())){
      install.packages(listOfPackages[i])
    }
    library(listOfPackages[i], character.only = T)
  }
}
