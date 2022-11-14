listOfPackages <- c(
  "fastDummies",
  "pracma"
  )

for (i in listOfPackages){
  if(! i %in% installed.packages()){install.packages(i, dependencies = TRUE)}
  require(i)
}
