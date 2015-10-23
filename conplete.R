complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  directory <- paste("D:/Sridhar/Coursera/Datasciencecoursera/", directory, "/", sep="")
  
  # list all files in the specdata folder
  filesInDirectory <- as.character( list.files(directory) )
  filePath <- paste(directory, filesInDirectory, sep="")
  
  # initialise vectors to hold data
  idVector <- c()
  nobsVector <- c()
  
  for(i in id) {
    currentFile <- read.csv(filePath[i], header=T, sep=",")
    idVector <- c(idVector, i)
    nobsVector <- c(nobsVector, sum(complete.cases(read.csv(currentFile))))
  }
  
  return (data.frame(id=idVector, nobs=nobsVector, stringsAsFactors = FALSE))
}