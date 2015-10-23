corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  directory <- paste("D:/Sridhar/Coursera/Datasciencecoursera/", directory, "/", sep="")
  
  # list all files in the specdata folder
  filesInDirectory <- as.character( list.files(directory) )
  filePath <- paste(directory, filesInDirectory, sep="")
  
  # Initialize vector to hold correlation data
  corr <- c()
  
  for (i in 1:332) {
    currentFile <- read.csv(filePath[i], header=T, sep=",")
    
    naCleansed <- currentFile[complete.cases(currentFile), ]
    
    if (nrow(naCleansed) >= threshold) {
      cr <- cor(naCleansed["sulfate"], naCleansed["nitrate"])
      
      if (!is.na(cr)) {
        corr <- append(corr, cr)
      }
    }
  }
  
  corr
}