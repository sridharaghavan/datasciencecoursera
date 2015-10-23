pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    directory <- paste("D:/Sridhar/Coursera/Datasciencecoursera/", directory, "/", sep="")

    # list all files in the specdata folder
    filesInDirectory <- as.character( list.files(directory) )
    filePath <- paste(directory, filesInDirectory, sep="")

    # initialize a vector to hold the pollutant data
    resultVector <- c()
        
    for(i in id) {
        currentFile <- read.csv(filePath[i], header=T, sep=",")
        naCleansed <- currentFile[!is.na(currentFile[, pollutant]), pollutant]
        resultVector <- c(resultVector, naCleansed)
    }
    return(round(mean(resultVector), 3))
}