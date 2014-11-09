pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  x <- sprintf("~/%s/%03d.csv",directory,id)
  poldata <- c()
  for (i in x) { 
    y <- read.csv(i)
    poldata <- append(poldata, y[, pollutant])
  }  
  z <- sprintf("%.3f",mean(poldata,na.rm=TRUE))
  return(z)
}