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
  ## number of complete cases. run by: source("complete.R")
  x <- sprintf("~/%s/%03d.csv",directory,id)
  nobs <- c() 
  for (i in x) { 
    y <- read.csv(i)
    z <- na.omit(y)
    nobs <- append(nobs,nrow(z))
  }
  #nobs check
  q <- data.frame(id , nobs)
  return(q)
}