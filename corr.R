corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  x <- sprintf("~/%s/%03d.csv",directory,1:332)
  corl <- c()
    for (i in x) {
    y <- read.csv(i)
    z <- na.omit(y)
    if (nrow(z)>threshold)
      {
      corl <- append(corl,cor(z$sulfate,z$nitrate))
    }  
    }
  return(corl)
}